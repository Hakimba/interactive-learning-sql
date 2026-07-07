// Découpage d'un texte SQL en CLAUSES avec leurs positions (spans) dans le texte.
// Fondation pour : l'aperçu par clause, le surlignage, et le mode itératif "à la Coq".
// Léger (pas un vrai parseur) : suffisant pour le fragment SELECT-FROM-WHERE mono-table.

export type ClauseKey = "select" | "from" | "where" | "orderby" | "limit" | "offset";

// Ordre d'ÉVALUATION (≠ ordre d'écriture) — pour le pas-à-pas.
const EVAL_RANK: Record<ClauseKey, number> = {
  from: 0, where: 1, select: 2, orderby: 3, limit: 4, offset: 5,
};

export interface ClausePart {
  key: ClauseKey;
  start: number; // début du mot-clé dans le texte
  end: number; // = début de la clause suivante (ou fin du texte)
}

export interface Segmentation {
  parts: ClausePart[]; // dans l'ordre d'écriture
  byKey: Partial<Record<ClauseKey, ClausePart>>;
  evalOrder: ClauseKey[]; // clauses présentes, dans l'ordre d'évaluation
  tableName: string | null;
  selectCols: string[] | "*" | null;
  hasSelect: boolean;
  hasFrom: boolean;
}

interface Tok {
  kind: "word" | "str" | "punct" | "op";
  value: string;
  start: number;
  end: number;
}

function lex(sql: string): Tok[] {
  const toks: Tok[] = [];
  let i = 0;
  const n = sql.length;
  while (i < n) {
    const c = sql[i];
    if (/\s/.test(c)) { i++; continue; }
    const start = i;
    if (c === "'") {
      i++;
      let s = "";
      while (i < n) {
        if (sql[i] === "'") { if (sql[i + 1] === "'") { s += "'"; i += 2; continue; } i++; break; }
        s += sql[i++];
      }
      toks.push({ kind: "str", value: s, start, end: i });
    } else if (/[A-Za-z_]/.test(c)) {
      while (i < n && /[A-Za-z0-9_]/.test(sql[i])) i++;
      toks.push({ kind: "word", value: sql.slice(start, i), start, end: i });
    } else if (/[0-9]/.test(c)) {
      while (i < n && /[0-9.]/.test(sql[i])) i++;
      toks.push({ kind: "op", value: sql.slice(start, i), start, end: i });
    } else if (c === "(" || c === ")" || c === ",") {
      i++;
      toks.push({ kind: "punct", value: c, start, end: i });
    } else {
      i++;
      toks.push({ kind: "op", value: c, start, end: i });
    }
  }
  return toks;
}

const STARTERS: Record<string, ClauseKey> = {
  select: "select", from: "from", where: "where", limit: "limit", offset: "offset",
};

export function segment(sql: string): Segmentation {
  const toks = lex(sql);
  // 1) repérer les frontières de clauses (mot-clé + position)
  const bounds: { key: ClauseKey; start: number }[] = [];
  for (let k = 0; k < toks.length; k++) {
    const t = toks[k];
    if (t.kind !== "word") continue;
    const w = t.value.toLowerCase();
    if (w === "order" && toks[k + 1]?.kind === "word" && toks[k + 1].value.toLowerCase() === "by") {
      bounds.push({ key: "orderby", start: t.start });
    } else if (STARTERS[w]) {
      bounds.push({ key: STARTERS[w], start: t.start });
    }
  }
  // 2) construire les parts (chaque clause va jusqu'au début de la suivante)
  const n = sql.length;
  const parts: ClausePart[] = bounds.map((b, i) => ({
    key: b.key, start: b.start, end: i + 1 < bounds.length ? bounds[i + 1].start : n,
  }));
  const byKey: Partial<Record<ClauseKey, ClausePart>> = {};
  for (const p of parts) if (!byKey[p.key]) byKey[p.key] = p;

  // 3) nom de table (premier mot après FROM)
  let tableName: string | null = null;
  const from = byKey.from;
  if (from) {
    const inFrom = toks.filter((t) => t.start >= from.start && t.start < from.end && t.kind === "word");
    // inFrom[0] == "from"
    for (let j = 1; j < inFrom.length; j++) {
      if (inFrom[j].value.toLowerCase() !== "as") { tableName = inFrom[j].value; break; }
    }
  }

  // 4) colonnes du SELECT (identifiants nus, ou "*")
  let selectCols: string[] | "*" | null = null;
  const sel = byKey.select;
  if (sel) {
    const inSel = toks.filter((t) => t.start >= sel.start && t.start < sel.end);
    const isStar = inSel.some((t) => t.kind === "op" && t.value === "*");
    const idents: string[] = [];
    for (let j = 0; j < inSel.length; j++) {
      const t = inSel[j];
      if (t.kind !== "word") continue;
      const w = t.value.toLowerCase();
      if (w === "select" || w === "distinct" || w === "as") continue;
      // ignore un nom de fonction (suivi de "(")
      if (inSel[j + 1]?.kind === "punct" && inSel[j + 1].value === "(") continue;
      idents.push(t.value);
    }
    selectCols = isStar && idents.length === 0 ? "*" : idents;
  }

  const evalOrder = parts
    .map((p) => p.key)
    .filter((k, i, a) => a.indexOf(k) === i)
    .sort((a, b) => EVAL_RANK[a] - EVAL_RANK[b]);

  return {
    parts, byKey, evalOrder, tableName, selectCols,
    hasSelect: !!byKey.select, hasFrom: !!byKey.from,
  };
}
