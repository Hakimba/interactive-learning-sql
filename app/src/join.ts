// Détection + calcul d'une jointure (une seule pour la visualisation : base ⋈ table).
// L'appariement et le cadrage sont calculés PAR LE MOTEUR (sonde __idx). La regex ne
// sert qu'à repérer les 2 tables + le ON ; c'est le moteur qui valide/évalue réellement.
import { segment } from "./clauses";
import { runQuery } from "./engine";
import type { Database, Val } from "./types";

export type JoinKind = "INNER" | "LEFT" | "RIGHT" | "FULL" | "CROSS";
export interface JoinInfo {
  base: { table: string; alias: string };
  join: { table: string; alias: string };
  kind: JoinKind;
  onText: string | null;
}

const KW = "(?:inner|left|right|full|cross|outer|join)";
const IDX = "__idx";
const intFrom = (t: string, kw: string): number | null => {
  const m = t.match(new RegExp(kw + "\\s+(\\d+)", "i"));
  return m ? parseInt(m[1], 10) : null;
};

export function detectJoin(sql: string): JoinInfo | null {
  const seg = segment(sql);
  const fp = seg.byKey.from;
  if (!fp) return null;
  const t = sql.slice(fp.start, fp.end).trim().replace(/^from\s+/i, "");
  const comma = t.match(/^(\w+)(?:\s+(?:as\s+)?(\w+))?\s*,\s*(\w+)(?:\s+(?:as\s+)?(\w+))?\s*$/i);
  if (comma)
    return { base: { table: comma[1], alias: comma[2] || comma[1] }, join: { table: comma[3], alias: comma[4] || comma[3] }, kind: "CROSS", onText: null };
  const re = new RegExp(
    `^(\\w+)(?:\\s+(?:as\\s+)?(?!${KW}\\b)(\\w+))?\\s+(inner|left|right|full|cross)?\\s*(?:outer\\s+)?join\\s+(\\w+)(?:\\s+(?:as\\s+)?(?!on\\b)(\\w+))?(?:\\s+on\\s+([\\s\\S]+))?$`,
    "i"
  );
  const m = t.match(re);
  if (!m) return null;
  return {
    base: { table: m[1], alias: m[2] || m[1] },
    join: { table: m[4], alias: m[5] || m[4] },
    kind: (m[3] || "INNER").toUpperCase() as JoinKind,
    onText: m[6] ? m[6].trim() : null,
  };
}

export interface JoinData {
  ok: boolean;
  note?: string;
  info: JoinInfo;
  leftName: string; leftCols: string[]; leftRows: Val[][];
  rightName: string; rightCols: string[]; rightRows: Val[][];
  pairs: [number | null, number | null][];
  // relation jointe (SELECT *), + cadrages sur SES lignes (chunk stepper)
  combinedCols: string[]; combinedRows: Val[][];
  passSet: Set<number> | null;      // lignes jointes satisfaisant le WHERE
  orderedIdx: number[] | null;      // ordre ORDER BY sur les lignes jointes
  keptSet: Set<number>;             // WHERE+ORDER+LIMIT (vrai résultat)
  limitN: number | null; offset: number;
  hasWhere: boolean; hasOrder: boolean; hasLimit: boolean;
  selectNames: string[] | "*" | null;
  collageCols: string[]; collageRows: Val[][]; // projection réelle finale (avec ORDER/LIMIT)
  selectRows: Val[][];  // projection après WHERE, SANS ORDER/LIMIT (étape SELECT)
  orderRows: Val[][];   // projection après WHERE+ORDER, SANS LIMIT (étape ORDER BY)
}

export function computeJoin(sql: string, db: Database): JoinData | null {
  const info = detectJoin(sql);
  if (!info) return null;
  const seg = segment(sql);
  const bt = db.tables.find((t) => t.name.toLowerCase() === info.base.table.toLowerCase());
  const jt = db.tables.find((t) => t.name.toLowerCase() === info.join.table.toLowerCase());
  const empty = {
    ok: false as const, info,
    leftName: info.base.table, leftCols: [], leftRows: [], rightName: info.join.table, rightCols: [], rightRows: [],
    pairs: [], combinedCols: [], combinedRows: [], passSet: null, orderedIdx: null, keptSet: new Set<number>(),
    limitN: null, offset: 0, hasWhere: false, hasOrder: false, hasLimit: false, selectNames: null,
    collageCols: [], collageRows: [], selectRows: [], orderRows: [],
  };
  if (!bt || !jt) return { ...empty, note: "table de jointure inconnue" };

  const mk = (t: (typeof db.tables)[number]) => ({ cols: t.columns.map((c) => c.name), rows: t.rows.map((r) => t.columns.map((c) => r[c.name] ?? null)) });
  const L = mk(bt), R = mk(jt);

  const withIdx = (t: (typeof db.tables)[number]) => ({ name: t.name, columns: [...t.columns, { name: IDX, type: "INTEGER" }], rows: t.rows.map((r, i) => ({ ...r, [IDX]: i })) });
  const probeDb: Database = { tables: db.tables.map((t) => (t === bt || t === jt ? withIdx(t) : t)) };

  const bA = info.base.alias, jA = info.join.alias;
  const fromClause =
    info.kind === "CROSS" || !info.onText
      ? `${bt.name} ${bA} CROSS JOIN ${jt.name} ${jA}`
      : `${bt.name} ${bA} ${info.kind} JOIN ${jt.name} ${jA} ON ${info.onText}`;

  const idxPairs = (extra: string): [number | null, number | null][] => {
    const r = runQuery(`SELECT ${bA}.${IDX}, ${jA}.${IDX} FROM ${fromClause} ${extra}`, probeDb);
    return r.ok ? r.rows.map((row) => [row[0] as number | null, row[1] as number | null]) : [];
  };
  const pairs = idxPairs("");
  const key = (p: [number | null, number | null]) => `${p[0]}|${p[1]}`;
  const keyToIndex = new Map<string, number>();
  pairs.forEach((p, i) => keyToIndex.set(key(p), i));

  // WHERE sur les lignes jointes
  const wp = seg.byKey.where;
  let passSet: Set<number> | null = null;
  if (wp) {
    const ks = new Set(idxPairs(sql.slice(wp.start, wp.end)).map(key));
    passSet = new Set(pairs.map((_, i) => i).filter((i) => ks.has(key(pairs[i]))));
  }
  // ORDER BY sur les lignes jointes
  const op = seg.byKey.orderby;
  let orderedIdx: number[] | null = null;
  if (op) {
    const ord = idxPairs(sql.slice(op.start, op.end));
    orderedIdx = ord.map((p) => keyToIndex.get(key(p))).filter((i): i is number => i != null);
  }
  const lp = seg.byKey.limit, ofp = seg.byKey.offset;
  const limitN = lp ? intFrom(sql.slice(lp.start, lp.end), "limit") : null;
  const offset = ofp ? intFrom(sql.slice(ofp.start, ofp.end), "offset") ?? 0 : 0;
  const hasLimit = !!(lp || ofp);

  const order = orderedIdx ?? pairs.map((_, i) => i);
  const kept0 = order.filter((i) => (passSet ? passSet.has(i) : true));
  const keptOrdered = hasLimit ? kept0.slice(offset, limitN != null ? offset + limitN : undefined) : kept0;
  const keptSet = new Set(keptOrdered);

  const combined = runQuery(`SELECT * FROM ${fromClause}`, db);
  const collage = runQuery(sql, db);
  // relations projetées intermédiaires : on tronque la requête avant ORDER BY / LIMIT
  const cutBefore = (keys: ("orderby" | "limit" | "offset")[]) => {
    let m = sql.length;
    for (const k of keys) { const p = seg.byKey[k]; if (p) m = Math.min(m, p.start); }
    return m;
  };
  const rowsOf = (q: string) => { const r = runQuery(q.trim(), db); return r.ok ? r.rows : []; };
  const selectRows = rowsOf(sql.slice(0, cutBefore(["orderby", "limit", "offset"]))); // WHERE + SELECT
  const orderRows = rowsOf(sql.slice(0, cutBefore(["limit", "offset"])));            // + ORDER BY

  return {
    ok: true, info,
    leftName: bt.name, leftCols: L.cols, leftRows: L.rows,
    rightName: jt.name, rightCols: R.cols, rightRows: R.rows,
    pairs,
    combinedCols: combined.ok ? combined.columns : [],
    combinedRows: combined.ok ? combined.rows : [],
    passSet, orderedIdx, keptSet, limitN, offset,
    hasWhere: !!wp, hasOrder: !!op, hasLimit,
    selectNames: seg.selectCols,
    collageCols: collage.ok ? collage.columns : [],
    collageRows: collage.ok ? collage.rows : [],
    selectRows, orderRows,
  };
}
