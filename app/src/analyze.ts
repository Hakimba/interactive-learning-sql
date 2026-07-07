// analyse UNIFIÉE d'une requête sur la table source (aperçu ET pas-à-pas).
// On ne projette jamais ici : on calcule, via le MOTEUR (colonne technique __idx),
//  - passSet     : indices satisfaisant le WHERE
//  - allOrderedIdx: tous les indices, dans l'ordre du ORDER BY (ou identité)
//  - keptSet/keptOrdered : le VRAI résultat = WHERE puis ORDER BY puis LIMIT/OFFSET
// Dégradation gracieuse : une clause incomplète est simplement ignorée (aucune erreur).

import { segment, type Segmentation } from "./clauses";
import { runQuery } from "./engine";
import type { Database, Val } from "./types";

const IDX = "__idx";
const intFrom = (t: string, kw: string): number | null => {
  const m = t.match(new RegExp(kw + "\\s+(\\d+)", "i"));
  return m ? parseInt(m[1], 10) : null;
};

export interface Analysis {
  ok: boolean;
  note?: string;
  columns: string[];
  sourceRows: Val[][];
  seg: Segmentation;
  passSet: Set<number> | null; // WHERE (null si absent/incomplet)
  allOrderedIdx: number[]; // tous les indices dans l'ordre ORDER BY (sinon identité)
  keptSet: Set<number>; // indices du vrai résultat (WHERE+ORDER+LIMIT)
  keptOrdered: number[]; // idem, dans l'ordre
  selectedCols: string[] | "*" | null;
  hasLimit: boolean;
  framingActive: boolean; // WHERE ou LIMIT présent -> il y a des lignes "gardées" à distinguer
}

export function analyze(sqlText: string, db: Database): Analysis {
  const seg = segment(sqlText);
  const base: Analysis = {
    ok: false, columns: [], sourceRows: [], seg, passSet: null, allOrderedIdx: [],
    keptSet: new Set(), keptOrdered: [], selectedCols: seg.selectCols, hasLimit: false, framingActive: false,
  };
  if (!seg.tableName) return { ...base, note: seg.hasFrom ? "table inconnue…" : "écris FROM <table> pour voir une table" };
  const table = db.tables.find((t) => t.name.toLowerCase() === seg.tableName!.toLowerCase());
  if (!table) return { ...base, note: `table « ${seg.tableName} » inconnue` };

  const columns = table.columns.map((c) => c.name);
  const sourceRows = table.rows.map((r) => columns.map((c) => r[c] ?? null));
  const n = sourceRows.length;

  const probeDb: Database = {
    tables: [{
      name: table.name,
      columns: [...table.columns, { name: IDX, type: "INTEGER" }],
      rows: table.rows.map((r, i) => ({ ...r, [IDX]: i })),
    }],
  };
  const idxCol = (q: string): number[] | null => {
    const r = runQuery(q, probeDb);
    if (!r.ok) return null;
    const j = r.columns.indexOf(IDX);
    return j < 0 ? null : r.rows.map((x) => x[j] as number);
  };

  // WHERE
  let passSet: Set<number> | null = null;
  const wp = seg.byKey.where;
  if (wp) {
    const ix = idxCol(`SELECT ${IDX} FROM ${table.name} ${sqlText.slice(wp.start, wp.end)}`);
    if (ix) passSet = new Set(ix);
  }

  // ORDER BY (sur toute la table)
  let allOrderedIdx = Array.from({ length: n }, (_, i) => i);
  const op = seg.byKey.orderby;
  if (op) {
    const ix = idxCol(`SELECT ${IDX} FROM ${table.name} ${sqlText.slice(op.start, op.end)}`);
    if (ix) allOrderedIdx = ix;
  }

  // LIMIT / OFFSET
  const lp = seg.byKey.limit;
  const ofp = seg.byKey.offset;
  const limitN = lp ? intFrom(sqlText.slice(lp.start, lp.end), "limit") : null;
  const offset = ofp ? intFrom(sqlText.slice(ofp.start, ofp.end), "offset") ?? 0 : 0;
  const hasLimit = !!(lp || ofp);

  // Vrai résultat : WHERE, puis ORDER BY, puis fenêtre LIMIT/OFFSET.
  // (filtrer après tri préserve l'ordre → équivalent à filtrer avant.)
  const kept0 = allOrderedIdx.filter((i) => (passSet ? passSet.has(i) : true));
  const keptOrdered = hasLimit ? kept0.slice(offset, limitN != null ? offset + limitN : undefined) : kept0;
  const keptSet = new Set(keptOrdered);

  return {
    ok: true, columns, sourceRows, seg, passSet, allOrderedIdx, keptSet, keptOrdered,
    selectedCols: seg.selectCols, hasLimit, framingActive: passSet != null || hasLimit,
  };
}
