// Pas-à-pas d'une jointure, par CHUNKS (ordre d'évaluation) :
//   FROM (A) → JOIN (A ⋈ B) → WHERE (surlignage) →
//   SELECT en 2 temps : (1) colonnes surlignées, (2) projection RÉELLEMENT appliquée →
//   ORDER BY (tri réel) → LIMIT (coupe réelle).
// La requête est affichée avec un RECTANGLE coloré qui grandit clause par clause (façon Coq).
import { stepIndex, rightTab, stepSql } from "../state";
import { segment } from "../clauses";
import type { JoinData } from "../join";
import type { Val } from "../types";
import { Grid } from "./Grid";

type Chunk = "from" | "join" | "where" | "selhi" | "selproj" | "orderby" | "limit";

function fmt(v: Val) {
  if (v === null) return <span class="null">NULL</span>;
  if (typeof v === "boolean") return <span class="bool">{String(v)}</span>;
  if (typeof v === "number") return <span class="num">{String(v)}</span>;
  return <span>{String(v)}</span>;
}

function CombinedTable(props: { cols: string[]; rows: Val[][]; grp: (j: number) => "gA" | "gB"; countA: number; countB: number; aName: string; bName: string; rowClass: (p: number) => string; hiCol: (name: string) => boolean }) {
  const cc = (j: number) => props.grp(j) + (props.hiCol(props.cols[j]) ? " col-hi" : "");
  return (
    <div class="grid-wrap">
      <table class="grid">
        <thead>
          <tr class="group-row">
            {props.countA > 0 ? <th class="grp gA" colSpan={props.countA}>{props.aName} · A</th> : null}
            {props.countB > 0 ? <th class="grp gB" colSpan={props.countB}>{props.bName} · B</th> : null}
          </tr>
          <tr>{props.cols.map((c, j) => <th class={cc(j)}>{c}</th>)}</tr>
        </thead>
        <tbody>
          {props.rows.map((r, p) => <tr class={"jrow " + props.rowClass(p)}>{r.map((v, j) => <td class={cc(j)}>{fmt(v)}</td>)}</tr>)}
        </tbody>
      </table>
    </div>
  );
}

function BaseTable({ name, cols, rows }: { name: string; cols: string[]; rows: Val[][] }) {
  return (
    <div class="grid-wrap">
      <table class="grid">
        <thead>
          <tr class="group-row"><th class="grp gA" colSpan={cols.length}>{name} · A</th></tr>
          <tr>{cols.map((c) => <th class="gA">{c}</th>)}</tr>
        </thead>
        <tbody>{rows.map((r) => <tr class="jrow">{r.map((v) => <td class="gA">{fmt(v)}</td>)}</tr>)}</tbody>
      </table>
    </div>
  );
}

export function JoinStep({ data }: { data: JoinData }) {
  if (!data.ok) return <div class="placeholder">{data.note}</div>;
  const text = stepSql.value;

  const chunks: Chunk[] = ["from", "join"];
  if (data.hasWhere) chunks.push("where");
  chunks.push("selhi", "selproj");
  if (data.hasOrder) chunks.push("orderby");
  if (data.hasLimit) chunks.push("limit");

  const nb = chunks.length;
  const idx = Math.min(stepIndex.value, nb - 1);
  const set = (i: number) => (stepIndex.value = Math.max(0, Math.min(nb - 1, i)));
  const cur = chunks[idx];

  const baseAlias = data.info.base.alias.toLowerCase();
  const countA = data.combinedCols.filter((c) => c.split(".")[0].toLowerCase() === baseAlias).length;
  const countB = data.combinedCols.length - countA;
  const grp = (j: number): "gA" | "gB" => (j < countA ? "gA" : "gB");

  // rectangle coloré dans la requête (spans par chunk, cumul en ordre d'éval)
  const seg = segment(text);
  const fromP = seg.byKey.from;
  let joinPos = fromP ? fromP.end : 0;
  if (fromP) {
    const m = text.slice(fromP.start, fromP.end).match(/\b(?:inner|left|right|full|cross)\b|\bjoin\b|,/i);
    if (m && m.index != null) joinPos = fromP.start + m.index;
  }
  const spanOf = (c: Chunk): [number, number] | null => {
    if (c === "from") return fromP ? [fromP.start, joinPos] : null;
    if (c === "join") return fromP ? [joinPos, fromP.end] : null;
    if (c === "selhi" || c === "selproj") { const p = seg.byKey.select; return p ? [p.start, p.end] : null; }
    if (c === "orderby") { const p = seg.byKey.orderby; return p ? [p.start, p.end] : null; }
    if (c === "where") { const p = seg.byKey.where; return p ? [p.start, p.end] : null; }
    const p = seg.byKey.limit ?? seg.byKey.offset; return p ? [p.start, p.end] : null;
  };
  let rStart = Infinity, rEnd = -Infinity;
  chunks.slice(0, idx + 1).forEach((c) => { const s = spanOf(c); if (s) { rStart = Math.min(rStart, s[0]); rEnd = Math.max(rEnd, s[1]); } });
  const hasRect = rStart < rEnd;
  let realEnd = rEnd;
  while (realEnd > rStart && /\s/.test(text[realEnd - 1])) realEnd--;

  const whereRowClass = (p: number) => (data.passSet && !data.passSet.has(p) ? "row-fail" : data.passSet ? "row-pass" : "");
  const sel = data.selectNames;
  const selHiCol = (name: string) => Array.isArray(sel) && sel.includes(name.split(".").pop() || name);

  const label: Record<Chunk, string> = { from: "FROM", join: `${data.info.kind} JOIN`, where: "WHERE", selhi: "SELECT", selproj: "SELECT ✓", orderby: "ORDER BY", limit: "LIMIT" };
  const note = (() => {
    switch (cur) {
      case "from": return `lecture de « ${data.leftName} » (A) — ${data.leftRows.length} ligne(s)`;
      case "join": return `${data.leftName} (A) ⋈ ${data.rightName} (B) → ${data.combinedRows.length} ligne(s)`;
      case "where": return data.passSet ? `${data.passSet.size}/${data.combinedRows.length} ligne(s) gardée(s)` : "WHERE";
      case "selhi": return Array.isArray(sel) ? "colonnes du SELECT surlignées (avant projection)" : "toutes les colonnes";
      case "selproj": return `projection appliquée → ${data.selectRows.length} ligne(s), ${data.collageCols.length} colonne(s)`;
      case "orderby": return `trié → ${data.orderRows.length} ligne(s)`;
      case "limit": return `${data.collageRows.length} ligne(s) gardée(s)`;
    }
  })();

  const body = () => {
    if (cur === "from") return <BaseTable name={data.leftName} cols={data.leftCols} rows={data.leftRows} />;
    if (cur === "join" || cur === "where" || cur === "selhi")
      return <CombinedTable cols={data.combinedCols} rows={data.combinedRows} grp={grp} countA={countA} countB={countB}
        aName={data.leftName} bName={data.rightName}
        rowClass={cur === "join" ? () => "" : whereRowClass}
        hiCol={cur === "selhi" ? selHiCol : () => false} />;
    const projRows = cur === "selproj" ? data.selectRows : cur === "orderby" ? data.orderRows : data.collageRows;
    return <Grid columns={data.collageCols} rows={projRows} />;
  };

  return (
    <>
      <div class="lock-bar">
        <button class="btn ghost small" onClick={() => (rightTab.value = "editor")}>← revenir à l'éditeur</button>
      </div>
      <div class="proof-query">
        {hasRect ? (
          <>
            <span class="q-dim">{text.slice(0, rStart)}</span>
            <span class="q-bracket">{text.slice(rStart, realEnd)}</span>
            <span class="q-dim">{text.slice(realEnd)}</span>
          </>
        ) : <span class="q-dim">{text}</span>}
      </div>
      <div class="step-controls">
        <button class="btn ghost" onClick={() => set(0)} title="Début">⏮</button>
        <button class="btn ghost" onClick={() => set(idx - 1)} disabled={idx === 0}>◀</button>
        <button class="btn primary" onClick={() => set(idx + 1)} disabled={idx === nb - 1}>avancer ▶</button>
        <button class="btn ghost" onClick={() => set(nb - 1)} title="Fin">⏭</button>
        <span class="step-label">étape {idx + 1}/{nb} · <b>{label[cur]}</b> — {note}</span>
      </div>
      <div class="chips-row">{chunks.map((c, i) => <span class={"stage-chip" + (i === idx ? " on" : "")}>{label[c]}</span>)}</div>
      {cur === "join" || cur === "where" || cur === "selhi" ? (
        <div class="join-cap"><span class="pill gA">{data.leftName} · A</span> ⋈ <span class="pill gB">{data.rightName} · B</span></div>
      ) : null}
      <div class="result-area">{body()}</div>
    </>
  );
}
