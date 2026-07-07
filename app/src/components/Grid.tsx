// Grille de données réutilisable (colonnes + lignes de valeurs).
// Supporte le cadrage de lignes (rowClass) et de colonnes (highlightCol).
import type { ComponentChildren } from "preact";
import type { Val } from "../types";

function Cell({ v }: { v: Val }) {
  if (v === null) return <span class="null">NULL</span>;
  if (typeof v === "boolean") return <span class="bool">{v ? "true" : "false"}</span>;
  if (typeof v === "number") return <span class="num">{String(v)}</span>;
  return <span>{String(v)}</span>;
}

export function Grid({
  columns,
  rows,
  rowClass,
  rowPrefix,
  highlightCol,
  foldCol,
  onColClick,
}: {
  columns: string[];
  rows: Val[][];
  rowClass?: (i: number) => string;
  rowPrefix?: (i: number) => ComponentChildren;
  highlightCol?: (name: string) => boolean;
  foldCol?: (name: string) => boolean;
  onColClick?: (name: string) => void;
}) {
  const colCls = (j: number) =>
    (highlightCol && highlightCol(columns[j]) ? " col-hi" : "") +
    (foldCol && foldCol(columns[j]) ? " col-folded" : "");
  return (
    <div class="grid-wrap">
      <table class="grid">
        <thead>
          <tr>
            {rowPrefix ? <th class="gutter"></th> : null}
            {columns.map((c, j) => (
              <th
                class={"th" + colCls(j) + (onColClick ? " clickable" : "")}
                title={onColClick ? "insérer « " + c + " » dans la requête" : undefined}
                onClick={onColClick ? () => onColClick(c) : undefined}
              >
                {c}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((r, i) => (
            <tr class={rowClass ? rowClass(i) : ""}>
              {rowPrefix ? <td class="gutter">{rowPrefix(i)}</td> : null}
              {r.map((v, j) => (
                <td class={colCls(j).trim()}>
                  <Cell v={v} />
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
      {rows.length === 0 ? <div class="grid-empty">— aucune ligne —</div> : null}
    </div>
  );
}
