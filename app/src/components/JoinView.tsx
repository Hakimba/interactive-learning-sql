// Aperçu (onglet Éditeur) d'une jointure : la table JOINTE, avec le MÊME surlignage
// réactif qu'une table simple — WHERE cadre les lignes, SELECT les colonnes, ORDER BY
// réordonne, LIMIT coupe. Accordéon (« Afficher uniquement le résultat ») → collage.
// (L'appariement 2-tables détaillé vit dans l'onglet Pas à pas.)
import { resultOnly } from "../state";
import type { JoinData } from "../join";
import { Grid } from "./Grid";

export function JoinPreview({ data }: { data: JoinData }) {
  if (!data.ok) return <div class="placeholder">{data.note}</div>;

  if (resultOnly.value) {
    return (
      <div class="preview">
        <div class="preview-bar"><span class="mode-tag applied">résultat</span> {data.collageRows.length} ligne(s) — {data.info.kind} JOIN</div>
        <Grid columns={data.collageCols} rows={data.collageRows} />
      </div>
    );
  }

  const identity = data.combinedRows.map((_, i) => i);
  const order = data.orderedIdx ?? identity;
  const framingActive = data.passSet != null || data.hasLimit;
  const rows = order.map((i) => data.combinedRows[i]);
  const rowClass = (p: number) => {
    const orig = order[p];
    if (data.passSet && !data.passSet.has(orig)) return "row-fail";
    if (framingActive && !data.keptSet.has(orig)) return "row-cut";
    if (framingActive) return "row-pass";
    return "";
  };
  const sel = data.selectNames;
  const hiCol = (name: string) => Array.isArray(sel) && sel.includes(name.split(".").pop() || name);
  const note = framingActive
    ? `${data.keptSet.size}/${data.combinedRows.length} ligne(s) gardée(s)`
    : `${data.combinedRows.length} ligne(s) (jointure ${data.info.kind})`;

  return (
    <div class="preview">
      <div class="preview-bar"><span class="mode-tag">aperçu</span> {note} — clique une table/colonne à gauche, ou passe au Pas à pas</div>
      <Grid columns={data.combinedCols} rows={rows} rowClass={rowClass} highlightCol={hiCol} />
    </div>
  );
}
