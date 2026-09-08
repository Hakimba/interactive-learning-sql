// La table vue comme un TAS paginé (rows_per_page lignes par page — paramètre du modèle) :
// chaque case = une ligne ; on voit d'un coup d'œil ce que l'exécuteur a LU et ce qu'il a SAUTÉ.
export function HeapMap({ n, rowsPerPage, touched, kept, indexOnly, stream }: {
  n: number; rowsPerPage: number; touched: Set<number>; kept: Set<number>; indexOnly: boolean; stream: Set<number>;
}) {
  const pages = Math.max(1, Math.ceil(n / rowsPerPage));
  const cls = (rid: number) => {
    if (rid >= n) return "cell void";
    if (touched.has(rid)) return "cell " + (kept.has(rid) ? "kept" : "read");
    if (indexOnly && stream.has(rid)) return "cell idx";
    return "cell skip";
  };
  const pageTouched = (p: number) => Array.from({ length: rowsPerPage }, (_, k) => p * rowsPerPage + k).some((rid) => touched.has(rid));
  return (
    <div class="heapmap">
      <div class="hm-pages">
        {Array.from({ length: pages }, (_, p) => (
          <div class={"hm-page" + (pageTouched(p) ? " on" : "")} title={`page ${p}`}>
            <span class="hm-pnum">p{p}</span>
            {Array.from({ length: rowsPerPage }, (_, k) => { const rid = p * rowsPerPage + k; return <span class={cls(rid)} title={rid < n ? `ligne #${rid + 1}` : ""} />; })}
          </div>
        ))}
      </div>
      <div class="hm-legend">
        <span><i class="cell kept" /> lue et retenue</span>
        <span><i class="cell read" /> lue puis rejetée</span>
        {indexOnly ? <span><i class="cell idx" /> servie par l'index (tas non lu)</span> : null}
        <span><i class="cell skip" /> jamais lue</span>
        <span class="muted">· {pages} page(s) de {rowsPerPage} lignes</span>
      </div>
    </div>
  );
}
