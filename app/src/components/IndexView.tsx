// Vue d'un index B-tree (Comer 1979) : l'arbre (séparateurs → feuilles) et les entrées triées « clé → n° de ligne ».
// Le chemin de descente et l'intervalle [lo, hi) réellement parcouru sont surlignés — c'est le « log n » rendu visible.
import { useEffect, useRef } from "preact/hooks";
import type { BuiltIndex, Descent, TreeNode } from "../types";
import { fmtKey, fmtVal } from "../exec";

const NW = 78, NH = 24, GAP = 8, LEVEL_H = 60;
// libellés courts dans les boîtes (le détail complet est dans le <title> au survol)
const trunc = (s: string, n: number) => (s.length > n ? s.slice(0, n - 1) + "…" : s);

export function IndexView({ index, descents, backward }: { index: BuiltIndex; descents: Descent[]; backward: boolean }) {
  const tree = index.tree;
  const onPath = new Set<number>();
  descents.forEach((d) => d.path.forEach((id) => onPath.add(id)));
  const inRange = (pos: number) => descents.some((d) => pos >= d.lo && pos < d.hi);
  const n = index.entries.length;
  const wrap = useRef<HTMLDivElement>(null);
  const entriesWrap = useRef<HTMLDivElement>(null);
  // centre la vue (arbre + liste d'entrées) sur la feuille atteinte par la descente
  useEffect(() => {
    const w = wrap.current;
    const hot = w?.querySelector<SVGGElement>(".bt-node.hot.leaf");
    if (w && hot) { const r = hot.getBoundingClientRect(), wr = w.getBoundingClientRect(); w.scrollLeft += r.left - wr.left - wr.width / 2 + r.width / 2; }
    const ew = entriesWrap.current;
    const first = ew?.querySelector<HTMLTableRowElement>("tr.on");
    if (ew && first) ew.scrollTop = Math.max(0, first.offsetTop - 60);
  }, [index.name, descents.map((d) => d.lo).join(",")]);

  let svg = null;
  if (tree) {
    const leaves = tree.nodes.filter((nd) => nd.level === 0).sort((a, b) => a.first - b.first);
    const x = new Map<number, number>();
    leaves.forEach((nd, i) => x.set(nd.id, 12 + i * (NW + GAP)));
    // les nœuds internes se centrent au-dessus de leurs enfants (niveau par niveau)
    for (let lv = 1; lv < tree.height; lv++) {
      tree.nodes.filter((nd) => nd.level === lv).forEach((nd) => {
        const xs = nd.children.map((c) => x.get(c) ?? 0);
        x.set(nd.id, xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length));
      });
    }
    const y = (nd: TreeNode) => 8 + (tree.height - 1 - nd.level) * LEVEL_H;
    const width = Math.max(320, 24 + leaves.length * (NW + GAP));
    const height = 8 + tree.height * LEVEL_H;
    const label = (nd: TreeNode) => {
      if (nd.level > 0) return nd.seps.length ? nd.seps.map((s) => trunc(fmtVal(s[0]), 6)).join("|") : "·";
      if (nd.first >= n) return "∅";
      return `${trunc(fmtVal(index.entries[nd.first].key[0]), 8)} ·${nd.last - nd.first}`;
    };
    const leafTouched = (nd: TreeNode) => nd.level === 0 && descents.some((d) => d.lo < nd.last && Math.max(d.hi, d.lo + 1) > nd.first);
    svg = (
      <div class="btree-wrap" ref={wrap}>
        <svg class="btree" width={width} height={height} viewBox={`0 0 ${width} ${height}`}>
          {tree.nodes.map((nd) => nd.children.map((c) => {
            const ch = tree.nodes[c];
            const hot = onPath.has(nd.id) && onPath.has(c);
            return <line class={"bt-edge" + (hot ? " hot" : "")} x1={(x.get(nd.id) ?? 0) + NW / 2} y1={y(nd) + NH} x2={(x.get(c) ?? 0) + NW / 2} y2={y(ch)} />;
          }))}
          {tree.nodes.map((nd) => {
            const hot = onPath.has(nd.id), leaf = nd.level === 0;
            const cls = "bt-node" + (leaf ? " leaf" : "") + (hot ? " hot" : "") + (leafTouched(nd) ? " touched" : "");
            return (
              <g class={cls}>
                <rect x={x.get(nd.id) ?? 0} y={y(nd)} width={NW} height={NH} rx={5} />
                <text x={(x.get(nd.id) ?? 0) + NW / 2} y={y(nd) + NH / 2 + 4} text-anchor="middle">{label(nd)}</text>
                <title>{leaf ? `feuille : entrées ${nd.first}–${Math.max(nd.first, nd.last - 1)}` : `nœud interne : séparateurs ${nd.seps.map((s) => fmtKey(s)).join(" | ")}`}</title>
              </g>
            );
          })}
        </svg>
      </div>
    );
  }

  return (
    <div class="index-view">
      <div class="iv-head">
        <b>{index.name}</b> <span class="muted">({index.columns.join(", ")}){index.unique ? " · UNIQUE" : ""}{index.implicit ? " · clé primaire" : ""}</span>
        {tree ? <span class="muted"> · fan-out {tree.fanout} · hauteur {tree.height} · {n} entrée(s){backward ? " · parcours arrière" : ""}</span> : null}
      </div>
      {svg}
      <div class="entries-wrap" ref={entriesWrap}>
        <table class="grid entries">
          <thead><tr><th>#</th><th>clé ({index.columns.join(", ")})</th><th>→ ligne</th></tr></thead>
          <tbody>
            {index.entries.map((e, pos) => (
              <tr class={inRange(pos) ? "on" : "dim"}>
                <td class="rownum">{pos}</td>
                <td>{fmtKey(e.key)}</td>
                <td class="num">#{e.rowid + 1}</td>
              </tr>
            ))}
          </tbody>
        </table>
        {n === 0 ? <div class="grid-empty">— index vide —</div> : null}
      </div>
    </div>
  );
}
