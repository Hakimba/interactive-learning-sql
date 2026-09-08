// Courbe de bascule : coût estimé du Seq Scan et du chemin d'index selon la sélectivité (part des lignes retenues).
// Axe x logarithmique (0,01 % → 100 %), une seule échelle y. La requête courante est marquée à sa sélectivité
// observée ; le point de croisement dit à partir de quelle sélectivité l'index cesse d'être rentable.
// Couleurs de séries : --chart-seq / --chart-idx (validées pour les deux thèmes) ; le texte reste en encre.
import { useState } from "preact/hooks";
import { fmtCost } from "../exec";

type Pt = { sel: number; seq: number; idx: number };
const W = 560, H = 190, L = 58, R = 16, T = 14, B = 34;
const fmtAxis = (c: number) => (c >= 1e6 ? `${(c / 1e6).toFixed(1)} M` : c >= 1000 ? `${Math.round(c / 1000)} k` : fmtCost(c));

const pct = (s: number) => (s >= 0.1 ? `${Math.round(s * 100)} %` : s >= 0.01 ? `${(s * 100).toFixed(1)} %` : `${(s * 100).toFixed(2)} %`);

export function CostChart({ curve, indexName, querySel, scaleLabel }: { curve: Pt[]; indexName: string; querySel: number; scaleLabel: string }) {
  const [hover, setHover] = useState<number | null>(null);
  if (curve.length < 2) return null;
  const lx = (s: number) => L + ((Math.log10(Math.max(1e-4, Math.min(1, s))) + 4) / 4) * (W - L - R);
  const maxY = Math.max(...curve.map((p) => Math.max(p.seq, p.idx)));
  const ly = (c: number) => T + (1 - c / maxY) * (H - T - B);
  const path = (k: "seq" | "idx") => curve.map((p, i) => `${i ? "L" : "M"}${lx(p.sel).toFixed(1)},${ly(p[k]).toFixed(1)}`).join(" ");
  // croisement : premier point où l'index devient plus cher que le Seq Scan
  const cross = curve.findIndex((p, i) => i > 0 && p.idx > p.seq && curve[i - 1].idx <= curve[i - 1].seq);
  const crossSel = cross > 0 ? Math.sqrt(curve[cross - 1].sel * curve[cross].sel) : null;
  const qs = Math.max(1e-4, Math.min(1, querySel));
  const nearest = (s: number) => curve.reduce((b, p, i) => (Math.abs(Math.log10(p.sel) - Math.log10(s)) < Math.abs(Math.log10(curve[b].sel) - Math.log10(s)) ? i : b), 0);
  const qi = nearest(qs);
  const hi = hover ?? qi;
  const hp = curve[hi];
  const ticks = [1e-4, 1e-3, 1e-2, 1e-1, 1];
  const onMove = (e: MouseEvent) => {
    const svg = e.currentTarget as SVGSVGElement;
    const r = svg.getBoundingClientRect();
    const x = ((e.clientX - r.left) / r.width) * W;
    const s = 10 ** (((x - L) / (W - L - R)) * 4 - 4);
    setHover(nearest(s));
  };
  return (
    <figure class="cost-chart">
      <figcaption>
        <span class="cc-title">Coût selon la sélectivité <span class="muted">· {scaleLabel}</span></span>
        <span class="cc-legend"><i class="cc-key seq" /> Seq Scan <i class="cc-key idx" /> Index Scan · {indexName}</span>
      </figcaption>
      <svg viewBox={`0 0 ${W} ${H}`} class="cc-svg" onMouseMove={onMove} onMouseLeave={() => setHover(null)} role="img" aria-label={`coût du Seq Scan et de l'index ${indexName} selon la sélectivité`}>
        {ticks.map((t) => <g><line class="cc-grid" x1={lx(t)} x2={lx(t)} y1={T} y2={H - B} /><text class="cc-tick" x={lx(t)} y={H - B + 14} text-anchor="middle">{pct(t)}</text></g>)}
        <line class="cc-axis" x1={L} x2={W - R} y1={H - B} y2={H - B} />
        <text class="cc-tick" x={L - 6} y={T + 4} text-anchor="end">{fmtAxis(maxY)}</text>
        <text class="cc-tick" x={L - 6} y={H - B} text-anchor="end">0</text>
        <text class="cc-tick" x={W - R} y={H - 4} text-anchor="end">part des lignes retenues (log)</text>
        {crossSel ? <g><line class="cc-cross" x1={lx(crossSel)} x2={lx(crossSel)} y1={T} y2={H - B} /><text class="cc-note" x={lx(crossSel) + 4} y={T + 10}>bascule ≈ {pct(crossSel)}</text></g> : null}
        <path class="cc-line seq" d={path("seq")} />
        <path class="cc-line idx" d={path("idx")} />
        <line class="cc-hair" x1={lx(hp.sel)} x2={lx(hp.sel)} y1={T} y2={H - B} />
        <circle class="cc-dot seq" cx={lx(hp.sel)} cy={ly(hp.seq)} r={4} />
        <circle class="cc-dot idx" cx={lx(hp.sel)} cy={ly(hp.idx)} r={4} />
        <g class="cc-query"><circle cx={lx(qs)} cy={H - B} r={4} />
          <text class="cc-note" x={lx(qs) < L + 70 ? lx(qs) + 7 : lx(qs) > W - R - 70 ? lx(qs) - 7 : lx(qs)} y={H - B - 6}
                text-anchor={lx(qs) < L + 70 ? "start" : lx(qs) > W - R - 70 ? "end" : "middle"}>ta requête · {pct(qs)}</text></g>
      </svg>
      <div class="cc-readout">
        <span><b>{pct(hp.sel)}</b> des lignes retenues →</span>
        <span><i class="cc-key seq" /> <b>{fmtCost(hp.seq)}</b> Seq Scan</span>
        <span><i class="cc-key idx" /> <b>{fmtCost(hp.idx)}</b> Index Scan</span>
        <span class="muted">{hp.idx < hp.seq ? "l'index gagne" : "le parcours complet gagne"}{hover === null ? " (ta requête)" : ""}</span>
      </div>
    </figure>
  );
}
