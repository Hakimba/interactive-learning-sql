// Panneau gauche : sandbox des tables. Vue Schéma = MAP (façon DBeaver / diagramme UML) :
// cartes déplaçables, pan/zoom, relations FK dessinées automatiquement. Vue Données = grille.
import { useState, useRef, useEffect } from "preact/hooks";
import { tables, activeTable, leftView, relations, setSql, resultOnly } from "../state";
import { TYPES, SCHEMAS, cloneSchema, familyOf } from "../catalog";
import { generateRows } from "../datagen";
import type { Column, Table } from "../types";
import { Grid } from "./Grid";

const W = 210, HEAD = 30, RH = 24; // dimensions fixes des cartes (pour placer les connecteurs sans mesurer le DOM)

// Clic = construire une requête ET projeter pour de vrai (resultOnly = on voit la vraie projection).
const selCol = (col: string, table: string) => { setSql(`SELECT ${col} FROM ${table}`); resultOnly.value = true; };
const selAll = (table: string) => { setSql(`SELECT * FROM ${table}`); resultOnly.value = true; };

function replaceTable(name: string, updater: (t: Table) => Table) {
  tables.value = tables.value.map((t) => (t.name === name ? updater(t) : t));
}

function loadPreset(key: string) {
  const s = cloneSchema(key);
  if (!s) return;
  s.tables.forEach((t) => (t.rows = generateRows(t, 8)));
  tables.value = s.tables;
  relations.value = s.relations; // ← les FK des presets, dessinées sur la map
  activeTable.value = s.tables[0]?.name ?? null;
}

function TableCreator({ onClose }: { onClose: () => void }) {
  const [name, setName] = useState("ma_table");
  const [cols, setCols] = useState<Column[]>([
    { name: "id", type: "INTEGER", pk: true },
    { name: "libelle", type: "VARCHAR" },
  ]);
  const [err, setErr] = useState("");
  const setCol = (i: number, patch: Partial<Column>) => setCols(cols.map((c, k) => (k === i ? { ...c, ...patch } : c)));

  function save() {
    const nm = name.trim();
    if (!nm) return setErr("Nomme la table.");
    if (tables.value.some((t) => t.name.toLowerCase() === nm.toLowerCase())) return setErr("Ce nom existe déjà.");
    const clean = cols.filter((c) => c.name.trim());
    if (clean.length === 0) return setErr("Ajoute au moins une colonne.");
    const k = tables.value.length;
    const t: Table = { name: nm, columns: clean, rows: [], x: 40 + (k % 3) * 240, y: 40 + Math.floor(k / 3) * 200 };
    t.rows = generateRows(t, 8);
    tables.value = [...tables.value, t];
    activeTable.value = nm;
    onClose();
  }

  return (
    <div class="modal-backdrop" onClick={onClose}>
      <div class="modal" onClick={(e) => e.stopPropagation()}>
        <div class="modal-head"><strong>Nouvelle table</strong><button class="btn ghost" onClick={onClose}>✕</button></div>
        <label class="field"><span>Nom</span><input value={name} onInput={(e) => setName((e.target as HTMLInputElement).value)} /></label>
        <div class="cols-editor">
          {cols.map((c, i) => (
            <div class="col-row">
              <input class="c-name" value={c.name} placeholder="colonne" onInput={(e) => setCol(i, { name: (e.target as HTMLInputElement).value })} />
              <select class="c-type" value={c.type} onChange={(e) => setCol(i, { type: (e.target as HTMLSelectElement).value })}>
                {TYPES.map((t) => <option value={t.name}>{t.name}</option>)}
              </select>
              <label class="c-pk"><input type="checkbox" checked={!!c.pk} onChange={(e) => setCol(i, { pk: (e.target as HTMLInputElement).checked })} /> PK</label>
              <button class="btn ghost small" onClick={() => setCols(cols.filter((_, k) => k !== i))}>✕</button>
            </div>
          ))}
          <button class="btn ghost small" onClick={() => setCols([...cols, { name: "", type: "VARCHAR" }])}>+ colonne</button>
        </div>
        {err ? <div class="modal-error">{err}</div> : null}
        <div class="modal-foot"><button class="btn ghost" onClick={onClose}>Annuler</button><button class="btn primary" onClick={save}>Créer</button></div>
      </div>
    </div>
  );
}

type DragState = { mode: "pan" | "card" | "select"; name?: string; sx: number; sy: number; ox: number; oy: number };

function SchemaCanvas() {
  const ts = tables.value;
  const rels = relations.value;
  const [pan, setPan] = useState({ x: 24, y: 24 });
  const [zoom, setZoom] = useState(1);
  const drag = useRef<DragState | null>(null);
  // sélection multiple de colonnes (glisser sur les lignes d'une carte)
  const [sel, setSel] = useState<{ table: string; cols: string[] } | null>(null);
  const selRef = useRef<{ table: string; cols: string[] } | null>(null);
  const setSelection = (v: { table: string; cols: string[] } | null) => { selRef.current = v; setSel(v); };

  // projette réellement les colonnes choisies (dans l'ordre de la table)
  const applySelect = (table: string, cols: string[]) => {
    const t = tables.value.find((x) => x.name === table);
    const ordered = t ? t.columns.map((c) => c.name).filter((n) => cols.includes(n)) : cols;
    setSql(`SELECT ${ordered.length ? ordered.join(", ") : "*"} FROM ${table}`);
    resultOnly.value = true;
  };

  useEffect(() => {
    const move = (e: MouseEvent) => {
      const d = drag.current;
      if (!d) return;
      if (d.mode === "pan") setPan({ x: d.ox + (e.clientX - d.sx), y: d.oy + (e.clientY - d.sy) });
      else if (d.mode === "card") {
        const nx = d.ox + (e.clientX - d.sx) / zoom, ny = d.oy + (e.clientY - d.sy) / zoom;
        tables.value = tables.value.map((t) => (t.name === d.name ? { ...t, x: nx, y: ny } : t));
      }
    };
    const up = () => {
      if (drag.current?.mode === "select" && selRef.current) applySelect(selRef.current.table, selRef.current.cols);
      drag.current = null;
    };
    document.addEventListener("mousemove", move);
    document.addEventListener("mouseup", up);
    return () => { document.removeEventListener("mousemove", move); document.removeEventListener("mouseup", up); };
  }, [zoom]);

  if (ts.length === 0) return <div class="hint">Charge un schéma ou crée une table — la vue devient une carte déplaçable.</div>;

  const X = (t?: Table) => t?.x ?? 40;
  const Y = (t?: Table) => t?.y ?? 40;
  const byName = (n: string) => ts.find((t) => t.name.toLowerCase() === n.toLowerCase());
  const colY = (t: Table, col: string) => {
    const i = t.columns.findIndex((c) => c.name.toLowerCase() === col.toLowerCase());
    return Y(t) + HEAD + ((i < 0 ? 0 : i) + 0.5) * RH;
  };

  const paths = rels.map((r) => {
    const a = byName(r.fromTable), b = byName(r.toTable);
    if (!a || !b) return null;
    const ay = colY(a, r.fromCol), by = colY(b, r.toCol);
    const aLeft = X(a) + W / 2 <= X(b) + W / 2;
    const ax = aLeft ? X(a) + W : X(a);
    const bx = aLeft ? X(b) : X(b) + W;
    const dx = Math.max(30, Math.abs(bx - ax) / 2);
    const c1 = ax + (aLeft ? dx : -dx), c2 = bx + (aLeft ? -dx : dx);
    return { r, d: `M ${ax} ${ay} C ${c1} ${ay}, ${c2} ${by}, ${bx} ${by}`, ax, ay, bx, by, mx: (ax + bx) / 2, my: (ay + by) / 2 };
  }).filter((p): p is NonNullable<typeof p> => p != null);

  // clic sur une relation → génère la jointure. L'ordre suit la MAP : table la plus à gauche = FROM.
  const genJoin = (r: { fromTable: string; fromCol: string; toTable: string; toCol: string }) => {
    const fromLeft = X(byName(r.fromTable)) <= X(byName(r.toTable));
    const base = fromLeft ? r.fromTable : r.toTable, baseCol = fromLeft ? r.fromCol : r.toCol;
    const other = fromLeft ? r.toTable : r.fromTable, otherCol = fromLeft ? r.toCol : r.fromCol;
    setSql(`SELECT * FROM ${base} JOIN ${other} ON ${base}.${baseCol} = ${other}.${otherCol}`);
  };

  const startPan = (e: MouseEvent) => {
    if ((e.target as HTMLElement).closest(".node")) return;
    drag.current = { mode: "pan", sx: e.clientX, sy: e.clientY, ox: pan.x, oy: pan.y };
  };

  return (
    <div class="schema-map" onMouseDown={startPan} onWheel={(e) => { e.preventDefault(); setZoom((z) => Math.min(2, Math.max(0.4, z * (e.deltaY < 0 ? 1.1 : 0.9)))); }}>
      <div class="stage" style={`transform: translate(${pan.x}px, ${pan.y}px) scale(${zoom});`}>
        <svg class="rels" width="4000" height="3000">
          {paths.map((p) => (
            <g class="reljoin" onMouseDown={(e) => e.stopPropagation()} onClick={() => genJoin(p.r)}>
              <title>créer la jointure {p.r.fromTable}.{p.r.fromCol} = {p.r.toTable}.{p.r.toCol}</title>
              <path d={p.d} class="rel" fill="none" />
              <circle cx={p.ax} cy={p.ay} r="3.5" class="reldot" />
              <circle cx={p.bx} cy={p.by} r="3.5" class="reldot" />
              <circle cx={p.mx} cy={p.my} r="10" class="reljoin-bg" />
              <text x={p.mx} y={p.my} class="reljoin-txt" text-anchor="middle" dominant-baseline="central">⋈</text>
            </g>
          ))}
        </svg>
        {ts.map((t) => (
          <div class="node" style={`left:${X(t)}px; top:${Y(t)}px; width:${W}px;`}>
            <div class="node-head" onMouseDown={(e) => { e.stopPropagation(); drag.current = { mode: "card", name: t.name, sx: e.clientX, sy: e.clientY, ox: X(t), oy: Y(t) }; }}>
              <button class="link-name" title={"SELECT * FROM " + t.name} onMouseDown={(e) => e.stopPropagation()} onClick={() => selAll(t.name)}>{t.name}</button>
              <span class="tc-count">{t.rows.length}</span>
            </div>
            {t.columns.map((c) => (
              <div
                class={"node-col clickable" + (sel && sel.table === t.name && sel.cols.includes(c.name) ? " sel" : "")}
                title="clic = projeter · glisser = plusieurs contiguës · Ctrl/Cmd+clic = ajouter/retirer"
                onMouseDown={(e) => {
                  e.stopPropagation();
                  if (e.ctrlKey || e.metaKey) {
                    const cur = selRef.current?.table === t.name ? selRef.current.cols : [];
                    const cols = cur.includes(c.name) ? cur.filter((x) => x !== c.name) : [...cur, c.name];
                    setSelection(cols.length ? { table: t.name, cols } : null);
                    applySelect(t.name, cols);
                    drag.current = null;
                  } else {
                    setSelection({ table: t.name, cols: [c.name] });
                    drag.current = { mode: "select", name: t.name, sx: 0, sy: 0, ox: 0, oy: 0 };
                  }
                }}
                onMouseEnter={() => { if (drag.current?.mode === "select" && selRef.current?.table === t.name && !selRef.current.cols.includes(c.name)) setSelection({ table: t.name, cols: [...selRef.current.cols, c.name] }); }}
              >
                <span class={"dot dot-" + familyOf(c.type)}></span>
                <span class="nc-name">{c.name}{c.pk ? <span class="pk">PK</span> : null}</span>
                <span class="nc-type">{c.type}</span>
              </div>
            ))}
          </div>
        ))}
      </div>
      {paths.length > 0 ? <div class="map-hint">Clique une relation (⋈) pour générer le JOIN</div> : null}
      <div class="zoom-controls">
        <button class="btn ghost" onClick={() => setZoom((z) => Math.max(0.4, +(z - 0.1).toFixed(2)))}>−</button>
        <span class="zlabel">{Math.round(zoom * 100)}%</span>
        <button class="btn ghost" onClick={() => setZoom((z) => Math.min(2, +(z + 0.1).toFixed(2)))}>+</button>
        <button class="btn ghost" onClick={() => { setZoom(1); setPan({ x: 24, y: 24 }); }}>recentrer</button>
      </div>
    </div>
  );
}

function DataView() {
  const ts = tables.value;
  const [count, setCount] = useState(8);
  if (ts.length === 0) return <div class="hint">Charge un schéma ou crée une table pour voir des données.</div>;
  const active = ts.find((t) => t.name === activeTable.value) ?? ts[0];
  return (
    <div class="data-view">
      <div class="table-tabs">
        {ts.map((t) => <button class={"tab" + (t.name === active.name ? " active" : "")} onClick={() => (activeTable.value = t.name)}>{t.name}</button>)}
      </div>
      <div class="data-toolbar">
        <button class="link-name" title={"SELECT * FROM " + active.name} onClick={() => selAll(active.name)}>⤵ {active.name}</button>
        <span class="muted">{active.rows.length} lignes</span>
        <div class="spacer" />
        <input class="num-input" type="number" min={1} max={500} value={count} onInput={(e) => setCount(parseInt((e.target as HTMLInputElement).value) || 1)} />
        <button class="btn primary" onClick={() => replaceTable(active.name, (t) => ({ ...t, rows: generateRows(t, count) }))}>Générer des données</button>
        <button class="btn ghost" onClick={() => { tables.value = ts.filter((t) => t.name !== active.name); activeTable.value = tables.value[0]?.name ?? null; }}>Supprimer</button>
      </div>
      <Grid columns={active.columns.map((c) => c.name)} rows={active.rows.map((r) => active.columns.map((c) => r[c.name] ?? null))} onColClick={(col) => selCol(col, active.name)} />
    </div>
  );
}

export function TablesPanel() {
  const [creating, setCreating] = useState(false);
  return (
    <section class="panel panel-left">
      <div class="panel-head">
        <strong class="panel-title">Tables</strong>
        <div class="left-tools">
          <button class="btn primary" onClick={() => setCreating(true)}>+ Table</button>
          <select class="btn select" onChange={(e) => { const v = (e.target as HTMLSelectElement).value; if (v) loadPreset(v); (e.target as HTMLSelectElement).value = ""; }}>
            <option value="">Charger un schéma…</option>
            {Object.entries(SCHEMAS).map(([k, s]) => <option value={k}>{s.label}</option>)}
          </select>
          <div class="segmented">
            <button class={"seg" + (leftView.value === "schema" ? " active" : "")} onClick={() => (leftView.value = "schema")}>Schéma</button>
            <button class={"seg" + (leftView.value === "data" ? " active" : "")} onClick={() => (leftView.value = "data")}>Données</button>
          </div>
        </div>
      </div>
      <div class="panel-body">{leftView.value === "schema" ? <SchemaCanvas /> : <DataView />}</div>
      {creating ? <TableCreator onClose={() => setCreating(false)} /> : null}
    </section>
  );
}
