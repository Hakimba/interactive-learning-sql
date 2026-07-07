// Panneau gauche : sandbox des tables. Deux vues (Schéma / Données),
// chargement de schémas préchargés, génération de données, création de table.
import { useState } from "preact/hooks";
import { tables, activeTable, leftView, insertIdent } from "../state";
import { TYPES, SCHEMAS, cloneSchema, familyOf } from "../catalog";
import { generateRows } from "../datagen";
import type { Column, Table } from "../types";
import { Grid } from "./Grid";

function replaceTable(name: string, updater: (t: Table) => Table) {
  tables.value = tables.value.map((t) => (t.name === name ? updater(t) : t));
}

function loadPreset(key: string) {
  const s = cloneSchema(key);
  if (!s) return;
  s.tables.forEach((t) => (t.rows = generateRows(t, 8)));
  tables.value = s.tables;
  activeTable.value = s.tables[0]?.name ?? null;
}

function TableCreator({ onClose }: { onClose: () => void }) {
  const [name, setName] = useState("ma_table");
  const [cols, setCols] = useState<Column[]>([
    { name: "id", type: "INTEGER", pk: true },
    { name: "libelle", type: "VARCHAR" },
  ]);
  const [err, setErr] = useState("");

  const setCol = (i: number, patch: Partial<Column>) =>
    setCols(cols.map((c, k) => (k === i ? { ...c, ...patch } : c)));

  function save() {
    const nm = name.trim();
    if (!nm) return setErr("Nomme la table.");
    if (tables.value.some((t) => t.name.toLowerCase() === nm.toLowerCase())) return setErr("Ce nom existe déjà.");
    const clean = cols.filter((c) => c.name.trim());
    if (clean.length === 0) return setErr("Ajoute au moins une colonne.");
    const t: Table = { name: nm, columns: clean, rows: [] };
    t.rows = generateRows(t, 8);
    tables.value = [...tables.value, t];
    activeTable.value = nm;
    onClose();
  }

  return (
    <div class="modal-backdrop" onClick={onClose}>
      <div class="modal" onClick={(e) => e.stopPropagation()}>
        <div class="modal-head">
          <strong>Nouvelle table</strong>
          <button class="btn ghost" onClick={onClose}>✕</button>
        </div>
        <label class="field">
          <span>Nom</span>
          <input value={name} onInput={(e) => setName((e.target as HTMLInputElement).value)} />
        </label>
        <div class="cols-editor">
          {cols.map((c, i) => (
            <div class="col-row">
              <input
                class="c-name"
                value={c.name}
                placeholder="colonne"
                onInput={(e) => setCol(i, { name: (e.target as HTMLInputElement).value })}
              />
              <select
                class="c-type"
                value={c.type}
                onChange={(e) => setCol(i, { type: (e.target as HTMLSelectElement).value })}
              >
                {TYPES.map((t) => (
                  <option value={t.name}>{t.name}</option>
                ))}
              </select>
              <label class="c-pk">
                <input type="checkbox" checked={!!c.pk} onChange={(e) => setCol(i, { pk: (e.target as HTMLInputElement).checked })} /> PK
              </label>
              <button class="btn ghost small" onClick={() => setCols(cols.filter((_, k) => k !== i))}>✕</button>
            </div>
          ))}
          <button class="btn ghost small" onClick={() => setCols([...cols, { name: "", type: "VARCHAR" }])}>+ colonne</button>
        </div>
        {err ? <div class="modal-error">{err}</div> : null}
        <div class="modal-foot">
          <button class="btn ghost" onClick={onClose}>Annuler</button>
          <button class="btn primary" onClick={save}>Créer</button>
        </div>
      </div>
    </div>
  );
}

function SchemaView() {
  const ts = tables.value;
  if (ts.length === 0) return <div class="hint">Charge un schéma ou crée une table.</div>;
  return (
    <div class="cards">
      {ts.map((t) => (
        <div class="table-card">
          <div class="tc-head">
            <button class="link-name" title="insérer le nom de la table" onClick={() => insertIdent(t.name)}>{t.name}</button>
            <span class="tc-count">{t.rows.length} lignes</span>
          </div>
          <div class="tc-cols">
            {t.columns.map((c) => (
              <div class="tc-col clickable" title="insérer cette colonne" onClick={() => insertIdent(c.name)}>
                <span class={"dot dot-" + familyOf(c.type)}></span>
                <span class="tc-name">{c.name}{c.pk ? <span class="pk">PK</span> : null}</span>
                <span class="tc-type">{c.type}</span>
              </div>
            ))}
          </div>
        </div>
      ))}
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
        {ts.map((t) => (
          <button class={"tab" + (t.name === active.name ? " active" : "")} onClick={() => (activeTable.value = t.name)}>
            {t.name}
          </button>
        ))}
      </div>
      <div class="data-toolbar">
        <button class="link-name" title="insérer le nom de la table" onClick={() => insertIdent(active.name)}>⤵ {active.name}</button>
        <span class="muted">{active.rows.length} lignes</span>
        <div class="spacer" />
        <input
          class="num-input"
          type="number"
          min={1}
          max={500}
          value={count}
          onInput={(e) => setCount(parseInt((e.target as HTMLInputElement).value) || 1)}
        />
        <button class="btn primary" onClick={() => replaceTable(active.name, (t) => ({ ...t, rows: generateRows(t, count) }))}>
          Générer des données
        </button>
        <button class="btn ghost" onClick={() => { tables.value = ts.filter((t) => t.name !== active.name); activeTable.value = tables.value[0]?.name ?? null; }}>
          Supprimer
        </button>
      </div>
      <Grid
        columns={active.columns.map((c) => c.name)}
        rows={active.rows.map((r) => active.columns.map((c) => r[c.name] ?? null))}
        onColClick={insertIdent}
      />
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
            {Object.entries(SCHEMAS).map(([k, s]) => (
              <option value={k}>{s.label}</option>
            ))}
          </select>
          <div class="segmented">
            <button class={"seg" + (leftView.value === "schema" ? " active" : "")} onClick={() => (leftView.value = "schema")}>Schéma</button>
            <button class={"seg" + (leftView.value === "data" ? " active" : "")} onClick={() => (leftView.value = "data")}>Données</button>
          </div>
        </div>
      </div>
      <div class="panel-body">{leftView.value === "schema" ? <SchemaView /> : <DataView />}</div>
      {creating ? <TableCreator onClose={() => setCreating(false)} /> : null}
    </section>
  );
}
