// Déclaration d'index côté UI (l'autre voie est le DDL dans l'éditeur : CREATE INDEX … ON … (…)).
// IndexForm = formulaire (colonnes ordonnées par clics, UNIQUE, nom auto) ; IndexEditor = modale listant les index
// d'une table avec interrupteur actif/inactif (« résultat identique, exécution différente ») et suppression.
import { useState } from "preact/hooks";
import { addIndex, dropIndex, toggleIndex, effectiveIndexes, allIndexNames, tables } from "../state";
import type { IndexDef, Table } from "../types";

export function autoName(table: string, cols: string[]) { return cols.length ? `idx_${table}_${cols.join("_")}`.slice(0, 48) : ""; }

export function IndexForm({ table, onSave, onCancel }: { table: Table; onSave: (d: IndexDef) => void; onCancel?: () => void }) {
  const [cols, setCols] = useState<string[]>([]);
  const [unique, setUnique] = useState(false);
  const [name, setName] = useState("");
  const [touched, setTouched] = useState(false);
  const [err, setErr] = useState("");
  const shownName = touched ? name : autoName(table.name, cols);
  const toggle = (c: string) => setCols(cols.includes(c) ? cols.filter((x) => x !== c) : [...cols, c]);
  function save() {
    const nm = shownName.trim();
    if (!cols.length) return setErr("Choisis au moins une colonne (l'ordre compte : règle du préfixe gauche).");
    if (!nm) return setErr("Nomme l'index.");
    if (allIndexNames().some((x) => x.toLowerCase() === nm.toLowerCase())) return setErr(`« ${nm} » existe déjà (les noms d'index sont uniques dans toute la base).`);
    if (tables.value.some((t) => t.name.toLowerCase() === nm.toLowerCase())) return setErr(`« ${nm} » est déjà le nom d'une table.`);
    onSave({ name: nm, columns: cols, unique, enabled: true });
    setCols([]); setUnique(false); setName(""); setTouched(false); setErr("");
  }
  return (
    <div class="index-form">
      <div class="if-cols">
        <span class="muted">colonnes (dans l'ordre du clic) :</span>
        {table.columns.map((c) => {
          const k = cols.indexOf(c.name);
          return <button class={"chip" + (k >= 0 ? " on" : "")} onClick={() => toggle(c.name)}>{k >= 0 ? <b>{k + 1}·</b> : null}{c.name}</button>;
        })}
      </div>
      <div class="if-row">
        <input class="c-name" value={shownName} placeholder="nom de l'index" onInput={(e) => { setTouched(true); setName((e.target as HTMLInputElement).value); }} />
        <label class="c-pk"><input type="checkbox" checked={unique} onChange={(e) => setUnique((e.target as HTMLInputElement).checked)} /> UNIQUE</label>
        <span class="muted mono">{cols.length ? `CREATE ${unique ? "UNIQUE " : ""}INDEX ${shownName} ON ${table.name} (${cols.join(", ")})` : "…"}</span>
        <div class="spacer" />
        {onCancel ? <button class="btn ghost small" onClick={onCancel}>Annuler</button> : null}
        <button class="btn primary small" onClick={save}>Créer l'index</button>
      </div>
      {err ? <div class="modal-error">{err}</div> : null}
    </div>
  );
}

export function IndexEditor({ table, onClose }: { table: Table; onClose: () => void }) {
  const t = tables.value.find((x) => x.name === table.name) ?? table;
  const list = effectiveIndexes(t);
  return (
    <div class="modal-backdrop" onClick={onClose}>
      <div class="modal" onClick={(e) => e.stopPropagation()}>
        <div class="modal-head"><strong>Index de « {t.name} »</strong><button class="btn ghost" onClick={onClose}>✕</button></div>
        <p class="modal-note">
          Un index ne change <b>jamais</b> le résultat d'une requête : il change le <b>chemin d'accès</b> (quelles lignes sont lues, dans quel ordre, à quel coût).
          Désactive-le avec ● / ○ et compare dans l'onglet <b>Exécution</b>. La clé primaire est un index unique implicite (<code>{t.name}_pkey</code>).
        </p>
        <div class="index-list">
          {list.length === 0 ? <div class="hint">Aucun index sur cette table.</div> : null}
          {list.map((i) => (
            <div class={"index-row" + (i.enabled ? "" : " off")}>
              <button class={"idx-toggle" + (i.enabled ? " on" : "")} title={i.enabled ? "désactiver (l'index existe mais le planificateur l'ignore)" : "activer"} onClick={() => toggleIndex(t.name, i.name)}>{i.enabled ? "●" : "○"}</button>
              <span class="idx-name">{i.name}</span>
              <span class="muted mono">({i.columns.join(", ")})</span>
              {i.unique ? <span class="idx-badge">UNIQUE</span> : null}
              {i.implicit ? <span class="idx-badge">PK</span> : null}
              <div class="spacer" />
              {i.implicit ? <span class="muted">décoche PK pour le retirer</span> : <button class="btn ghost small danger" onClick={() => dropIndex(t.name, i.name)}>✕ supprimer</button>}
            </div>
          ))}
        </div>
        <h4 class="if-title">Nouvel index</h4>
        <IndexForm table={t} onSave={(d) => addIndex(t.name, d)} />
        <div class="modal-foot"><button class="btn" onClick={onClose}>Fermer</button></div>
      </div>
    </div>
  );
}
