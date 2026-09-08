// Panneau droit à ONGLETS :
//  • Éditeur  : saisie + APERÇU (table entière, réordonnée par ORDER BY, lignes gardées cadrées).
//               Bouton bascule "Afficher uniquement le résultat" = accordéon qui replie les
//               lignes non gardées (et redéplie au re-clic). JAMAIS de projection destructive.
//  • Pas à pas: VERROUILLÉ. Même mécanique, cumulée clause par clause (WHERE→ORDER→LIMIT→…).
//  • Exécution: couche PHYSIQUE (chemin d'accès, index, lignes lues, coût) — voir ExecTab.
//  Un DDL (CREATE / DROP INDEX) tapé dans l'éditeur est validé par le moteur et appliqué sur clic seulement.
import { sql, setSql, resultOnly, rightTab, stepSql, stepIndex, enterStep, enterExec, applyDdl, database, bindEditor, noteCursor } from "../state";
import { runQuery } from "../engine";
import { analyze } from "../analyze";
import { computeJoin } from "../join";
import { isDdlResult, isDdlText } from "../exec";
import type { ClauseKey } from "../clauses";
import { Grid } from "./Grid";
import { JoinPreview } from "./JoinView";
import { JoinStep } from "./JoinStep";
import { ExecTab } from "./ExecTab";

const JOIN_SNIPPETS: Record<string, string> = {
  JOIN: " JOIN  ON ", "LEFT JOIN": " LEFT JOIN  ON ", "RIGHT JOIN": " RIGHT JOIN  ON ",
  "FULL JOIN": " FULL JOIN  ON ", "CROSS JOIN": " CROSS JOIN ",
};
const INDEX_SNIPPETS: Record<string, string> = {
  "CREATE INDEX": "CREATE INDEX idx_table_colonne ON table (colonne)",
  "CREATE UNIQUE INDEX": "CREATE UNIQUE INDEX idx_table_colonne ON table (colonne)",
  "DROP INDEX": "DROP INDEX idx_table_colonne",
};
const OP_SNIPPETS: Record<string, string> = {
  "=": " = ", "<>": " <> ", "<": " < ", "<=": " <= ", ">": " > ", ">=": " >= ",
  AND: " AND ", OR: " OR ", NOT: " NOT ", IN: " IN ()", "NOT IN": " NOT IN ()",
  LIKE: " LIKE '%'", BETWEEN: " BETWEEN  AND ", "IS NULL": " IS NULL", "IS NOT NULL": " IS NOT NULL",
};
const range = (n: number) => Array.from({ length: n }, (_, i) => i);
const CLAUSE_LABEL: Record<ClauseKey, string> = {
  from: "FROM", where: "WHERE", select: "SELECT", orderby: "ORDER BY", limit: "LIMIT", offset: "OFFSET",
};

/* ---------------- Onglet Éditeur : aperçu + accordéon résultat ---------------- */
function PreviewView() {
  const a = analyze(sql.value, database.value);
  if (!a.ok) return <div class="placeholder">{a.note}</div>;
  const collapsed = resultOnly.value;
  const display = a.allOrderedIdx; // ordre = ORDER BY (ou identité)
  const rows = display.map((i) => a.sourceRows[i]);
  const sel = a.selectedCols;
  const hiCol = (name: string) => Array.isArray(sel) && sel.includes(name);
  // en mode replié : les colonnes NON sélectionnées se replient (projection).
  const foldCol = collapsed ? (name: string) => Array.isArray(sel) && !sel.includes(name) : undefined;

  const rowClass = (p: number) => {
    const orig = display[p];
    if (collapsed) return a.keptSet.has(orig) ? "" : "folded"; // accordéon : replie les non-gardées
    if (a.passSet && !a.passSet.has(orig)) return "row-fail"; // rejetée par WHERE
    if (a.framingActive && !a.keptSet.has(orig)) return "row-cut"; // coupée par LIMIT/OFFSET
    if (a.framingActive) return "row-pass"; // gardée
    return "";
  };

  const nbCols = Array.isArray(sel) ? sel.length : a.columns.length;
  const note = collapsed
    ? `${a.keptOrdered.length} ligne(s) × ${nbCols} colonne(s) — résultat`
    : a.framingActive
      ? `${a.keptSet.size}/${a.sourceRows.length} ligne(s) gardée(s)`
      : `${a.sourceRows.length} ligne(s)`;

  return (
    <div class="preview">
      <div class="preview-bar">
        <span class={"mode-tag" + (collapsed ? " applied" : "")}>{collapsed ? "résultat" : "aperçu"}</span> {note}
      </div>
      <Grid columns={a.columns} rows={rows} rowClass={rowClass} highlightCol={hiCol} foldCol={foldCol} />
    </div>
  );
}

/* Carte DDL : le moteur valide (table, colonnes, nom, unicité) ; l'app applique sur clic. */
function DdlCard() {
  const r = runQuery(sql.value, database.value, { plan: false });
  if (!r.ok) return <div class="card ddl"><div class="card-title">DDL <span class="tag model">index</span></div><div class="error-box"><span class="err-tag">erreur</span>{r.error ?? "instruction incomplète"}</div></div>;
  if (!isDdlResult(r) || !r.ddl) return <div class="placeholder">instruction non reconnue</div>;
  const d = r.ddl;
  const apply = () => {
    applyDdl(d);
    if (d.op === "create_index") {
      // on enchaîne sur une requête qui exploite l'index, dans l'onglet Exécution : « crée, puis regarde »
      const t = database.value.tables.find((x) => x.name.toLowerCase() === d.table.toLowerCase());
      const col = d.index.columns[0];
      const v = t?.rows.find((row) => row[col] !== null && row[col] !== undefined)?.[col];
      const lit = v === null || v === undefined ? "NULL" : typeof v === "string" ? `'${v}'` : String(v);
      setSql(`SELECT * FROM ${d.table} WHERE ${col} = ${lit}`);
      enterExec();
    } else setSql("");
  };
  return (
    <div class="card ddl">
      <div class="card-title">{d.op === "create_index" ? "Créer un index" : "Supprimer un index"} <span class="tag model">DDL</span></div>
      <code class="ddl-summary">{d.summary}</code>
      <p class="muted small">
        {d.op === "create_index"
          ? `Un index B-tree sur ${d.table}(${d.index.columns.join(", ")}) : une copie triée des clés → n° de ligne. Il ne change aucun résultat ; il change le chemin d'accès (et le coût des écritures).`
          : "L'index disparaît : les requêtes qui l'utilisaient redeviennent des parcours complets (Seq Scan)."}
      </p>
      <div class="eval-actions">
        <button class="btn primary" onClick={apply}>{d.op === "create_index" ? "Créer l'index" : "Supprimer l'index"}</button>
        <span class="muted small">appliqué sur clic seulement, jamais à la frappe</span>
      </div>
    </div>
  );
}

function EditorResults() {
  if (isDdlText(sql.value)) return <DdlCard />;
  const jd = computeJoin(sql.value, database.value);
  if (jd) return <JoinPreview data={jd} />;
  return <PreviewView />;
}

function EditorTab() {
  return (
    <>
      <div class="constructs bar">
        <button class="chip" onClick={() => setSql(sql.value + "SELECT ")}>SELECT</button>
        <button class="chip" onClick={() => setSql(sql.value + " DISTINCT ")}>DISTINCT</button>
        <button class="chip" onClick={() => setSql(sql.value + " FROM ")}>FROM</button>
        <select class="chip chip-sel" onChange={(e) => { const el = e.target as HTMLSelectElement; if (el.value) setSql(sql.value + JOIN_SNIPPETS[el.value]); el.value = ""; }}>
          <option value="">JOIN ▾</option>
          {Object.keys(JOIN_SNIPPETS).map((k) => <option value={k}>{k}</option>)}
        </select>
        <button class="chip" onClick={() => setSql(sql.value + " WHERE ")}>WHERE</button>
        <select class="chip chip-sel" onChange={(e) => { const el = e.target as HTMLSelectElement; if (el.value) setSql(sql.value + OP_SNIPPETS[el.value]); el.value = ""; }}>
          <option value="">op ▾</option>
          {Object.keys(OP_SNIPPETS).map((k) => <option value={k}>{k}</option>)}
        </select>
        <button class="chip" onClick={() => setSql(sql.value + " ORDER BY ")}>ORDER BY</button>
        <button class="chip" onClick={() => setSql(sql.value + " LIMIT 10")}>LIMIT</button>
        <button class="chip" onClick={() => setSql(sql.value + " OFFSET 0")}>OFFSET</button>
        <select class="chip chip-sel" title="DDL : déclarer un index (appliqué sur clic)" onChange={(e) => { const el = e.target as HTMLSelectElement; if (el.value) setSql(INDEX_SNIPPETS[el.value]); el.value = ""; }}>
          <option value="">INDEX ▾</option>
          {Object.keys(INDEX_SNIPPETS).map((k) => <option value={k}>{k}</option>)}
        </select>
      </div>
      <textarea
        class="sql-input"
        spellcheck={false}
        ref={bindEditor}
        value={sql.value}
        placeholder="SELECT nom, ville FROM clients WHERE ville = 'Paris' ORDER BY nom LIMIT 5"
        onInput={(e) => { setSql((e.target as HTMLTextAreaElement).value); noteCursor(); }}
        onKeyUp={noteCursor}
        onClick={noteCursor}
      />
      <div class="eval-actions">
        <button class="btn primary" onClick={() => (resultOnly.value = !resultOnly.value)}>
          {resultOnly.value ? "⤢ Déplier l'aperçu" : "⤡ Afficher uniquement le résultat"}
        </button>
        <button class="btn" onClick={enterStep} title="Évaluer pas à pas (verrouillé)">Pas à pas →</button>
        <button class="btn" onClick={enterExec} title="Voir le chemin d'accès, les index, les lignes lues et le coût">Exécution →</button>
      </div>
      <div class="result-area"><EditorResults /></div>
    </>
  );
}

/* ---------------- Onglet Pas à pas (verrouillé) ---------------- */
function StepTab() {
  const text = stepSql.value;
  const jd = computeJoin(text, database.value);
  if (jd) return <JoinStep data={jd} />; // pas-à-pas dédié aux jointures (nested-loop)
  const a = analyze(text, database.value);
  if (!a.ok) return <div class="placeholder">{a.note}</div>;
  const steps = a.seg.evalOrder;
  const nb = steps.length;
  if (nb === 0) return <div class="placeholder">requête vide</div>;
  const idx = Math.min(stepIndex.value, nb - 1);
  const set = (i: number) => (stepIndex.value = Math.max(0, Math.min(nb - 1, i)));
  const current = steps[idx];
  const active = new Set<ClauseKey>(steps.slice(0, idx + 1));

  const n = a.sourceRows.length;
  const ordered = active.has("orderby") ? a.allOrderedIdx : range(n);
  const rowFrame = active.has("where") && a.passSet != null;
  const limitActive = active.has("limit") && a.hasLimit;
  const colFrame = active.has("select");
  const rows = ordered.map((i) => a.sourceRows[i]);

  const rowClass = (p: number) => {
    const orig = ordered[p];
    if (rowFrame && !a.passSet!.has(orig)) return "row-fail";
    if (limitActive && !a.keptSet.has(orig)) return "row-cut";
    if (rowFrame || limitActive) return "row-pass";
    return "";
  };
  const sel = a.selectedCols;
  const hiCol = (name: string) => colFrame && Array.isArray(sel) && sel.includes(name);

  let bStart = Infinity, bEnd = -Infinity;
  active.forEach((k) => { const p = a.seg.byKey[k]; if (p) { bStart = Math.min(bStart, p.start); bEnd = Math.max(bEnd, p.end); } });
  const hasBracket = bStart < bEnd;
  let realEnd = bEnd;
  while (realEnd > bStart && /\s/.test(text[realEnd - 1])) realEnd--;

  const note = (() => {
    switch (current) {
      case "from": return `lecture de « ${a.seg.tableName} » — ${n} ligne(s)`;
      case "where": return a.passSet ? `${a.passSet.size}/${n} ligne(s) satisfont le WHERE` : "WHERE incomplet";
      case "select": return "colonnes du SELECT cadrées (on ne projette pas)";
      case "orderby": return "lignes réordonnées par ORDER BY";
      case "limit": return `${a.keptSet.size} ligne(s) gardée(s) — les suivantes coupées`;
      case "offset": return `décalage OFFSET appliqué`;
    }
  })();

  return (
    <>
      <div class="lock-bar">
        <button class="btn ghost small" onClick={() => (rightTab.value = "editor")}>← revenir à l'éditeur</button>
      </div>
      <div class="proof-query">
        {hasBracket ? (
          <>
            <span class="q-dim">{text.slice(0, bStart)}</span>
            <span class="q-bracket">[{text.slice(bStart, realEnd)}]</span>
            <span class="q-dim">{text.slice(realEnd)}</span>
          </>
        ) : (<span class="q-dim">{text}</span>)}
      </div>
      <div class="step-controls">
        <button class="btn ghost" onClick={() => set(0)} title="Début">⏮</button>
        <button class="btn ghost" onClick={() => set(idx - 1)} disabled={idx === 0}>◀</button>
        <button class="btn primary" onClick={() => set(idx + 1)} disabled={idx === nb - 1}>avancer ▶</button>
        <button class="btn ghost" onClick={() => set(nb - 1)} title="Fin">⏭</button>
        <span class="step-label">étape {idx + 1}/{nb} · <b>{CLAUSE_LABEL[current]}</b> — {note}</span>
      </div>
      <div class="result-area">
        <Grid columns={a.columns} rows={rows} rowClass={rowClass} highlightCol={hiCol} />
      </div>
    </>
  );
}

export function Evaluator() {
  const tab = rightTab.value;
  return (
    <section class="panel panel-right">
      <div class="panel-head">
        <strong class="panel-title">Évaluateur</strong>
        <div class="rtabs">
          <button class={"seg" + (tab === "editor" ? " active" : "")} onClick={() => (rightTab.value = "editor")}>Éditeur</button>
          <button class={"seg" + (tab === "step" ? " active" : "")} onClick={enterStep}>Pas à pas</button>
          <button class={"seg" + (tab === "exec" ? " active" : "")} onClick={enterExec} title="couche physique : index, chemin d'accès, coût">Exécution</button>
        </div>
      </div>
      {tab === "editor" ? <EditorTab /> : tab === "step" ? <StepTab /> : <ExecTab />}
    </section>
  );
}
