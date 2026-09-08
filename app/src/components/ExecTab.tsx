// Onglet « Exécution » : la couche PHYSIQUE de la requête courante — ce que le résultat ne montre pas.
//   • bandeau « résultat identique, exécution différente » (moteur exécuté sans index vs avec) ;
//   • chemins d'accès candidats (Seq Scan / Index Scan / Index Only Scan) avec coût ESTIMÉ (modèle System R,
//     constantes PostgreSQL) et compteurs EXACTS ; possibilité de FORCER un chemin (≈ enable_seqscan) ;
//   • mini-stepper : chemin → descente d'index → lecture du tas → filtre → tri | déjà trié → LIMIT → résultat ;
//   • « pourquoi pas cet index ? » : raison par conjoint (sargabilité, préfixe gauche…).
import { sql, database, rightTab, execStep, execForce, tables, toggleIndex } from "../state";
import { runQuery } from "../engine";
import { generateRows } from "../datagen";
import { bagEqual, fmtCost, fmtKey, isDdlText, pathLabel, reasonText, stepsOf, withoutIndexes } from "../exec";
import type { Plan, PlanPath, Table, Val } from "../types";
import { Grid } from "./Grid";
import { IndexView } from "./IndexView";
import { HeapMap } from "./HeapMap";

type Avail = Extract<Plan, { available: true }>;

function Back() {
  return <div class="lock-bar"><button class="btn ghost small" onClick={() => (rightTab.value = "editor")}>← revenir à l'éditeur</button><span>exécution physique de la requête courante</span></div>;
}

function CostCard({ plan, path }: { plan: Avail; path: PlanPath }) {
  const c = plan.consts;
  return (
    <div class="card">
      <div class="card-title">Coût estimé de <b>{pathLabel(path)}</b> <span class="tag model">modèle</span></div>
      <ul class="formula">{path.est.formula.map((l) => <li>{l}</li>)}</ul>
      <div class="muted small">
        Forme System R (Selinger 1979) : coût = accès pages + CPU. Constantes PostgreSQL : seq_page_cost {c.seq_page_cost} · random_page_cost {c.random_page_cost} ·
        cpu_tuple_cost {c.cpu_tuple_cost} · cpu_index_tuple_cost {c.cpu_index_tuple_cost} · cpu_operator_cost {c.cpu_operator_cost}.
        Paramètres du modèle : {c.rows_per_page} lignes/page, fan-out {c.fanout}. Lignes estimées : {path.est.rows} (sélectivité Selinger) — réelles : {plan.exec.passed}.
      </div>
    </div>
  );
}

function PathsTable({ plan, hasOrder }: { plan: Avail; hasOrder: boolean }) {
  return (
    <div class="grid-wrap">
      <table class="grid paths">
        <thead><tr><th></th><th>chemin</th><th>lignes est.</th><th>accès</th><th>tri</th><th>total</th><th></th></tr></thead>
        <tbody>
          {plan.paths.map((p, i) => (
            <tr class={i === plan.chosen ? "row-pass" : ""}>
              <td>{i === plan.chosen ? (plan.forced ? "⚑" : "✓") : ""}</td>
              <td>{pathLabel(p)}{p.hasCond ? "" : p.kind !== "seq_scan" ? <span class="muted"> (parcours complet, pour l'ordre)</span> : null}</td>
              <td class="num">{p.est.rows}</td>
              <td class="num">{fmtCost(p.est.accessCost)}</td>
              <td>{hasOrder ? (p.sortNeeded ? <span class="tag warn">tri {fmtCost(p.est.sortCost)}</span> : <span class="tag ok">déjà trié{p.orderProvided === "backward" ? " (arrière)" : ""}</span>) : <span class="muted">—</span>}</td>
              <td class="num"><b>{fmtCost(p.est.total)}</b></td>
              <td>{i !== plan.chosen ? <button class="btn ghost small" title="forcer ce chemin (≈ enable_seqscan / enable_indexscan)" onClick={() => { execForce.value = p.kind === "seq_scan" ? "seq_scan" : p.index; }}>forcer</button>
                : plan.forced ? <button class="btn ghost small" onClick={() => (execForce.value = null)}>choix auto</button> : <span class="muted">choisi (coût min.)</span>}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function FilterCard({ plan, path }: { plan: Avail; path: PlanPath }) {
  const role = (i: number) => {
    if (path.indexCond.includes(i)) return { cls: "ok", txt: "borne le parcours d'index" };
    if (path.indexCheck.includes(i)) return { cls: "mid", txt: "vérifié dans l'index (ne borne pas)" };
    return { cls: path.kind === "seq_scan" ? "mid" : "warn", txt: "vérifié sur chaque ligne lue (résiduel)" };
  };
  const why = (i: number) => path.notApplicable.find((x) => x.conjunct === i);
  return (
    <div class="card">
      <div class="card-title">Filtre WHERE : {plan.conjuncts.length} conjoint(s) · {plan.exec.passed}/{plan.exec.candidates} ligne(s) lue(s) satisfont la condition</div>
      <ul class="conj-list">
        {plan.conjuncts.map((c, i) => { const r = role(i); const w = why(i); return (
          <li>
            <code>{c.text}</code> <span class={"tag " + r.cls}>{r.txt}</span>
            <span class="muted small"> · sélectivité estimée {Math.round(c.sel * 1000) / 10} %</span>
            {w && path.kind !== "seq_scan" ? <div class="muted small why">↳ {reasonText(w.reason)}</div> : null}
            {!c.sargable && c.reason && path.kind === "seq_scan" ? <div class="muted small why">↳ non sargable : {reasonText(c.reason)}</div> : null}
          </li>); })}
      </ul>
      <div class="muted small">Quoi qu'en dise l'index, la sémantique ré-évalue <b>toute</b> la condition WHERE sur chaque candidat (logique à 3 valeurs) : la correction ne dépend pas de cette décomposition.</div>
    </div>
  );
}

function WhyNot({ plan }: { plan: Avail }) {
  const chosen = plan.paths[plan.chosen];
  const others = plan.reports.filter((r) => r.index !== chosen.index);
  if (!others.length) return null;
  return (
    <div class="card">
      <div class="card-title">Pourquoi pas cet index ?</div>
      {others.map((r) => {
        const p = plan.paths.find((x) => x.index === r.index);
        return (
          <div class="whynot">
            <b>{r.index}</b>{" "}
            {p ? <span class="muted">— utilisable (coût {fmtCost(p.est.total)} vs {fmtCost(chosen.est.total)} pour le chemin choisi){p.hasCond ? "" : " : il fournit l'ordre mais ne borne rien"}</span>
               : <span class="muted">— inutilisable pour cette requête</span>}
            <ul class="conj-list small">
              {r.notApplicable.map((na) => <li><code>{plan.conjuncts[na.conjunct]?.text}</code> : {reasonText(na.reason)}</li>)}
              {r.indexCond.map((i) => <li><code>{plan.conjuncts[i]?.text}</code> : <span class="tag ok">bornerait le parcours</span></li>)}
              {!plan.conjuncts.length ? <li class="muted">pas de WHERE : un index ne peut servir que pour l'ordre</li> : null}
            </ul>
          </div>
        );
      })}
    </div>
  );
}

export function ExecTab() {
  const text = sql.value;
  const db = database.value;
  if (!text.trim()) return <><Back /><div class="placeholder">Écris une requête dans l'éditeur, puis reviens ici pour voir comment elle s'exécute.</div></>;
  if (isDdlText(text)) return <><Back /><div class="placeholder">Un DDL (CREATE / DROP INDEX) s'applique depuis l'éditeur ; ici on observe l'exécution d'un SELECT.</div></>;
  const r = runQuery(text, db, { force: execForce.value });
  if (!r.ok) return <><Back /><div class="error-box"><span class="err-tag">erreur</span>{r.error}</div></>;
  const plan = r.plan;
  if (!plan || !plan.available) return <><Back /><div class="placeholder">Couche physique indisponible : {plan?.reason ?? "—"}.</div></>;

  const table: Table | undefined = tables.value.find((t) => t.name.toLowerCase() === plan.table.toLowerCase());
  const dbTable = db.tables.find((t) => t.name.toLowerCase() === plan.table.toLowerCase());
  const path = plan.paths[plan.chosen];
  const ex = plan.exec;
  const noIdx = runQuery(text, withoutIndexes(db), { plan: false });
  const same = noIdx.ok && bagEqual(noIdx.rows, r.rows);
  const hasOrder = /\border\s+by\b/i.test(text), hasLimit = /\b(limit|offset)\b/i.test(text);
  const steps = stepsOf(plan, hasOrder, hasLimit);
  const idx = Math.min(execStep.value, steps.length - 1);
  const set = (i: number) => (execStep.value = Math.max(0, Math.min(steps.length - 1, i)));
  const cur = steps[idx];

  const touched = new Set(ex.touched), stream = new Set(ex.stream);
  const kept = new Set<number>();
  const whereStage = plan.physPipeline.find((s) => s.kind === "where");
  if (whereStage && whereStage.whereRows) whereStage.whereRows.forEach((w) => { if (w.pass) kept.add(ex.stream[w.i]); });
  else ex.stream.forEach((id) => kept.add(id));
  const n = dbTable?.rows.length ?? 0;
  const usedIndex = ex.index ? plan.indexes.find((i) => i.name === ex.index) : undefined;
  const indexes = plan.indexes;

  const gen120 = () => { if (table) tables.value = tables.value.map((t) => (t.name === table.name ? { ...t, rows: generateRows(t, 120) } : t)); };

  const stepNote = (() => {
    switch (cur.kind) {
      case "path": return `${plan.paths.length} chemin(s) candidat(s) — ${plan.forced ? "chemin FORCÉ" : "le moins coûteux est choisi"} : ${pathLabel(path)}`;
      case "descent": return `${ex.descents.length} sonde(s) · ${ex.entriesScanned} entrée(s) d'index lue(s) · ${ex.indexPages} page(s) d'index`;
      case "heap": return path.kind === "index_only_scan" ? `0 ligne du tas lue : tout vient de l'index (couvrant)` : `${ex.touched.length}/${n} ligne(s) lue(s) · ${ex.heapPages.length} page(s) du tas`;
      case "filter": return `${ex.passed}/${ex.candidates} ligne(s) lue(s) passent le WHERE`;
      case "sort": return path.sortNeeded ? "les lignes gardées sont triées (étape Sort)" : `aucun tri : l'index livre déjà l'ordre demandé (${path.orderProvided === "backward" ? "parcours arrière" : "parcours avant"})`;
      case "limit": return ex.earlyStop ? `arrêt anticipé : on cesse de lire dès que LIMIT est satisfait (${ex.candidates} ligne(s) lue(s))` : "LIMIT appliqué après lecture complète (tri ou DISTINCT l'imposent)";
      case "result": return `${ex.returned} ligne(s) renvoyée(s)`;
    }
  })();

  const srcCols = dbTable ? dbTable.columns.map((c) => c.name) : [];
  const srcRows: Val[][] = dbTable ? dbTable.rows.map((row) => srcCols.map((c) => row[c] ?? null)) : [];
  const rowClass = (i: number) => (!stream.has(i) ? "row-skipped" : kept.has(i) ? "row-pass" : "row-read");
  const rowPrefix = (i: number) => <span class="gut" title={touched.has(i) ? "lue dans le tas" : stream.has(i) ? "servie par l'index" : "jamais lue"}>{touched.has(i) ? "●" : stream.has(i) ? "◐" : "○"}</span>;

  return (
    <>
      <Back />
      <div class="proof-query"><span class="q-dim">{text}</span></div>

      <div class={"banner " + (same ? "ok" : plan.sound.tieAmbiguity ? "warn" : plan.sound.sound ? "ok" : "bad")}>
        {same ? <><b>Résultat identique</b> avec ou sans index ({r.rows.length} ligne(s)). Seul le <b>travail</b> change : {ex.candidates} ligne(s) lue(s) ici, contre {n} en Seq Scan.</>
          : plan.sound.tieAmbiguity ? <><b>Résultat équivalent</b> : mêmes clés de tri, mais des <b>ex æquo</b> différents — SQL ne fixe pas l'ordre des ex æquo, donc LIMIT dépend du chemin d'accès. Leçon : sans ORDER BY total, LIMIT n'est pas déterministe.</>
          : plan.sound.sound ? <><b>Résultat identique</b> (vérifié par le moteur).</>
          : <><b>Incohérence interne</b> : le chemin physique ne reproduit pas le résultat sémantique — c'est un bug à signaler.</>}
        {plan.warnings.length ? <div class="muted small">{plan.warnings.join(" · ")}</div> : null}
      </div>

      <div class="exec-tools">
        <span class="muted">index de {plan.table} :</span>
        {indexes.length === 0 ? <span class="muted">aucun (déclare-en un dans la fiche table ou par CREATE INDEX)</span> : null}
        {indexes.map((i) => (
          <button class={"chip idx-chip" + (i.enabled ? " on" : "")} title={i.enabled ? "désactiver" : "activer"} onClick={() => toggleIndex(plan.table, i.name)}>
            {i.enabled ? "●" : "○"} {i.name} <span class="muted">({i.columns.join(", ")})</span>
          </button>
        ))}
        <div class="spacer" />
        {n < 60 && table ? <button class="btn ghost small" title="Sur 8 lignes, tout tient en 2 pages : le Seq Scan gagne toujours. Avec plus de lignes, l'index devient rentable." onClick={gen120}>Générer 120 lignes</button> : null}
      </div>

      <div class="step-controls">
        <button class="btn ghost" onClick={() => set(0)} title="Début">⏮</button>
        <button class="btn ghost" onClick={() => set(idx - 1)} disabled={idx === 0}>◀</button>
        <button class="btn primary" onClick={() => set(idx + 1)} disabled={idx === steps.length - 1}>avancer ▶</button>
        <button class="btn ghost" onClick={() => set(steps.length - 1)} title="Fin">⏭</button>
        <span class="step-label">étape {idx + 1}/{steps.length} · <b>{cur.label}</b> — {stepNote}</span>
      </div>
      <div class="chips-row exec-chips">{steps.map((s, i) => <span class={"stage-chip" + (i <= idx ? " on" : "")} onClick={() => set(i)}>{s.label}</span>)}</div>

      <div class="result-area exec-body">
        {cur.kind === "path" ? (<>
          <PathsTable plan={plan} hasOrder={hasOrder} />
          <CostCard plan={plan} path={path} />
          <div class="muted small">Table : {plan.stats.rows} ligne(s), {plan.stats.pages} page(s). Le Seq Scan lit tout séquentiellement (pages × seq_page_cost) ; un Index Scan paie des accès aléatoires (random_page_cost = {plan.consts.random_page_cost} × plus chers) mais n'en fait que pour les lignes candidates : il gagne quand la condition est sélective.</div>
        </>) : null}

        {cur.kind === "descent" && usedIndex ? (<>
          <IndexView index={usedIndex} descents={ex.descents} backward={path.orderProvided === "backward"} />
          <div class="card">
            <div class="card-title">Sondes</div>
            <ul class="conj-list small">
              {ex.descents.map((d) => <li>recherche de <code>{d.probe.length ? fmtKey(d.probe) : "(tout l'index)"}</code> → descente {d.path.join(" → ")} → entrées [{d.lo}, {d.hi}) soit {d.hi - d.lo} entrée(s)</li>)}
            </ul>
            <div class="muted small">Chaque sonde = une descente racine → feuille (hauteur {usedIndex.tree?.height ?? "?"}) puis un parcours des feuilles dans l'intervalle : c'est le « log n » d'un B-tree (Comer 1979).</div>
          </div>
        </>) : null}

        {cur.kind === "heap" ? (<>
          <HeapMap n={n} rowsPerPage={plan.consts.rows_per_page} touched={touched} kept={kept} indexOnly={path.kind === "index_only_scan"} stream={stream} />
          <Grid columns={srcCols} rows={srcRows} rowClass={rowClass} rowPrefix={rowPrefix} />
        </>) : null}

        {cur.kind === "filter" ? <FilterCard plan={plan} path={path} /> : null}

        {cur.kind === "sort" ? (
          <div class="card">
            <div class="card-title">{path.sortNeeded ? "Tri (Sort)" : "Déjà trié par l'index"}</div>
            <div class="muted">{path.sortNeeded
              ? `Les ${ex.passed} ligne(s) gardée(s) sont triées explicitement : coût 2 × cpu_operator_cost × R × log₂ R ≈ ${fmtCost(path.est.sortCost)}. Un index dont les colonnes commencent par celles de l'ORDER BY (même direction) éviterait cette étape.`
              : `Un B-tree livre ses entrées triées : parcouru ${path.orderProvided === "backward" ? "à l'envers" : "à l'endroit"}, il fournit l'ordre demandé sans étape Sort (PostgreSQL 11.4 ; Selinger : « interesting order »). Avec LIMIT, on peut donc s'arrêter tôt.`}</div>
            {!path.sortNeeded ? <div class="muted small">Vérification : sortie triée = {plan.sound.sortedOk ? "oui" : "NON"} · mêmes clés de tri que la sémantique = {plan.sound.orderKeysEqual ? "oui" : "NON"}.</div> : null}
          </div>) : null}

        {cur.kind === "limit" ? (
          <div class="card">
            <div class="card-title">LIMIT / OFFSET</div>
            <div class="muted">{ex.earlyStop
              ? `Arrêt anticipé : l'exécuteur s'est arrêté après ${ex.candidates} ligne(s) lue(s), dès que ${ex.passed} ligne(s) passante(s) suffisaient. Possible seulement si aucun tri n'est nécessaire (ordre fourni par l'index ou pas d'ORDER BY) et sans DISTINCT.`
              : `Pas d'arrêt anticipé : il faut d'abord ${path.sortNeeded ? "trier toutes les lignes gardées" : "tout lire (DISTINCT)"} avant de savoir lesquelles sont les premières.`}</div>
          </div>) : null}

        {cur.kind === "result" ? (<>
          <div class="preview-bar"><span class="mode-tag applied">résultat</span> {ex.returned} ligne(s) · produit par {pathLabel(path)} · {ex.candidates} ligne(s) lue(s) sur {n}</div>
          <Grid columns={r.columns} rows={plan.physRows} />
        </>) : null}

        <WhyNot plan={plan} />
      </div>
    </>
  );
}
