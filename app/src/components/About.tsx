// Section « À propos / Fondements formels ».
// Transparence : pour CHAQUE construction supportée, la formalisation exacte qui la
// fonde + un statut honnête. Plus la méthodologie de validation (différentiel SQLite).
type Status = "ok" | "partial" | "regrounding" | "todo";

interface Src { label: string; url: string }
const GL: Src = { label: "Guagliardo & Libkin — A Formal Semantics of SQL Queries (PVLDB 11(1):27–39, 2017)", url: "https://www.vldb.org/pvldb/vol11/p27-guagliardo.pdf" };
const RC: Src = { label: "Ricciotti & Cheney — A Formalization of SQL with Nulls (arXiv:2003.11331, JAR 2022)", url: "https://arxiv.org/abs/2003.11331" };

interface Row { c: string; s: Src | null; st: Status; note?: string }
const ROWS: Row[] = [
  { c: "SELECT · projection · alias · colonnes calculées", s: GL, st: "ok" },
  { c: "WHERE · AND / OR / NOT · comparaisons", s: GL, st: "ok" },
  { c: "NULL + logique à 3 valeurs (Kleene)", s: GL, st: "ok" },
  { c: "IN (liste & sous-requête) · BETWEEN · LIKE · IS NULL", s: GL, st: "ok" },
  { c: "DISTINCT · sémantique de sacs (multiset)", s: GL, st: "ok" },
  { c: "Produit / CROSS JOIN · INNER JOIN (produit + ON)", s: GL, st: "ok" },
  { c: "LEFT / RIGHT / FULL JOIN (avec NULL)", s: RC, st: "ok", note: "sémantique standard (σ_ON(A×B) + lignes non-appariées complétées par NULL), validée par différentiel vs SQLite" },
  { c: "Expressions arithmétiques & fonctions scalaires", s: GL, st: "partial", note: "évaluation d'expressions selon GL ; comportement concret des fonctions validé par différentiel" },
  { c: "ORDER BY · LIMIT · OFFSET", s: null, st: "todo", note: "hors du cœur algébrique (impose un ordre sur un sac) — fondement formel à confirmer" },
];

const STLABEL: Record<Status, string> = { ok: "fondé", partial: "fondé (partiel)", regrounding: "en re-fondation", todo: "à confirmer" };

export function About({ onClose }: { onClose: () => void }) {
  return (
    <div class="modal-backdrop" onClick={onClose}>
      <div class="modal about" onClick={(e) => e.stopPropagation()}>
        <div class="modal-head">
          <strong>À propos — fondements formels</strong>
          <button class="btn ghost" onClick={onClose}>✕</button>
        </div>

        <p class="about-intro">
          Règle du projet : <b>chaque construction SQL de l'outil est fondée sur une formalisation sourcée</b> —
          aucune sémantique n'est codée « à la main ». Voici la source exacte par construction, et comment on valide.
        </p>

        <h4>Sources par construction</h4>
        <div class="grid-wrap">
          <table class="grid about-table">
            <thead><tr><th>Construction</th><th>Fondement</th><th>Statut</th></tr></thead>
            <tbody>
              {ROWS.map((r) => (
                <tr>
                  <td>{r.c}{r.note ? <div class="about-note">{r.note}</div> : null}</td>
                  <td>{r.s ? <a href={r.s.url} target="_blank" rel="noreferrer">{r.s.label}</a> : <span class="about-none">—</span>}</td>
                  <td><span class={"st st-" + r.st}>{STLABEL[r.st]}</span></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>

        <h4>Comment on garantit la correction</h4>
        <ol class="about-steps">
          <li><b>Fidélité à la formalisation</b> : une règle du papier = un cas du code (moteur OCaml).</li>
          <li><b>Types (cœur GADT)</b> : le typechecker interdit les confusions de types (on ne compare pas un texte à un nombre).</li>
          <li><b>Tests de propriétés (QCheck)</b> : lois algébriques (idempotence de DISTINCT, monotonie du WHERE, exclusion des NULL…). <span class="st st-ok">en place</span></li>
          <li><b>Test différentiel vs SQLite</b> : voir ci-dessous. <span class="st st-ok">en place · jointures INNER/LEFT/RIGHT/FULL identiques à SQLite</span></li>
        </ol>

        <h4>Test différentiel (méthodologie)</h4>
        <p class="about-intro">
          On exécute la <b>même base + requête</b> dans notre moteur <b>et</b> dans <b>SQLite</b> (oracle de confiance,
          compilé en WASM via <code>sql.js</code>), puis on <b>compare</b> les résultats — en <b>sacs</b> (ordre ignoré)
          sans <code>ORDER BY</code>, en séquence sinon. Répété sur des milliers de cas générés. Toute divergence = bug à
          corriger, <i>ou</i> bizarrerie connue de SQLite (on suit alors la formalisation, qui reste l'autorité).
          C'est la méthode de validation de Guagliardo–Libkin.
        </p>
        <pre class="about-ex">{`Base    clients(id, ville) = { (1,'Paris'), (2,'Lyon'), (3, NULL) }
Requête SELECT id FROM clients WHERE ville <> 'Paris'

  SQLite (oracle) → { 2 }
  notre moteur    → { 2 }        ✓ identique

La ligne 3 est exclue : NULL <> 'Paris' vaut « inconnu » (pas « vrai »)
→ logique à 3 valeurs. Une divergence ici révélerait un bug.`}</pre>

        <p class="about-foot">
          Sources : <a href={GL.url} target="_blank" rel="noreferrer">Guagliardo & Libkin, PVLDB 2017</a> ·{" "}
          <a href={RC.url} target="_blank" rel="noreferrer">Ricciotti & Cheney, JAR 2022</a>.
        </p>
      </div>
    </div>
  );
}
