// Section « À propos / Fondements formels ».
// Transparence : pour CHAQUE construction supportée, la formalisation exacte qui la
// fonde + un statut honnête. Plus la méthodologie de validation (différentiel SQLite).
type Status = "ok" | "partial" | "regrounding" | "todo" | "model";

interface Src { label: string; url: string }
const GL: Src = { label: "Guagliardo & Libkin — A Formal Semantics of SQL Queries (PVLDB 11(1):27–39, 2017)", url: "https://www.vldb.org/pvldb/vol11/p27-guagliardo.pdf" };
const RC: Src = { label: "Ricciotti & Cheney — A Formalization of SQL with Nulls (arXiv:2003.11331, JAR 2022)", url: "https://arxiv.org/abs/2003.11331" };
const SEL: Src = { label: "Selinger, Astrahan, Chamberlin, Lorie, Price — Access Path Selection in a Relational Database Management System (SIGMOD 1979)", url: "https://people.eecs.berkeley.edu/~brewer/cs262/3-selinger79.pdf" };
const COMER: Src = { label: "Comer — The Ubiquitous B-Tree (ACM Computing Surveys 11(2), 1979)", url: "https://dl.acm.org/doi/10.1145/356770.356776" };
const PG: Src = { label: "PostgreSQL — chap. 11 Indexes (11.2 types, 11.3 multicolonne, 11.4 ORDER BY, 11.9 index-only) · 14.1 EXPLAIN · 19.7 constantes de coût", url: "https://www.postgresql.org/docs/current/indexes.html" };

interface Row { c: string; s: Src | null; s2?: Src; s3?: Src; st: Status; note?: string }
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
  { c: "Index B-tree (structure, descente, feuilles triées)", s: COMER, st: "ok", note: "entrées triées (clé, n° de ligne), NULL en tête comme l'ORDER BY du moteur ; arbre de fan-out F affiché avec le chemin de descente" },
  { c: "Chemin d'accès : Seq Scan / Index Scan / Index Only Scan · prédicats sargables · préfixe gauche · ordre fourni par l'index · arrêt anticipé", s: SEL, s2: PG, st: "ok", note: "règles PostgreSQL 11.2–11.9 ; compteurs (lignes lues, pages, entrées) EXACTS ; propriété vérifiée à chaque exécution et par QCheck : résultat(chemin physique) ≡ résultat(sémantique) — sacs, ordre, ex æquo sous LIMIT" },
  { c: "Modèle de coût (pages × seq_page_cost + lignes × cpu_tuple_cost, random_page_cost, tri, sélectivité)", s: SEL, s2: PG, st: "model", note: "MODÈLE, pas une mesure : forme System R, constantes PostgreSQL 19.7 (1.0 / 4.0 / 0.01 / 0.005 / 0.0025), 4 lignes par page et fan-out 4 pour rester lisible ; la sélectivité estimée (Selinger §4) est affichée à côté du réel" },
];

const STLABEL: Record<Status, string> = { ok: "fondé", partial: "fondé (partiel)", regrounding: "en re-fondation", todo: "à confirmer", model: "fondé (modèle)" };

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
                  <td>
                    {r.s ? <a href={r.s.url} target="_blank" rel="noreferrer">{r.s.label}</a> : <span class="about-none">—</span>}
                    {r.s2 ? <div><a href={r.s2.url} target="_blank" rel="noreferrer">{r.s2.label}</a></div> : null}
                  </td>
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
          <li><b>Couche physique (index)</b> : le moteur sémantique reste l'autorité ; la couche physique ne fait que choisir <i>quelles lignes lui donner</i> et compter. À chaque exécution elle vérifie résultat(physique) ≡ résultat(sémantique) (propriété QCheck sur 600 cas × tous les chemins), et l'<b>applicabilité</b> des index est comparée à <code>EXPLAIN QUERY PLAN</code> de SQLite (<code>SEARCH … USING INDEX</code> vs <code>SCAN</code>, disparition du <code>TEMP B-TREE FOR ORDER BY</code>). <span class="st st-ok">en place · 155 comparaisons, 0 désaccord</span></li>
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
          <a href={RC.url} target="_blank" rel="noreferrer">Ricciotti & Cheney, JAR 2022</a> ·{" "}
          <a href={SEL.url} target="_blank" rel="noreferrer">Selinger et al., SIGMOD 1979</a> ·{" "}
          <a href={COMER.url} target="_blank" rel="noreferrer">Comer, 1979</a> ·{" "}
          <a href={PG.url} target="_blank" rel="noreferrer">PostgreSQL, chap. 11 / 14 / 19</a> ·{" "}
          <a href="https://www.sqlite.org/eqp.html" target="_blank" rel="noreferrer">SQLite EXPLAIN QUERY PLAN</a>.
        </p>
      </div>
    </div>
  );
}
