# SQL Sandbox

Outil pédagogique pour **voir** ce que fait une requête SQL : on crée des tables,
on génère des données, on écrit une requête et on observe le résultat en direct —
avec un mode **pas-à-pas** qui déroule l'évaluation clause par clause.

## Architecture

Deux parties découplées, reliées par une frontière JSON étroite :

- **`engine/` — moteur SQL en OCaml.** Une fonction *pure* `requête × base → résultat (+ trace)`.
  Pipeline : `parseur → typechecker (cœur typé GADT) → sémantique`. La sémantique suit le papier
  *Guagliardo & Libkin, « A Formal Semantics of SQL Queries », PVLDB 2017* (sacs, NULL, logique à 3 valeurs).
  Compilé en JS par **js_of_ocaml** et exposé via `SqlEngine.run(sql, dbJSON)`.
- **`app/` — application web réactive** (TypeScript + Preact + Vite). Possède les tables, les données
  et l'UI ; interroge le moteur et affiche l'aperçu cadré, la projection et le pas-à-pas. Ne dépend
  jamais des internes OCaml → le moteur est remplaçable sans toucher à l'app.
- **`reference-prototype/`** — prototype JS initial (référence de design, non utilisé).

**Stack :** OCaml (dune, js_of_ocaml, qcheck) · TypeScript · Preact + @preact/signals · Vite.

## Prérequis
- OCaml via opam (switch `4.14.1`) avec `dune js_of_ocaml js_of_ocaml-ppx qcheck-core yojson`
- Node ≥ 18 + npm

## Lancer
```bash
./build-engine.sh          # compile le moteur OCaml, le teste, copie le bundle JS dans app/
cd app && npm install      # une seule fois
npm run dev                # ouvre http://localhost:5173
```

## Tester le moteur seul
```bash
cd engine && eval "$(opam env --switch 4.14.1)" && dune exec test/run_tests.exe
```

## Fragment SQL couvert

**Incrément 1 — mono-table.** `SELECT [DISTINCT] … FROM <table> [WHERE …] [ORDER BY …] [LIMIT/OFFSET]` — opérateurs
`= <> < <= > >=`, `AND/OR/NOT`, `IN`, `LIKE`, `IS [NOT] NULL`, `BETWEEN`, arithmétique et fonctions scalaires.

**Incrément 2 — jointures.** `[INNER|LEFT|RIGHT|FULL|CROSS] JOIN … ON …` (et la virgule), alias de table, map de schéma
avec clés étrangères, pas-à-pas dédié (boucle imbriquée). Sémantique : produit + ON (Guagliardo–Libkin), jointures
externes avec NULL (Ricciotti & Cheney). Validé par différentiel vs SQLite (`difftest/diff.cjs`).

**Incrément 3 — index (couche physique).** Ce que le *résultat* ne montre pas : le **chemin d'accès**.

- Déclarer un index : fiche table (bouton « ⚡ Index », case IDX à la création) **ou** DDL dans l'éditeur,
  `CREATE [UNIQUE] INDEX nom ON table (c1, c2)` / `DROP INDEX nom`, appliqué sur clic. La clé primaire est un index
  unique implicite (`<table>_pkey`).
- Onglet **Exécution** : chemins candidats (Seq Scan / Index Scan / Index Only Scan) avec coût *estimé* (modèle :
  forme System R, constantes PostgreSQL) et compteurs *exacts* (lignes lues, pages, entrées d'index) ; l'index visible
  (entrées triées + B-tree avec la descente) ; la table vue comme un tas paginé (lignes lues / sautées) ; le filtre
  décomposé (borne l'index / vérifié dans l'index / résiduel) avec la raison quand un conjoint n'est pas *sargable* ;
  « déjà trié par l'index » et arrêt anticipé sous LIMIT ; interrupteur actif/inactif par index et « forcer » un chemin.
- **Comparaison sans / avec index** en tête d'onglet : lignes examinées et pages lues des deux côtés, avec le rapport
  (« ÷ 100 000 »). À l'échelle réelle, les deux chemins sont réellement exécutés ; à l'**échelle simulée**
  (sélecteur « 1 000 … 1 000 000 lignes »), ta table sert d'échantillon : les fractions observées sont extrapolées,
  une colonne unique reste une clé, et le chemin choisi est celui qui coûte le moins à cette échelle. Le résultat et
  les compteurs exacts restent ceux de tes vraies lignes. Un graphique montre le coût des deux chemins selon la
  sélectivité, avec ta requête et le point de bascule.
- **Invariant vérifié à chaque exécution** (et par QCheck sur tous les chemins) : le chemin physique produit le même
  résultat que la sémantique — sacs égaux ; sous LIMIT, mêmes clés de tri (les ex æquo peuvent différer : SQL ne fixe
  pas leur ordre, et l'app le dit).
- Différentiel `difftest/plan.cjs` : applicabilité des index et disparition du tri comparées à `EXPLAIN QUERY PLAN`
  de SQLite (`SEARCH … USING INDEX` vs `SCAN`, `USE TEMP B-TREE FOR ORDER BY`).

Sources : Selinger et al. (SIGMOD 1979), Comer (1979), PostgreSQL chap. 11 / 14.1 / 19.7 — détail dans « À propos ».
Mono-table pour la couche physique ; ClickHouse (index primaire clairsemé, granules) prévu en incrément suivant.

## Tests différentiels (Node)
```bash
cd difftest && npm install        # une seule fois (sql.js)
node diff.cjs                     # résultats vs SQLite (jointures)
node plan.cjs                     # plan physique vs EXPLAIN QUERY PLAN (index, tri)
```
