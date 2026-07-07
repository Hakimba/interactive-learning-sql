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

## Fragment SQL couvert (incrément 1)
`SELECT [DISTINCT] … FROM <table> [WHERE …] [ORDER BY …] [LIMIT/OFFSET]` — opérateurs
`= <> < <= > >=`, `AND/OR/NOT`, `IN`, `LIKE`, `IS [NOT] NULL`, `BETWEEN`, arithmétique et
fonctions scalaires. Une seule table pour l'instant (les JOIN arrivent à l'incrément 2).
