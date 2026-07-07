#!/usr/bin/env bash
# Reconstruit le moteur OCaml, le teste, et copie le bundle JS dans l'app.
set -euo pipefail
cd "$(dirname "$0")/engine"
eval "$(opam env --switch 4.14.1)"
echo "→ build + tests du moteur"
dune build
dune exec test/run_tests.exe
echo "→ bundle js_of_ocaml"
dune build bin/main.bc.js
cp _build/default/bin/main.bc.js ../app/public/sql_engine.js
echo "✓ moteur reconstruit et copié dans app/public/sql_engine.js"
