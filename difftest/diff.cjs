// Test différentiel : notre moteur (js_of_ocaml) vs SQLite (oracle, via sql.js).
// Même base + même requête dans les deux ; comparaison en SACS (ordre ignoré).
// La formalisation reste l'autorité ; l'oracle sert à révéler les bugs.
const path = require("path");
const initSqlJs = require("sql.js");
const { SqlEngine } = require(path.join(__dirname, "..", "engine", "_build", "default", "bin", "main.bc.js"));

const sqliteType = (t) => {
  const u = (t || "").toUpperCase();
  if (["INTEGER", "BIGINT", "SMALLINT"].includes(u)) return "INTEGER";
  if (["DECIMAL", "NUMERIC", "DOUBLE", "REAL", "FLOAT"].includes(u)) return "REAL";
  if (u === "BOOLEAN") return "INTEGER";
  return "TEXT";
};

function sqliteRun(SQL, schema, query) {
  const db = new SQL.Database();
  for (const t of schema.tables) {
    const cols = t.columns.map((c) => `${c.name} ${sqliteType(c.type)}`).join(", ");
    db.run(`CREATE TABLE ${t.name} (${cols});`);
    const keys = t.columns.map((c) => c.name);
    for (const r of t.rows) {
      const vals = keys.map((k) => (r[k] === undefined ? null : r[k]));
      db.run(`INSERT INTO ${t.name} (${keys.join(",")}) VALUES (${keys.map(() => "?").join(",")});`, vals);
    }
  }
  const res = db.exec(query);
  db.close();
  return res.length === 0 ? { columns: [], rows: [] } : { columns: res[0].columns, rows: res[0].values };
}

function engineRun(schema, query) {
  const r = JSON.parse(SqlEngine.run(query, JSON.stringify(schema)));
  if (!r.ok) throw new Error("moteur: " + r.error);
  return { columns: r.columns, rows: r.rows };
}

const norm = (v) => (v === null || v === undefined ? "␀" : String(v));
const bag = (res) => res.rows.map((r) => r.map(norm).join("|")).sort();
const sameBag = (a, b) => { const x = bag(a), y = bag(b); return x.length === y.length && x.every((v, i) => v === y[i]); };

const schema = {
  tables: [
    { name: "clients", columns: [{ name: "id", type: "INTEGER" }, { name: "nom", type: "VARCHAR" }],
      rows: [{ id: 1, nom: "Alice" }, { id: 2, nom: "Bob" }, { id: 3, nom: "Chloé" }, { id: 4, nom: "David" }, { id: 5, nom: "Emma" }] },
    { name: "commandes", columns: [{ name: "id", type: "INTEGER" }, { name: "client_id", type: "INTEGER" }, { name: "montant", type: "INTEGER" }],
      rows: [{ id: 10, client_id: 1, montant: 100 }, { id: 11, client_id: 1, montant: 50 }, { id: 12, client_id: 2, montant: 75 }, { id: 13, client_id: 99, montant: 20 }] },
  ],
};

const ON = "ON c.id = o.client_id";
const queries = [
  `SELECT nom, montant FROM clients c INNER JOIN commandes o ${ON}`,
  `SELECT nom, montant FROM clients c LEFT JOIN commandes o ${ON}`,
  `SELECT nom, montant FROM clients c RIGHT JOIN commandes o ${ON}`,
  `SELECT nom, montant FROM clients c FULL JOIN commandes o ${ON}`,
];

initSqlJs({ locateFile: (f) => path.join(__dirname, "node_modules", "sql.js", "dist", f) }).then((SQL) => {
  let fail = 0;
  for (const q of queries) {
    const eng = engineRun(schema, q);
    let ora;
    try { ora = sqliteRun(SQL, schema, q); } catch (e) { console.log("SQLite ne supporte pas:", q.match(/(INNER|LEFT|RIGHT|FULL)/)[0], "→", String(e.message).slice(0, 60)); continue; }
    const ok = sameBag(eng, ora);
    if (!ok) fail++;
    console.log(`${ok ? "✓" : "✗"} ${q.match(/(INNER|LEFT|RIGHT|FULL)/)[0]} JOIN — moteur ${eng.rows.length} lignes | SQLite ${ora.rows.length} lignes`);
  }
  // détail de la requête de Hakim
  console.log("\n--- détail LEFT JOIN (ta requête) ---");
  const q = `SELECT nom, montant FROM clients c LEFT JOIN commandes o ${ON}`;
  const eng = engineRun(schema, q);
  const matched = eng.rows.filter((r) => r[1] !== null).length;
  const nullFilled = eng.rows.filter((r) => r[1] === null).length;
  console.log(`${eng.rows.length} lignes = ${matched} appariées + ${nullFilled} clients sans commande (montant NULL)`);
  console.log(JSON.stringify(eng.rows));
  process.exit(fail ? 1 : 0);
});
