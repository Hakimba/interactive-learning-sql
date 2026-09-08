// Test différentiel de la couche PHYSIQUE : notre plan vs SQLite `EXPLAIN QUERY PLAN` (oracle via sql.js).
// Ce qu'on compare (https://www.sqlite.org/eqp.html) :
//   (1) applicabilité : ∃ chemin d'index avec condition  ⇔  SQLite écrit « SEARCH t USING [COVERING] INDEX … » ;
//   (2) ordre : si SQLite n'a pas de « USE TEMP B-TREE FOR … ORDER BY », nous avons un chemin sans tri ;
//       et si SQLite parcourt l'index X et que notre chemin X fournit l'ordre, SQLite n'a pas de tri ;
//   (3) couvrant : si SQLite nomme l'index X, notre chemin X est Index Only Scan ⇔ « COVERING » ;
//   (4) résultats égaux en sacs (comme diff.cjs), et notre chemin physique est « sound ».
// Le CHOIX du chemin n'est pas comparé (deux planificateurs, deux modèles de coût) : seule l'applicabilité l'est.
// Exclusions documentées (info, pas échec) : IS NOT NULL (Postgres l'indexe, SQLite non) ; DISTINCT sans WHERE
// (SQLite parcourt un index couvrant pour dédoublonner) ; OR entre colonnes toutes indexées (SQLite MULTI-INDEX OR,
// Postgres BitmapOr — hors périmètre de cet incrément).
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

function mkSqlite(SQL, schema) {
  const db = new SQL.Database();
  for (const t of schema.tables) {
    db.run(`CREATE TABLE ${t.name} (${t.columns.map((c) => `${c.name} ${sqliteType(c.type)}`).join(", ")});`);
    const keys = t.columns.map((c) => c.name);
    for (const r of t.rows) db.run(`INSERT INTO ${t.name} (${keys.join(",")}) VALUES (${keys.map(() => "?").join(",")});`, keys.map((k) => (r[k] === undefined ? null : r[k])));
    for (const i of t.indexes || []) if (i.enabled !== false) db.run(`CREATE ${i.unique ? "UNIQUE " : ""}INDEX ${i.name} ON ${t.name} (${i.columns.join(", ")});`);
  }
  return db;
}
const eqp = (db, q) => { const r = db.exec("EXPLAIN QUERY PLAN " + q); return r.length ? r[0].values.map((v) => String(v[3])) : []; };
const sqliteRows = (db, q) => { const r = db.exec(q); return r.length ? r[0].values : []; };
const norm = (v) => (v === null || v === undefined ? "␀" : typeof v === "number" ? String(Math.round(v * 1e9) / 1e9) : String(v));
const bag = (rows) => rows.map((r) => r.map(norm).join("|")).sort();
const sameBag = (a, b) => { const x = bag(a), y = bag(b); return x.length === y.length && x.every((v, i) => v === y[i]); };

// ---- fixture : 30 lignes déterministes (LCG), a nullable 0..5, b 0..6, s ∈ {x,y,z} ----
let seed = 42; const rnd = () => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed / 0x7fffffff; };
const rows = Array.from({ length: 30 }, () => ({ a: rnd() < 0.2 ? null : Math.floor(rnd() * 6), b: Math.floor(rnd() * 7), s: ["x", "y", "z"][Math.floor(rnd() * 3)] }));
const cols = [{ name: "a", type: "INTEGER" }, { name: "b", type: "INTEGER" }, { name: "s", type: "VARCHAR" }];
const configs = {
  "sans index": [],
  "i_a": [{ name: "i_a", columns: ["a"] }],
  "i_ab + i_s": [{ name: "i_ab", columns: ["a", "b"] }, { name: "i_s", columns: ["s"] }],
  "i_ba + i_a(off)": [{ name: "i_ba", columns: ["b", "a"] }, { name: "i_a", columns: ["a"], enabled: false }],
};
const queries = [
  "SELECT * FROM t WHERE a = 3", "SELECT * FROM t WHERE 3 = a", "SELECT a FROM t WHERE a = 3", "SELECT a, b FROM t WHERE a = 3",
  "SELECT * FROM t WHERE a > 2", "SELECT * FROM t WHERE a <= 2", "SELECT * FROM t WHERE a BETWEEN 1 AND 3", "SELECT * FROM t WHERE a IN (1, 4)",
  "SELECT * FROM t WHERE a = 1 OR a = 2", "SELECT * FROM t WHERE a IS NULL", "SELECT * FROM t WHERE a = NULL",
  "SELECT * FROM t WHERE a <> 2", "SELECT * FROM t WHERE NOT a = 2", "SELECT * FROM t WHERE a NOT IN (1, 2)",
  "SELECT * FROM t WHERE upper(s) = 'X'", "SELECT * FROM t WHERE a + 1 = 3", "SELECT * FROM t WHERE a = b", "SELECT * FROM t WHERE s LIKE 'x%'", "SELECT * FROM t WHERE s LIKE '%x'",
  "SELECT * FROM t WHERE b = 2", "SELECT * FROM t WHERE b = 2 AND a = 3", "SELECT * FROM t WHERE a > 1 AND b = 2", "SELECT * FROM t WHERE a = 2 AND b > 1 AND b < 5",
  "SELECT * FROM t WHERE a = 2 AND s = 'x'", "SELECT * FROM t WHERE s = 'x'", "SELECT s FROM t WHERE s = 'x'", "SELECT * FROM t WHERE a = 2 OR b = 3",
  "SELECT * FROM t ORDER BY a", "SELECT * FROM t ORDER BY a DESC", "SELECT * FROM t ORDER BY b", "SELECT * FROM t WHERE a = 2 ORDER BY b",
  "SELECT * FROM t WHERE a > 1 ORDER BY b", "SELECT * FROM t ORDER BY a, b", "SELECT * FROM t ORDER BY a, b DESC", "SELECT * FROM t ORDER BY b DESC, a DESC",
  "SELECT * FROM t WHERE s = 'y' ORDER BY a", "SELECT a FROM t WHERE a > 1 ORDER BY a LIMIT 3", "SELECT * FROM t ORDER BY a LIMIT 2 OFFSET 1",
  "SELECT * FROM t WHERE a IS NOT NULL", "SELECT DISTINCT a FROM t", "SELECT DISTINCT a FROM t WHERE a > 1",
];
const excluded = (q, idx) => {
  if (/IS NOT NULL/.test(q)) return "IS NOT NULL : Postgres l'indexe, SQLite non";
  if (/DISTINCT/.test(q) && !/WHERE/.test(q)) return "DISTINCT sans WHERE : SQLite parcourt un index couvrant pour dédoublonner";
  if (/ OR /.test(q) && /a = 2 OR b = 3/.test(q) && idx.some((i) => i.columns[0] === "a") && idx.some((i) => i.columns[0] === "b")) return "OR multi-colonnes toutes indexées : MULTI-INDEX OR (hors périmètre)";
  return null;
};

initSqlJs({ locateFile: (f) => path.join(__dirname, "node_modules", "sql.js", "dist", f) }).then((SQL) => {
  let fail = 0, checks = 0, skipped = 0;
  for (const [label, indexes] of Object.entries(configs)) {
    const schema = { tables: [{ name: "t", columns: cols, rows, indexes }] };
    const db = mkSqlite(SQL, schema);
    console.log(`\n=== configuration : ${label} ===`);
    for (const q of queries) {
      const r = JSON.parse(SqlEngine.run(q, JSON.stringify(schema)));
      if (!r.ok) { console.log(`✗ moteur en erreur : ${q} → ${r.error}`); fail++; continue; }
      const plan = r.plan, details = eqp(db, q), ora = sqliteRows(db, q);
      const problems = [];
      // (4) résultats + soundness. Avec LIMIT/OFFSET, les ex æquo d'ORDER BY (ou l'absence d'ORDER BY) rendent le choix
      // des lignes légitimement dépendant du chemin d'accès (SQL ne fixe pas l'ordre des ex æquo) : on compare alors le
      // cardinal et l'inclusion dans le résultat complet.
      if (/LIMIT|OFFSET/.test(q)) {
        const full = sqliteRows(db, q.replace(/\s+(LIMIT|OFFSET)\s+\d+/g, ""));
        const fb = bag(full);
        if (r.rows.length !== ora.length) problems.push(`cardinal ≠ SQLite (${r.rows.length} vs ${ora.length})`);
        if (!bag(r.rows).every((x) => fb.includes(x))) problems.push("lignes hors du résultat complet");
        if (!sameBag(r.rows, ora)) console.log(`  (ex æquo : lignes différentes mais valides pour « ${q} »)`);
      } else if (!sameBag(r.rows, ora)) problems.push(`résultats ≠ SQLite (${r.rows.length} vs ${ora.length})`);
      if (!plan.available) problems.push(`plan indisponible : ${plan.reason}`);
      else {
        if (!plan.sound.sound) problems.push("plan non sound");
        const skip = excluded(q, indexes);
        if (skip) { skipped++; console.log(`· ${q}  [exclu : ${skip}]`); continue; }
        // (1) applicabilité
        const ourCond = plan.paths.some((p) => p.kind !== "seq_scan" && p.hasCond);
        const sqliteSearch = details.some((d) => /^SEARCH t USING (COVERING )?INDEX/.test(d));
        if (ourCond !== sqliteSearch) problems.push(`applicabilité : nous ${ourCond ? "index" : "scan"} / SQLite ${sqliteSearch ? "SEARCH" : "SCAN"}`);
        // (2) ordre
        if (/ORDER BY/.test(q)) {
          const sqliteSorted = details.some((d) => /USE TEMP B-TREE FOR .*ORDER BY/.test(d));
          const ourNoSort = plan.paths.some((p) => !p.sortNeeded);
          if (!sqliteSorted && !ourNoSort) problems.push("ordre : SQLite évite le tri, pas nous");
          const used = details.map((d) => (d.match(/USING (?:COVERING )?INDEX (\w+)/) || [])[1]).filter(Boolean);
          for (const x of used) { const p = plan.paths.find((p) => p.index === x); if (p && p.orderProvided && sqliteSorted) problems.push(`ordre : ${x} fournit l'ordre pour nous, SQLite trie`); }
        }
        // (3) couvrant
        for (const d of details) {
          const m = d.match(/USING (COVERING )?INDEX (\w+)/); if (!m) continue;
          const p = plan.paths.find((p) => p.index === m[2]);
          if (p && p.indexOnly !== Boolean(m[1])) problems.push(`couvrant : ${m[2]} nous ${p.indexOnly ? "oui" : "non"} / SQLite ${m[1] ? "oui" : "non"}`);
        }
      }
      checks++;
      const chosen = plan.available ? plan.paths[plan.chosen] : null;
      const ours = chosen ? `${chosen.kind}${chosen.index ? " " + chosen.index : ""}` : "—";
      if (problems.length) { fail++; console.log(`✗ ${q}\n    nous : ${ours} | SQLite : ${details.join(" ; ")}\n    ${problems.join(" ; ")}`); }
      else console.log(`✓ ${q}  →  nous : ${ours} | SQLite : ${details.join(" ; ")}`);
    }
    db.close();
  }
  console.log(`\n${checks} comparaisons, ${skipped} exclues, ${fail} désaccord(s)`);
  process.exit(fail ? 1 : 0);
});
