/* ============================================================================
 * sql.js — Moteur SQL pédagogique (sous-ensemble : SELECT/FROM/WHERE/ORDER BY/LIMIT)
 *
 * Écrit à la main (tokenizer + parser Pratt + évaluateur) plutôt que d'utiliser
 * un moteur SQL tout fait, PARCE QUE l'objectif est pédagogique : on veut exposer
 * les relations intermédiaires étape par étape (FROM -> WHERE -> SELECT -> ...).
 *
 * API principale :
 *   SQLEngine.run(sqlText, database) -> {
 *     ok, error, errorPos,
 *     columns: [name...],           // colonnes du résultat final
 *     rows: [[v,...]...],           // lignes finales (alignées sur columns)
 *     pipeline: [ stage... ]        // trace pour l'évaluation itérative
 *   }
 *
 * database = { tables: [ { name, columns:[{name,type,pk}], rows:[ {col:val} ] } ] }
 * ==========================================================================*/
(function (root, factory) {
  var api = factory();
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  if (typeof window !== "undefined") window.SQLEngine = api;
})(this, function () {
  "use strict";

  /* ----------------------------------------------------------------------- */
  /* Erreurs                                                                  */
  /* ----------------------------------------------------------------------- */
  function SqlError(message, pos) {
    this.name = "SqlError";
    this.message = message;
    this.pos = pos == null ? -1 : pos;
  }
  SqlError.prototype = Object.create(Error.prototype);

  /* ----------------------------------------------------------------------- */
  /* Tokenizer                                                                */
  /* ----------------------------------------------------------------------- */
  var KEYWORDS = {
    select: 1, distinct: 1, from: 1, where: 1, as: 1, and: 1, or: 1, not: 1,
    in: 1, like: 1, is: 1, null: 1, between: 1, order: 1, by: 1, asc: 1,
    desc: 1, limit: 1, offset: 1, true: 1, false: 1
  };

  function tokenize(input) {
    var toks = [];
    var i = 0, n = input.length;
    function push(type, value, pos) { toks.push({ type: type, value: value, pos: pos }); }

    while (i < n) {
      var c = input[i];

      // espaces
      if (c === " " || c === "\t" || c === "\n" || c === "\r") { i++; continue; }

      // commentaire -- ... fin de ligne
      if (c === "-" && input[i + 1] === "-") {
        while (i < n && input[i] !== "\n") i++;
        continue;
      }

      var start = i;

      // nombre
      if (c >= "0" && c <= "9") {
        var num = "";
        while (i < n && input[i] >= "0" && input[i] <= "9") num += input[i++];
        if (input[i] === ".") { num += input[i++]; while (i < n && input[i] >= "0" && input[i] <= "9") num += input[i++]; }
        push("num", parseFloat(num), start);
        continue;
      }

      // chaîne 'texte' avec '' échappé
      if (c === "'") {
        i++;
        var str = "";
        while (i < n) {
          if (input[i] === "'") {
            if (input[i + 1] === "'") { str += "'"; i += 2; continue; }
            i++; break;
          }
          str += input[i++];
        }
        push("str", str, start);
        continue;
      }

      // identifiant entre guillemets doubles "col"
      if (c === '"') {
        i++;
        var id = "";
        while (i < n && input[i] !== '"') id += input[i++];
        i++;
        push("ident", id, start);
        continue;
      }

      // identifiant / mot-clé
      if (/[A-Za-z_]/.test(c)) {
        var word = "";
        while (i < n && /[A-Za-z0-9_]/.test(input[i])) word += input[i++];
        var lower = word.toLowerCase();
        if (KEYWORDS[lower]) push("kw", lower, start);
        else push("ident", word, start);
        continue;
      }

      // opérateurs multi-caractères
      var two = input.substr(i, 2);
      if (two === "<=" || two === ">=" || two === "<>" || two === "!=") {
        push("op", two === "!=" ? "<>" : two, start); i += 2; continue;
      }

      // opérateurs / ponctuation simples
      if ("=<>+-*/%".indexOf(c) !== -1) { push("op", c, start); i++; continue; }
      if (c === "(") { push("punct", "(", start); i++; continue; }
      if (c === ")") { push("punct", ")", start); i++; continue; }
      if (c === ",") { push("punct", ",", start); i++; continue; }
      if (c === ".") { push("punct", ".", start); i++; continue; }
      if (c === "*") { push("op", "*", start); i++; continue; }

      throw new SqlError("Caractère inattendu « " + c + " »", i);
    }
    push("eof", null, n);
    return toks;
  }

  /* ----------------------------------------------------------------------- */
  /* Parser                                                                   */
  /* ----------------------------------------------------------------------- */
  function Parser(tokens) { this.toks = tokens; this.p = 0; }

  Parser.prototype.peek = function () { return this.toks[this.p]; };
  Parser.prototype.next = function () { return this.toks[this.p++]; };
  Parser.prototype.isKw = function (kw) { var t = this.peek(); return t.type === "kw" && t.value === kw; };
  Parser.prototype.isOp = function (op) { var t = this.peek(); return t.type === "op" && t.value === op; };
  Parser.prototype.isPunct = function (p) { var t = this.peek(); return t.type === "punct" && t.value === p; };
  Parser.prototype.eatKw = function (kw) {
    if (!this.isKw(kw)) this.err("Mot-clé « " + kw.toUpperCase() + " » attendu");
    return this.next();
  };
  Parser.prototype.eatPunct = function (p) {
    if (!this.isPunct(p)) this.err("« " + p + " » attendu");
    return this.next();
  };
  Parser.prototype.err = function (msg) {
    var t = this.peek();
    var got = t.type === "eof" ? "la fin de la requête" : "« " + t.value + " »";
    throw new SqlError(msg + " (trouvé : " + got + ")", t.pos);
  };

  Parser.prototype.parseQuery = function () {
    if (!this.isKw("select")) this.err("Une requête doit commencer par SELECT");
    this.eatKw("select");

    var distinct = false;
    if (this.isKw("distinct")) { this.next(); distinct = true; }

    var selectList = this.parseSelectList();

    this.eatKw("from");
    var from = this.parseFrom();

    var where = null;
    if (this.isKw("where")) { this.next(); where = this.parseExpr(); }

    var orderBy = null;
    if (this.isKw("order")) { this.next(); this.eatKw("by"); orderBy = this.parseOrderBy(); }

    var limit = null, offset = null;
    if (this.isKw("limit")) { this.next(); limit = this.parseIntLiteral("LIMIT"); }
    if (this.isKw("offset")) { this.next(); offset = this.parseIntLiteral("OFFSET"); }

    if (this.peek().type !== "eof") this.err("Fin de requête attendue");

    return { distinct: distinct, select: selectList, from: from, where: where, orderBy: orderBy, limit: limit, offset: offset };
  };

  Parser.prototype.parseIntLiteral = function (what) {
    var t = this.peek();
    if (t.type !== "num") this.err("Un nombre est attendu après " + what);
    this.next();
    return Math.floor(t.value);
  };

  Parser.prototype.parseSelectList = function () {
    // '*' seul
    if (this.isOp("*")) { this.next(); return { star: true, items: [] }; }
    var items = [];
    do {
      var expr = this.parseExpr();
      var alias = null;
      if (this.isKw("as")) { this.next(); alias = this.next().value; }
      else if (this.peek().type === "ident") { alias = this.next().value; } // alias implicite
      items.push({ expr: expr, alias: alias });
    } while (this.isPunct(",") && this.next());
    return { star: false, items: items };
  };

  Parser.prototype.parseFrom = function () {
    var t = this.peek();
    if (t.type !== "ident") this.err("Un nom de table est attendu après FROM");
    this.next();
    var alias = null;
    if (this.isKw("as")) { this.next(); alias = this.next().value; }
    else if (this.peek().type === "ident") { alias = this.next().value; }
    return { table: t.value, alias: alias };
  };

  Parser.prototype.parseOrderBy = function () {
    var keys = [];
    do {
      var expr = this.parseExpr();
      var dir = "asc";
      if (this.isKw("asc")) { this.next(); }
      else if (this.isKw("desc")) { this.next(); dir = "desc"; }
      keys.push({ expr: expr, dir: dir });
    } while (this.isPunct(",") && this.next());
    return keys;
  };

  // ---- Expressions (descente récursive avec priorités) ----
  Parser.prototype.parseExpr = function () { return this.parseOr(); };

  Parser.prototype.parseOr = function () {
    var left = this.parseAnd();
    while (this.isKw("or")) { this.next(); left = { t: "logic", op: "or", l: left, r: this.parseAnd() }; }
    return left;
  };
  Parser.prototype.parseAnd = function () {
    var left = this.parseNot();
    while (this.isKw("and")) { this.next(); left = { t: "logic", op: "and", l: left, r: this.parseNot() }; }
    return left;
  };
  Parser.prototype.parseNot = function () {
    if (this.isKw("not")) { this.next(); return { t: "not", e: this.parseNot() }; }
    return this.parseCmp();
  };
  Parser.prototype.parseCmp = function () {
    var left = this.parseAdd();

    // NOT optionnel avant IN / LIKE / BETWEEN  (ex: x NOT IN (...))
    var neg = false;
    if (this.isKw("not") && (this.toks[this.p + 1].type === "kw") &&
        /^(in|like|between)$/.test(this.toks[this.p + 1].value)) {
      this.next(); neg = true;
    }

    if (this.isKw("in")) {
      this.next(); this.eatPunct("(");
      var list = [];
      if (!this.isPunct(")")) { do { list.push(this.parseExpr()); } while (this.isPunct(",") && this.next()); }
      this.eatPunct(")");
      return { t: "in", e: left, list: list, neg: neg };
    }
    if (this.isKw("like")) { this.next(); return { t: "like", e: left, pat: this.parseAdd(), neg: neg }; }
    if (this.isKw("between")) {
      this.next(); var lo = this.parseAdd(); this.eatKw("and"); var hi = this.parseAdd();
      return { t: "between", e: left, lo: lo, hi: hi, neg: neg };
    }
    if (this.isKw("is")) {
      this.next();
      var isNeg = false;
      if (this.isKw("not")) { this.next(); isNeg = true; }
      this.eatKw("null");
      return { t: "isnull", e: left, neg: isNeg };
    }
    // comparaisons
    var t = this.peek();
    if (t.type === "op" && /^(=|<>|<|<=|>|>=)$/.test(t.value)) {
      this.next();
      return { t: "cmp", op: t.value, l: left, r: this.parseAdd() };
    }
    return left;
  };
  Parser.prototype.parseAdd = function () {
    var left = this.parseMul();
    while (this.isOp("+") || this.isOp("-")) { var op = this.next().value; left = { t: "arith", op: op, l: left, r: this.parseMul() }; }
    return left;
  };
  Parser.prototype.parseMul = function () {
    var left = this.parseUnary();
    while (this.isOp("*") || this.isOp("/") || this.isOp("%")) { var op = this.next().value; left = { t: "arith", op: op, l: left, r: this.parseUnary() }; }
    return left;
  };
  Parser.prototype.parseUnary = function () {
    if (this.isOp("-")) { this.next(); return { t: "neg", e: this.parseUnary() }; }
    if (this.isOp("+")) { this.next(); return this.parseUnary(); }
    return this.parsePrimary();
  };
  Parser.prototype.parsePrimary = function () {
    var t = this.peek();
    if (t.type === "num") { this.next(); return { t: "lit", v: t.value }; }
    if (t.type === "str") { this.next(); return { t: "lit", v: t.value }; }
    if (t.type === "kw" && t.value === "true") { this.next(); return { t: "lit", v: true }; }
    if (t.type === "kw" && t.value === "false") { this.next(); return { t: "lit", v: false }; }
    if (t.type === "kw" && t.value === "null") { this.next(); return { t: "lit", v: null }; }
    if (this.isPunct("(")) { this.next(); var e = this.parseExpr(); this.eatPunct(")"); return e; }
    if (t.type === "ident") {
      this.next();
      // appel de fonction ?
      if (this.isPunct("(")) {
        this.next();
        var args = [];
        if (!this.isPunct(")")) { do { args.push(this.parseExpr()); } while (this.isPunct(",") && this.next()); }
        this.eatPunct(")");
        return { t: "func", name: t.value.toLowerCase(), args: args };
      }
      // table.colonne ?
      if (this.isPunct(".")) {
        this.next();
        var col = this.peek();
        if (col.type !== "ident") this.err("Nom de colonne attendu après « . »");
        this.next();
        return { t: "col", table: t.value, name: col.value };
      }
      return { t: "col", table: null, name: t.value };
    }
    this.err("Expression attendue");
  };

  /* ----------------------------------------------------------------------- */
  /* Évaluateur                                                               */
  /* ----------------------------------------------------------------------- */
  function isNull(v) { return v === null || v === undefined; }

  function looseCmp(a, b) {
    if (typeof a === "number" && typeof b === "number") return a < b ? -1 : a > b ? 1 : 0;
    if (typeof a === "boolean" || typeof b === "boolean") { a = a ? 1 : 0; b = b ? 1 : 0; return a - b; }
    a = String(a); b = String(b);
    return a < b ? -1 : a > b ? 1 : 0;
  }

  function likeToRegex(pattern) {
    var out = "^";
    for (var i = 0; i < pattern.length; i++) {
      var ch = pattern[i];
      if (ch === "%") out += "[\\s\\S]*";
      else if (ch === "_") out += "[\\s\\S]";
      else out += ch.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    }
    return new RegExp(out + "$");
  }

  var FUNCS = {
    upper: function (a) { return isNull(a) ? null : String(a).toUpperCase(); },
    lower: function (a) { return isNull(a) ? null : String(a).toLowerCase(); },
    length: function (a) { return isNull(a) ? null : String(a).length; },
    abs: function (a) { return isNull(a) ? null : Math.abs(a); },
    round: function (a, d) { if (isNull(a)) return null; var f = Math.pow(10, d || 0); return Math.round(a * f) / f; },
    trim: function (a) { return isNull(a) ? null : String(a).trim(); },
    coalesce: function () { for (var i = 0; i < arguments.length; i++) if (!isNull(arguments[i])) return arguments[i]; return null; }
  };

  function Ctx(colset) { this.colset = colset; }

  function evalExpr(node, row, ctx) {
    switch (node.t) {
      case "lit": return node.v;
      case "col": {
        var key = node.name;
        if (!ctx.colset[key]) {
          throw new SqlError("Colonne inconnue : « " + (node.table ? node.table + "." + node.name : node.name) + " »");
        }
        var v = row[key];
        return v === undefined ? null : v;
      }
      case "neg": { var e = evalExpr(node.e, row, ctx); return isNull(e) ? null : -e; }
      case "arith": {
        var l = evalExpr(node.l, row, ctx), r = evalExpr(node.r, row, ctx);
        if (isNull(l) || isNull(r)) return null;
        switch (node.op) {
          case "+": return l + r;
          case "-": return l - r;
          case "*": return l * r;
          case "/": return r === 0 ? null : l / r;
          case "%": return r === 0 ? null : l % r;
        }
        return null;
      }
      case "cmp": {
        var a = evalExpr(node.l, row, ctx), b = evalExpr(node.r, row, ctx);
        if (isNull(a) || isNull(b)) return null;
        var c = looseCmp(a, b);
        switch (node.op) {
          case "=": return c === 0;
          case "<>": return c !== 0;
          case "<": return c < 0;
          case "<=": return c <= 0;
          case ">": return c > 0;
          case ">=": return c >= 0;
        }
        return null;
      }
      case "logic": {
        var lv = evalExpr(node.l, row, ctx), rv = evalExpr(node.r, row, ctx);
        if (node.op === "and") {
          if (lv === false || rv === false) return false;
          if (isNull(lv) || isNull(rv)) return null;
          return true;
        } else { // or
          if (lv === true || rv === true) return true;
          if (isNull(lv) || isNull(rv)) return null;
          return false;
        }
      }
      case "not": { var x = evalExpr(node.e, row, ctx); return isNull(x) ? null : !x; }
      case "isnull": { var y = evalExpr(node.e, row, ctx); var r1 = isNull(y); return node.neg ? !r1 : r1; }
      case "in": {
        var target = evalExpr(node.e, row, ctx);
        if (isNull(target)) return null;
        var sawNull = false, matched = false;
        for (var i = 0; i < node.list.length; i++) {
          var item = evalExpr(node.list[i], row, ctx);
          if (isNull(item)) { sawNull = true; continue; }
          if (looseCmp(target, item) === 0) { matched = true; break; }
        }
        var res = matched ? true : (sawNull ? null : false);
        return node.neg ? (res === null ? null : !res) : res;
      }
      case "like": {
        var s = evalExpr(node.e, row, ctx), p = evalExpr(node.pat, row, ctx);
        if (isNull(s) || isNull(p)) return null;
        var m = likeToRegex(String(p)).test(String(s));
        return node.neg ? !m : m;
      }
      case "between": {
        var v2 = evalExpr(node.e, row, ctx), lo = evalExpr(node.lo, row, ctx), hi = evalExpr(node.hi, row, ctx);
        if (isNull(v2) || isNull(lo) || isNull(hi)) return null;
        var inRange = looseCmp(v2, lo) >= 0 && looseCmp(v2, hi) <= 0;
        return node.neg ? !inRange : inRange;
      }
      case "func": {
        var fn = FUNCS[node.name];
        if (!fn) throw new SqlError("Fonction inconnue : « " + node.name + "() ». Disponibles : " + Object.keys(FUNCS).join(", "));
        var vals = node.args.map(function (a) { return evalExpr(a, row, ctx); });
        return fn.apply(null, vals);
      }
    }
    throw new SqlError("Nœud d'expression non géré : " + node.t);
  }

  // Libellé de colonne pour l'affichage
  function exprLabel(expr) {
    switch (expr.t) {
      case "col": return expr.name;
      case "lit": return expr.v === null ? "NULL" : String(expr.v);
      case "func": return expr.name + "(…)";
      case "arith": return exprLabel(expr.l) + " " + expr.op + " " + exprLabel(expr.r);
      case "neg": return "-" + exprLabel(expr.e);
      default: return "expr";
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Exécution + trace pédagogique                                            */
  /* ----------------------------------------------------------------------- */
  function findTable(db, name) {
    var low = name.toLowerCase();
    for (var i = 0; i < db.tables.length; i++) {
      if (db.tables[i].name.toLowerCase() === low) return db.tables[i];
    }
    return null;
  }

  function run(sqlText, db) {
    var result = { ok: false, error: null, errorPos: -1, columns: [], rows: [], pipeline: [] };
    var ast;
    try {
      var toks = tokenize(sqlText);
      ast = new Parser(toks).parseQuery();
    } catch (e) {
      if (e instanceof SqlError) { result.error = e.message; result.errorPos = e.pos; return result; }
      throw e;
    }

    try {
      // --- FROM ---
      var table = findTable(db, ast.from.table);
      if (!table) throw new SqlError("Table inconnue : « " + ast.from.table + " »");
      var colset = {};
      table.columns.forEach(function (c) { colset[c.name] = true; });
      var ctx = new Ctx(colset);
      var srcCols = table.columns.map(function (c) { return c.name; });
      var srcRows = table.rows.map(function (r) { return r; });

      result.pipeline.push({
        kind: "from", label: "FROM " + table.name,
        columns: srcCols, rows: srcRows,
        note: srcRows.length + " ligne(s) lue(s) dans « " + table.name + " »"
      });

      // --- WHERE ---
      var survivors = srcRows;
      if (ast.where) {
        var perRow = [];
        survivors = [];
        for (var i = 0; i < srcRows.length; i++) {
          var val = evalExpr(ast.where, srcRows[i], ctx);
          var pass = val === true;
          perRow.push({ index: i, pass: pass, value: val });
          if (pass) survivors.push(srcRows[i]);
        }
        result.pipeline.push({
          kind: "where", label: "WHERE", columns: srcCols,
          perRow: perRow, srcRows: srcRows,
          note: survivors.length + " ligne(s) gardée(s) sur " + srcRows.length
        });
      }

      // --- SELECT (projection) ---
      var outCols, projector;
      if (ast.select.star) {
        outCols = srcCols.slice();
        projector = function (row) { return outCols.map(function (c) { return row[c]; }); };
      } else {
        outCols = [];
        var usedNames = {};
        ast.select.items.forEach(function (it, idx) {
          var name = it.alias || exprLabel(it.expr);
          if (usedNames[name]) name = name + "_" + idx;
          usedNames[name] = true;
          outCols.push(name);
        });
        projector = function (row) {
          return ast.select.items.map(function (it) { return evalExpr(it.expr, row, ctx); });
        };
      }
      var projected = survivors.map(projector);
      result.pipeline.push({
        kind: "select", label: "SELECT", columns: outCols, rows: projected,
        note: (ast.select.star ? "toutes les colonnes" : outCols.length + " colonne(s) projetée(s)")
      });

      // --- DISTINCT ---
      if (ast.distinct) {
        var seen = {}, deduped = [];
        projected.forEach(function (r) {
          var key = JSON.stringify(r);
          if (!seen[key]) { seen[key] = true; deduped.push(r); }
        });
        projected = deduped;
        result.pipeline.push({ kind: "distinct", label: "DISTINCT", columns: outCols, rows: projected, note: projected.length + " ligne(s) unique(s)" });
      }

      // --- ORDER BY ---
      if (ast.orderBy) {
        // On associe chaque ligne projetée à sa ligne survivante pour évaluer les clés de tri.
        var pairs = projected.map(function (out, k) { return { out: out, src: survivors[k] }; });
        pairs.sort(function (A, B) {
          for (var j = 0; j < ast.orderBy.length; j++) {
            var key = ast.orderBy[j];
            var av = evalExpr(key.expr, A.src, ctx), bv = evalExpr(key.expr, B.src, ctx);
            var c;
            if (isNull(av) && isNull(bv)) c = 0;
            else if (isNull(av)) c = 1;          // NULL en dernier
            else if (isNull(bv)) c = -1;
            else c = looseCmp(av, bv);
            if (key.dir === "desc") c = -c;
            if (c !== 0) return c;
          }
          return 0;
        });
        projected = pairs.map(function (p) { return p.out; });
        result.pipeline.push({ kind: "order", label: "ORDER BY", columns: outCols, rows: projected, note: "tri appliqué" });
      }

      // --- LIMIT / OFFSET ---
      if (ast.limit != null || ast.offset != null) {
        var off = ast.offset || 0;
        var lim = ast.limit == null ? projected.length : ast.limit;
        projected = projected.slice(off, off + lim);
        result.pipeline.push({ kind: "limit", label: "LIMIT" + (ast.offset ? " / OFFSET" : ""), columns: outCols, rows: projected, note: projected.length + " ligne(s) conservée(s)" });
      }

      result.ok = true;
      result.columns = outCols;
      result.rows = projected;
      return result;
    } catch (e) {
      if (e instanceof SqlError) { result.error = e.message; result.errorPos = e.pos; return result; }
      throw e;
    }
  }

  return { run: run, tokenize: tokenize, SqlError: SqlError, functions: FUNCS };
});
