/* ============================================================================
 * datagen.js — Générateur de données "smart constructor".
 *
 * Produit des valeurs plausibles en croisant le TYPE de la colonne et des
 * INDICES tirés de son NOM (id, email, prix, ville, date...). Objectif : que les
 * tables générées ressemblent à de vraies données, pour rendre les requêtes
 * parlantes. Les clés primaires reçoivent des valeurs uniques.
 * ==========================================================================*/
(function (root, factory) {
  var api = factory();
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  if (typeof window !== "undefined") window.DataGen = api;
})(this, function () {
  "use strict";

  var PRENOMS = ["Alice", "Bob", "Chloé", "David", "Emma", "Farid", "Gaëlle", "Hugo",
    "Inès", "Jules", "Karim", "Léa", "Malik", "Nora", "Omar", "Paul", "Quentin",
    "Rania", "Sofia", "Théo", "Yasmine", "Zoé", "Nathan", "Manon", "Lucas"];
  var NOMS = ["Martin", "Bernard", "Dubois", "Robert", "Petit", "Durand", "Leroy",
    "Moreau", "Simon", "Laurent", "Lefebvre", "Michel", "Garcia", "Roux", "Fontaine"];
  var VILLES = ["Paris", "Lyon", "Marseille", "Toulouse", "Nice", "Nantes",
    "Bordeaux", "Lille", "Rennes", "Strasbourg"];
  var PAYS = ["France", "Belgique", "Suisse", "Canada", "Maroc", "Espagne", "Italie"];
  var STATUTS = ["nouveau", "en cours", "payé", "expédié", "annulé"];
  var DEPARTEMENTS = ["Ingénierie", "Ventes", "Marketing", "RH", "Finance", "Support"];
  var PRODUITS = ["Clavier", "Souris", "Écran", "Ordinateur", "Casque", "Webcam",
    "Câble", "Chargeur", "Disque SSD", "Station d'accueil"];
  var CATEGORIES = ["Périphérique", "Composant", "Accessoire", "Écran"];
  var MOTS = ["lorem", "data", "test", "info", "note", "valeur", "réf", "item"];

  // Générateur pseudo-aléatoire déterministe (graine) -> données stables si besoin
  function rng(seed) {
    var s = seed >>> 0 || 1;
    return function () { s = (s * 1664525 + 1013904223) >>> 0; return s / 4294967296; };
  }
  function pick(rand, arr) { return arr[Math.floor(rand() * arr.length)]; }
  function intBetween(rand, a, b) { return a + Math.floor(rand() * (b - a + 1)); }

  function uuid(rand) {
    var hex = "0123456789abcdef";
    var s = "";
    for (var i = 0; i < 36; i++) {
      if (i === 8 || i === 13 || i === 18 || i === 23) s += "-";
      else if (i === 14) s += "4";
      else s += hex[Math.floor(rand() * 16)];
    }
    return s;
  }
  function pad(n) { return n < 10 ? "0" + n : "" + n; }
  function randomDate(rand) {
    var y = intBetween(rand, 2022, 2025), m = intBetween(rand, 1, 12), d = intBetween(rand, 1, 28);
    return y + "-" + pad(m) + "-" + pad(d);
  }
  function randomTimestamp(rand) {
    return randomDate(rand) + " " + pad(intBetween(rand, 0, 23)) + ":" + pad(intBetween(rand, 0, 59)) + ":" + pad(intBetween(rand, 0, 59));
  }

  function hint(name) { return (name || "").toLowerCase(); }
  function nameHas(name, words) {
    var h = hint(name);
    for (var i = 0; i < words.length; i++) if (h.indexOf(words[i]) !== -1) return true;
    return false;
  }

  var TEXT_TYPES = { VARCHAR: 1, TEXT: 1, CHAR: 1 };
  var INT_TYPES = { INTEGER: 1, BIGINT: 1, SMALLINT: 1 };
  var FLOAT_TYPES = { DECIMAL: 1, NUMERIC: 1, DOUBLE: 1, REAL: 1, FLOAT: 1 };

  // Génère une valeur pour une colonne, selon (type, nom).
  function genValue(rand, col, rowIndex, seqState) {
    var type = (col.type || "VARCHAR").toUpperCase();
    var name = col.name;

    // Clé primaire entière -> séquence unique
    if (col.pk && INT_TYPES[type]) { return seqState.next++; }
    if (col.pk && type === "UUID") { return uuid(rand); }

    // ~8% de NULL pour les colonnes nullables non-PK (pour illustrer IS NULL)
    if (!col.pk && col.nullable !== false && !nameHas(name, ["id"]) && rand() < 0.08) return null;

    if (type === "UUID") return uuid(rand);
    if (type === "BOOLEAN") {
      if (nameHas(name, ["actif", "active", "enabled", "valid"])) return rand() < 0.7;
      return rand() < 0.5;
    }
    if (type === "DATE") return randomDate(rand);
    if (type === "TIMESTAMP") return randomTimestamp(rand);

    if (INT_TYPES[type]) {
      if (nameHas(name, ["_id", "id_"]) || hint(name) === "id") return intBetween(rand, 1, 20);
      if (nameHas(name, ["age"])) return intBetween(rand, 18, 75);
      if (nameHas(name, ["stock", "qte", "quantit", "count", "nombre", "nb"])) return intBetween(rand, 0, 200);
      if (nameHas(name, ["annee", "year"])) return intBetween(rand, 2000, 2025);
      return intBetween(rand, 1, 1000);
    }
    if (FLOAT_TYPES[type]) {
      if (nameHas(name, ["prix", "price", "montant", "amount", "total", "salaire", "salary", "budget", "cout", "cost"]))
        return Math.round((intBetween(rand, 5, 5000) + rand()) * 100) / 100;
      if (nameHas(name, ["taux", "rate", "ratio", "pct", "pourcent"]))
        return Math.round(rand() * 10000) / 100;
      return Math.round(rand() * 100000) / 100;
    }

    // Types texte : on s'appuie fortement sur le nom
    if (TEXT_TYPES[type]) {
      if (nameHas(name, ["email", "mail"])) {
        var p = pick(rand, PRENOMS).toLowerCase().normalize("NFD").replace(/[^a-z]/g, "");
        return p + intBetween(rand, 1, 99) + "@example.com";
      }
      if (nameHas(name, ["prenom", "firstname"])) return pick(rand, PRENOMS);
      if (nameHas(name, ["nom_famille", "lastname", "surname"])) return pick(rand, NOMS);
      if (nameHas(name, ["nom", "name", "client", "employe", "employee", "user", "auteur"]))
        return pick(rand, PRENOMS) + " " + pick(rand, NOMS);
      if (nameHas(name, ["ville", "city"])) return pick(rand, VILLES);
      if (nameHas(name, ["pays", "country"])) return pick(rand, PAYS);
      if (nameHas(name, ["statut", "status", "etat", "state"])) return pick(rand, STATUTS);
      if (nameHas(name, ["depart", "department", "service", "equipe", "team"])) return pick(rand, DEPARTEMENTS);
      if (nameHas(name, ["categ", "category", "type", "genre"])) return pick(rand, CATEGORIES);
      if (nameHas(name, ["produit", "product", "article", "item", "libelle", "label", "titre", "title"]))
        return pick(rand, PRODUITS);
      if (nameHas(name, ["desc", "comment", "note", "message", "texte", "text"]))
        return pick(rand, MOTS) + " " + pick(rand, MOTS) + " " + pick(rand, MOTS);
      // fallback texte
      return pick(rand, MOTS) + "-" + intBetween(rand, 100, 999);
    }

    return pick(rand, MOTS) + intBetween(rand, 1, 99);
  }

  // Génère `count` lignes pour une table {columns:[...]}.
  // seed optionnel pour reproductibilité.
  function generate(table, count, seed) {
    var rand = rng(seed || (Date.now() & 0xffffff) + (table.name || "").length * 7 + 1);
    var seqState = { next: 1 };
    var rows = [];
    for (var i = 0; i < count; i++) {
      var row = {};
      table.columns.forEach(function (col) { row[col.name] = genValue(rand, col, i, seqState); });
      rows.push(row);
    }
    return rows;
  }

  return { generate: generate, uuid: function () { return uuid(rng((Date.now() & 0xffffff) + 3)); } };
});
