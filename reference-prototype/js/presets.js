/* ============================================================================
 * presets.js — Types de colonnes agnostiques + schémas préchargés.
 * ==========================================================================*/
(function (root, factory) {
  var api = factory();
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  if (typeof window !== "undefined") window.Presets = api;
})(this, function () {
  "use strict";

  // Types SQL agnostiques (incrément 1). Chaque type porte une "famille" pour le style.
  var TYPES = [
    { name: "INTEGER", family: "num", desc: "Entier" },
    { name: "BIGINT", family: "num", desc: "Grand entier" },
    { name: "DECIMAL", family: "num", desc: "Décimal à précision fixe" },
    { name: "DOUBLE", family: "num", desc: "Flottant double précision" },
    { name: "VARCHAR", family: "text", desc: "Texte de longueur variable" },
    { name: "TEXT", family: "text", desc: "Texte long" },
    { name: "BOOLEAN", family: "bool", desc: "Vrai / faux" },
    { name: "DATE", family: "time", desc: "Date (AAAA-MM-JJ)" },
    { name: "TIMESTAMP", family: "time", desc: "Date + heure" },
    { name: "UUID", family: "id", desc: "Identifiant unique" }
  ];

  function familyOf(typeName) {
    var t = (typeName || "").toUpperCase();
    for (var i = 0; i < TYPES.length; i++) if (TYPES[i].name === t) return TYPES[i].family;
    return "text";
  }

  // Schémas préchargés. `relations` sert plus tard (JOIN, incrément 2) et pour dessiner les liens.
  var SCHEMAS = {
    boutique: {
      label: "Boutique en ligne",
      tables: [
        {
          name: "clients", x: 40, y: 40,
          columns: [
            { name: "id", type: "INTEGER", pk: true },
            { name: "nom", type: "VARCHAR" },
            { name: "email", type: "VARCHAR" },
            { name: "ville", type: "VARCHAR" },
            { name: "actif", type: "BOOLEAN" },
            { name: "cree_le", type: "DATE" }
          ]
        },
        {
          name: "commandes", x: 420, y: 40,
          columns: [
            { name: "id", type: "INTEGER", pk: true },
            { name: "client_id", type: "INTEGER" },
            { name: "montant", type: "DECIMAL" },
            { name: "statut", type: "VARCHAR" },
            { name: "date_commande", type: "DATE" }
          ]
        },
        {
          name: "produits", x: 420, y: 320,
          columns: [
            { name: "id", type: "INTEGER", pk: true },
            { name: "libelle", type: "VARCHAR" },
            { name: "categorie", type: "VARCHAR" },
            { name: "prix", type: "DECIMAL" },
            { name: "stock", type: "INTEGER" }
          ]
        }
      ],
      relations: [
        { fromTable: "commandes", fromCol: "client_id", toTable: "clients", toCol: "id" }
      ]
    },
    rh: {
      label: "Ressources humaines",
      tables: [
        {
          name: "employes", x: 40, y: 40,
          columns: [
            { name: "id", type: "INTEGER", pk: true },
            { name: "nom", type: "VARCHAR" },
            { name: "departement", type: "VARCHAR" },
            { name: "salaire", type: "DECIMAL" },
            { name: "age", type: "INTEGER" },
            { name: "embauche_le", type: "DATE" },
            { name: "actif", type: "BOOLEAN" }
          ]
        },
        {
          name: "departements", x: 440, y: 60,
          columns: [
            { name: "id", type: "INTEGER", pk: true },
            { name: "nom", type: "VARCHAR" },
            { name: "budget", type: "DECIMAL" }
          ]
        }
      ],
      relations: []
    }
  };

  // Clone profond simple (pour ne pas muter les presets).
  function clone(obj) { return JSON.parse(JSON.stringify(obj)); }

  return {
    TYPES: TYPES,
    familyOf: familyOf,
    schemas: SCHEMAS,
    loadSchema: function (key) { return SCHEMAS[key] ? clone(SCHEMAS[key]) : null; }
  };
});
