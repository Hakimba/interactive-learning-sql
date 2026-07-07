// Générateur "smart constructor" : croise le TYPE et le NOM de la colonne
// pour produire des données plausibles. Les PK entières sont uniques (séquence).
import type { Column, Table, Val } from "./types";

const PRENOMS = ["Alice", "Bob", "Chloé", "David", "Emma", "Farid", "Gaëlle", "Hugo", "Inès", "Jules", "Karim", "Léa", "Malik", "Nora", "Omar", "Paul", "Rania", "Sofia", "Théo", "Yasmine", "Zoé", "Nathan", "Manon", "Lucas"];
const NOMS = ["Martin", "Bernard", "Dubois", "Robert", "Petit", "Durand", "Leroy", "Moreau", "Simon", "Laurent", "Garcia", "Roux"];
const VILLES = ["Paris", "Lyon", "Marseille", "Toulouse", "Nice", "Nantes", "Bordeaux", "Lille", "Rennes", "Strasbourg"];
const PAYS = ["France", "Belgique", "Suisse", "Canada", "Maroc", "Espagne"];
const STATUTS = ["nouveau", "en cours", "payé", "expédié", "annulé"];
const DEPARTEMENTS = ["Ingénierie", "Ventes", "Marketing", "RH", "Finance", "Support"];
const PRODUITS = ["Clavier", "Souris", "Écran", "Ordinateur", "Casque", "Webcam", "Câble", "Chargeur", "Disque SSD"];
const CATEGORIES = ["Périphérique", "Composant", "Accessoire", "Écran"];
const MOTS = ["lorem", "data", "test", "info", "note", "valeur", "ref", "item"];

const pick = <T,>(a: T[]) => a[Math.floor(Math.random() * a.length)];
const intBetween = (lo: number, hi: number) => lo + Math.floor(Math.random() * (hi - lo + 1));
const pad = (n: number) => (n < 10 ? "0" + n : "" + n);
const randDate = () => `${intBetween(2022, 2025)}-${pad(intBetween(1, 12))}-${pad(intBetween(1, 28))}`;
const randTs = () => `${randDate()} ${pad(intBetween(0, 23))}:${pad(intBetween(0, 59))}:${pad(intBetween(0, 59))}`;

function uuid(): string {
  const hex = "0123456789abcdef";
  let s = "";
  for (let i = 0; i < 36; i++)
    s += i === 8 || i === 13 || i === 18 || i === 23 ? "-" : i === 14 ? "4" : hex[Math.floor(Math.random() * 16)];
  return s;
}

const has = (name: string, words: string[]) => {
  const h = name.toLowerCase();
  return words.some((w) => h.includes(w));
};

const INT = ["INTEGER", "BIGINT", "SMALLINT"];
const FLOAT = ["DECIMAL", "NUMERIC", "DOUBLE", "REAL", "FLOAT"];
const TEXT = ["VARCHAR", "TEXT", "CHAR"];

function genValue(col: Column, seq: { n: number }): Val {
  const type = (col.type || "VARCHAR").toUpperCase();
  const name = col.name;

  if (col.pk && INT.includes(type)) return seq.n++;
  if (col.pk && type === "UUID") return uuid();

  // ~8% de NULL sur colonnes non-PK non-id (pour illustrer IS NULL)
  if (!col.pk && !has(name, ["id"]) && Math.random() < 0.08) return null;

  if (type === "UUID") return uuid();
  if (type === "BOOLEAN") return has(name, ["actif", "active", "valid"]) ? Math.random() < 0.7 : Math.random() < 0.5;
  if (type === "DATE") return randDate();
  if (type === "TIMESTAMP") return randTs();

  if (INT.includes(type)) {
    if (has(name, ["_id", "id_"]) || name.toLowerCase() === "id") return intBetween(1, 20);
    if (has(name, ["age"])) return intBetween(18, 75);
    if (has(name, ["stock", "qte", "quantit", "nombre", "nb", "count"])) return intBetween(0, 200);
    return intBetween(1, 1000);
  }
  if (FLOAT.includes(type)) {
    if (has(name, ["prix", "price", "montant", "amount", "total", "salaire", "salary", "budget", "cout"]))
      return Math.round((intBetween(5, 5000) + Math.random()) * 100) / 100;
    if (has(name, ["taux", "rate", "ratio", "pct"])) return Math.round(Math.random() * 10000) / 100;
    return Math.round(Math.random() * 100000) / 100;
  }
  if (TEXT.includes(type)) {
    if (has(name, ["email", "mail"])) return pick(PRENOMS).toLowerCase().normalize("NFD").replace(/[^a-z]/g, "") + intBetween(1, 99) + "@example.com";
    if (has(name, ["prenom", "firstname"])) return pick(PRENOMS);
    if (has(name, ["nom", "name", "client", "employe", "user", "auteur"])) return `${pick(PRENOMS)} ${pick(NOMS)}`;
    if (has(name, ["ville", "city"])) return pick(VILLES);
    if (has(name, ["pays", "country"])) return pick(PAYS);
    if (has(name, ["statut", "status", "etat"])) return pick(STATUTS);
    if (has(name, ["depart", "service", "equipe", "team"])) return pick(DEPARTEMENTS);
    if (has(name, ["categ", "type", "genre"])) return pick(CATEGORIES);
    if (has(name, ["produit", "product", "article", "item", "libelle", "label", "titre"])) return pick(PRODUITS);
    if (has(name, ["desc", "comment", "note", "message", "texte"])) return `${pick(MOTS)} ${pick(MOTS)} ${pick(MOTS)}`;
    return `${pick(MOTS)}-${intBetween(100, 999)}`;
  }
  return `${pick(MOTS)}${intBetween(1, 99)}`;
}

export function generateRows(table: Table, count: number): Record<string, Val>[] {
  const seq = { n: 1 };
  const rows: Record<string, Val>[] = [];
  for (let i = 0; i < count; i++) {
    const row: Record<string, Val> = {};
    for (const col of table.columns) row[col.name] = genValue(col, seq);
    rows.push(row);
  }
  return rows;
}
