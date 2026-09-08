// Types de colonnes agnostiques + schémas préchargés.
import type { Table, Relation } from "./types";

export interface SqlType {
  name: string;
  family: "num" | "text" | "bool" | "time" | "id";
  desc: string;
}

export const TYPES: SqlType[] = [
  { name: "INTEGER", family: "num", desc: "Entier" },
  { name: "BIGINT", family: "num", desc: "Grand entier" },
  { name: "DECIMAL", family: "num", desc: "Décimal à précision fixe" },
  { name: "DOUBLE", family: "num", desc: "Flottant" },
  { name: "VARCHAR", family: "text", desc: "Texte variable" },
  { name: "TEXT", family: "text", desc: "Texte long" },
  { name: "BOOLEAN", family: "bool", desc: "Vrai / faux" },
  { name: "DATE", family: "time", desc: "Date (AAAA-MM-JJ)" },
  { name: "TIMESTAMP", family: "time", desc: "Date + heure" },
  { name: "UUID", family: "id", desc: "Identifiant unique" },
];

export function familyOf(type: string): SqlType["family"] {
  return TYPES.find((t) => t.name === type.toUpperCase())?.family ?? "text";
}

export interface Schema {
  label: string;
  tables: Table[];
  relations: Relation[];
}

export const SCHEMAS: Record<string, Schema> = {
  boutique: {
    label: "Boutique en ligne",
    tables: [
      { name: "clients", x: 40, y: 40, columns: [
        { name: "id", type: "INTEGER", pk: true },
        { name: "nom", type: "VARCHAR" },
        { name: "email", type: "VARCHAR" },
        { name: "ville", type: "VARCHAR" },
        { name: "actif", type: "BOOLEAN" },
        { name: "cree_le", type: "DATE" },
      ], rows: [] },
      { name: "commandes", x: 420, y: 40, columns: [
        { name: "id", type: "INTEGER", pk: true },
        { name: "client_id", type: "INTEGER" },
        { name: "montant", type: "DECIMAL" },
        { name: "statut", type: "VARCHAR" },
        { name: "date_commande", type: "DATE" },
      ], rows: [] },
      { name: "produits", x: 420, y: 320, columns: [
        { name: "id", type: "INTEGER", pk: true },
        { name: "libelle", type: "VARCHAR" },
        { name: "categorie", type: "VARCHAR" },
        { name: "prix", type: "DECIMAL" },
        { name: "stock", type: "INTEGER" },
      ], rows: [] },
    ],
    relations: [{ fromTable: "commandes", fromCol: "client_id", toTable: "clients", toCol: "id", fk: true }],
  },
  rh: {
    label: "Ressources humaines",
    tables: [
      { name: "employes", x: 40, y: 40, columns: [
        { name: "id", type: "INTEGER", pk: true },
        { name: "nom", type: "VARCHAR" },
        { name: "departement", type: "VARCHAR" },
        { name: "salaire", type: "DECIMAL" },
        { name: "age", type: "INTEGER" },
        { name: "embauche_le", type: "DATE" },
        { name: "actif", type: "BOOLEAN" },
      ], rows: [] },
      { name: "departements", x: 440, y: 60, columns: [
        { name: "id", type: "INTEGER", pk: true },
        { name: "nom", type: "VARCHAR" },
        { name: "budget", type: "DECIMAL" },
      ], rows: [] },
    ],
    relations: [],
  },
};

export function cloneSchema(key: string): Schema | null {
  const s = SCHEMAS[key];
  return s ? (JSON.parse(JSON.stringify(s)) as Schema) : null;
}
