// Types partagés app <-> moteur. Le moteur renvoie exactement cette forme en JSON.

export type Val = number | string | boolean | null;

export interface Column {
  name: string;
  type: string; // INTEGER, VARCHAR, BOOLEAN, DATE, ...
  pk?: boolean;
}

export interface Table {
  name: string;
  columns: Column[];
  rows: Record<string, Val>[];
  x?: number;
  y?: number;
}

export interface Database {
  tables: Table[];
}

export type JoinType = "INNER" | "LEFT" | "RIGHT" | "FULL" | "CROSS";

export interface Relation {
  fromTable: string;
  fromCol: string;
  toTable: string;
  toCol: string;
  user?: boolean; // true = créé à la main → supprimable
  fk?: boolean; // true = clé étrangère DÉCLARÉE (badge FK + trait plein) ; sinon simple lien de jointure (pointillé)
  jtype?: JoinType; // type de jointure choisi pour ce lien (défaut INNER)
  flip?: boolean; // true = sens du JOIN généré inversé (FROM ↔ table jointe) — indépendant de la FK
}

export type TV = "True" | "False" | "Unknown";

export interface WhereRow {
  i: number;
  tv: TV;
  pass: boolean;
}

export interface Stage {
  kind: "from" | "where" | "select" | "distinct" | "order" | "limit";
  label: string;
  columns: string[];
  rows: Val[][];
  note: string;
  whereRows: WhereRow[] | null;
}

export interface Result {
  ok: boolean;
  error: string | null;
  errorPos?: number;
  columns: string[];
  rows: Val[][];
  pipeline: Stage[];
}
