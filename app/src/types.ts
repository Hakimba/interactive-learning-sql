// Types partagés app <-> moteur. Le moteur renvoie exactement cette forme en JSON.

export type Val = number | string | boolean | null;

export interface Column {
  name: string;
  type: string; // INTEGER, VARCHAR, BOOLEAN, DATE, ...
  pk?: boolean;
}

// Index déclaré sur une table (couche PHYSIQUE). Ne change jamais le résultat d'une requête :
// seulement le chemin d'accès, les lignes lues et le coût. implicit = index unique de la clé primaire (<table>_pkey).
export interface IndexDef {
  name: string;
  columns: string[];
  unique: boolean;
  enabled: boolean;
  implicit?: boolean;
}

export interface Table {
  name: string;
  columns: Column[];
  rows: Record<string, Val>[];
  x?: number;
  y?: number;
  indexes?: IndexDef[];
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

/* ---------------- Couche physique (plan d'exécution) ---------------- */
export type ReasonKind =
  | "func_on_col" | "arith_on_col" | "like_leading_wildcard" | "like_collation" | "not_equal" | "negation" | "not_in"
  | "is_not_null" | "or_across_cols" | "col_vs_col" | "no_column" | "not_indexed" | "not_leading" | "after_range";
export interface Reason { kind: ReasonKind; column: string | null }
export interface Bound { incl: boolean; v: Val } // null = non borné
export interface Conjunct {
  text: string;
  sel: number; // sélectivité ESTIMÉE (Selinger 1979)
  sargable: boolean;
  column?: string;
  pred?: "eq" | "in" | "range" | "is_null" | "never";
  value?: Val;
  values?: Val[];
  lo?: Bound | null;
  hi?: Bound | null;
  reason?: Reason;
}
export interface Probe { prefix: Val[]; lo: Bound | null; hi: Bound | null }
export interface Matching {
  hasCond: boolean;
  probes: Probe[];
  constCols: string[];
  indexCond: number[]; // conjoints qui bornent le parcours d'index
  indexCheck: number[]; // vérifiés dans l'index, sans borner
  residual: number[]; // vérifiés sur la ligne du tas
  notApplicable: { conjunct: number; reason: Reason }[];
}
export type PathKind = "seq_scan" | "index_scan" | "index_only_scan";
export interface PathEst { rows: number; accessCost: number; sortCost: number; total: number; formula: string[] }
export interface PlanPath extends Matching {
  kind: PathKind;
  index: string | null;
  indexOnly: boolean;
  orderProvided: "forward" | "backward" | null;
  sortNeeded: boolean;
  chosen: boolean;
  est: PathEst;
}
export interface TreeNode { id: number; level: number; first: number; last: number; seps: Val[][]; children: number[] }
export interface BTree { fanout: number; height: number; root: number; nodes: TreeNode[] }
export interface BuiltIndex extends IndexDef {
  entries: { key: Val[]; rowid: number }[]; // triées (NULL d'abord), rowid = position dans la table
  uniqueViolations: Val[][];
  tree: BTree | null;
}
export interface Descent { probe: Val[]; path: number[]; lo: number; hi: number }
export interface Exec {
  kind: PathKind;
  index: string | null;
  descents: Descent[];
  entriesScanned: number;
  indexPages: number;
  stream: number[]; // rowids dans l'ordre de lecture
  touched: number[]; // lignes du tas réellement lues
  heapPages: number[];
  candidates: number;
  passed: number;
  returned: number;
  earlyStop: boolean;
}
export interface Consts {
  seq_page_cost: number; random_page_cost: number; cpu_tuple_cost: number;
  cpu_index_tuple_cost: number; cpu_operator_cost: number; rows_per_page: number; fanout: number;
}
export interface ColStat { name: string; distinct: number; nulls: number; indexed: boolean; min: number | null; max: number | null }
export interface Stats { rows: number; pages: number; columns: ColStat[] }
export interface Soundness { bagEqual: boolean; orderKeysEqual: boolean; sortedOk: boolean; tieAmbiguity: boolean; sound: boolean }
export type Plan =
  | { available: false; reason: string }
  | {
      available: true;
      reason: null;
      table: string;
      consts: Consts;
      stats: Stats;
      conjuncts: Conjunct[];
      indexes: BuiltIndex[];
      reports: (Matching & { index: string })[];
      paths: PlanPath[];
      chosen: number;
      forced: boolean;
      exec: Exec;
      physRows: Val[][];
      physPipeline: Stage[];
      sound: Soundness;
      warnings: string[];
    };

export interface Ddl {
  op: "create_index" | "drop_index";
  table: string;
  index: IndexDef;
  summary: string;
  warnings: string[];
}

// Options envoyées au moteur (couche physique). Toutes facultatives.
export interface EngineOptions { plan?: boolean; force?: string | null; consts?: Partial<Consts> }

export interface Result {
  ok: boolean;
  kind?: "select" | "ddl" | "error";
  error: string | null;
  errorPos?: number;
  columns: string[];
  rows: Val[][];
  pipeline: Stage[];
  plan?: Plan | null;
  ddl?: Ddl | null;
}
