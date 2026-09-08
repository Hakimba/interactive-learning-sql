// Aides PURES pour l'onglet Exécution (couche physique) : textes pédagogiques, égalité de sacs, étapes.
import type { Database, IndexDef, Plan, PlanPath, Reason, Result, Val } from "./types";

export const isDdlText = (sql: string) => /^\s*(create|drop)\b/i.test(sql);

const norm = (v: Val) => (v === null ? "␀" : typeof v === "number" ? String(Math.round(v * 1e9) / 1e9) : String(v));
export function bagEqual(a: Val[][], b: Val[][]): boolean {
  if (a.length !== b.length) return false;
  const x = a.map((r) => r.map(norm).join("|")).sort();
  const y = b.map((r) => r.map(norm).join("|")).sort();
  return x.every((v, i) => v === y[i]);
}

// Base avec tous les index désactivés : sert à la comparaison « sans index / avec index ».
export function withoutIndexes(db: Database): Database {
  return { tables: db.tables.map((t) => ({ ...t, indexes: (t.indexes ?? []).map((i) => ({ ...i, enabled: false })) })) };
}

export const pathLabel = (p: PlanPath) =>
  p.kind === "seq_scan" ? "Seq Scan" : p.kind === "index_only_scan" ? `Index Only Scan · ${p.index}` : `Index Scan · ${p.index}`;

export const fmtVal = (v: Val) => (v === null ? "NULL" : typeof v === "string" ? `'${v}'` : String(v));
export const fmtKey = (k: Val[]) => k.map(fmtVal).join(", ");
export const fmtCost = (c: number) => (Math.round(c * 100) / 100).toFixed(2);

// Pourquoi un conjoint ne borne pas le parcours (sourcé : Selinger 1979 « sargable », PostgreSQL 11.2 / 11.3).
export function reasonText(r: Reason): string {
  switch (r.kind) {
    case "func_on_col": return "une fonction est appliquée à la colonne : l'index stocke la valeur brute, pas upper(col) (il faudrait un index sur expression)";
    case "arith_on_col": return "la colonne est dans une expression arithmétique : l'index ne connaît que la valeur brute";
    case "like_leading_wildcard": return "LIKE avec un joker en tête : aucun préfixe fixe pour borner le parcours";
    case "like_collation": return "LIKE : un B-tree ne sert qu'avec un préfixe fixe ET une collation binaire (Postgres : text_pattern_ops)";
    case "not_equal": return "<> : presque toutes les lignes correspondent — rien à borner";
    case "negation": return "NOT : la négation d'un intervalle n'est pas un intervalle";
    case "not_in": return "NOT IN : complément d'un ensemble, pas un intervalle";
    case "is_not_null": return "IS NOT NULL : Postgres peut l'indexer, SQLite non — non pris en charge ici";
    case "or_across_cols": return "OR entre colonnes différentes : il faudrait combiner deux index (BitmapOr) — incrément suivant";
    case "col_vs_col": return "comparaison entre deux colonnes : pas de valeur fixe à chercher dans l'index";
    case "no_column": return "aucune colonne : la condition est une constante";
    case "not_indexed": return "colonne absente de cet index";
    case "not_leading": return `« ${r.column} » n'est pas en tête de l'index : sans égalité sur les colonnes précédentes, l'index est d'abord trié par une autre colonne (règle du préfixe gauche, PG 11.3)`;
    case "after_range": return `« ${r.column} » vient après la colonne d'intervalle : vérifiée dans l'index, mais ne réduit pas la portion parcourue`;
  }
}

export type ExecStepKind = "path" | "descent" | "heap" | "filter" | "sort" | "limit" | "result";
export interface ExecStep { kind: ExecStepKind; label: string }

// Étapes du mini-stepper, selon le chemin choisi et la requête.
export function stepsOf(plan: Plan, hasOrder: boolean, hasLimit: boolean): ExecStep[] {
  if (!plan.available) return [];
  const p = plan.paths[plan.chosen];
  const steps: ExecStep[] = [{ kind: "path", label: "Chemin d'accès" }];
  if (p.kind !== "seq_scan") steps.push({ kind: "descent", label: "Descente d'index" });
  steps.push({ kind: "heap", label: p.kind === "index_only_scan" ? "Lecture (index seul)" : "Lecture du tas" });
  if (plan.conjuncts.length) steps.push({ kind: "filter", label: "Filtre" });
  if (hasOrder) steps.push({ kind: "sort", label: p.sortNeeded ? "Tri" : "Déjà trié" });
  if (hasLimit) steps.push({ kind: "limit", label: "LIMIT" });
  steps.push({ kind: "result", label: "Résultat" });
  return steps;
}

export const isDdlResult = (r: Result) => r.ok && r.kind === "ddl" && !!r.ddl;

export const indexLabel = (i: IndexDef) => `${i.name} (${i.columns.join(", ")})${i.unique ? " UNIQUE" : ""}`;
