// Wrapper typé autour du moteur OCaml (exposé sur window.SqlEngine par js_of_ocaml).
// C'est le SEUL point où l'app parle au moteur : une string SQL + la base en JSON (+ options physiques).

import type { Database, EngineOptions, Result } from "./types";

declare global {
  interface Window {
    SqlEngine?: { run(sql: string, dbJson: string): string };
  }
}

const EMPTY = { columns: [] as string[], rows: [] as never[], pipeline: [] as never[], plan: null, ddl: null };

const NO_ENGINE: Result = {
  ok: false,
  kind: "error",
  error: "Moteur non chargé (sql_engine.js absent). Reconstruis le bundle OCaml.",
  ...EMPTY,
};

export function engineReady(): boolean {
  return typeof window !== "undefined" && !!window.SqlEngine;
}

// opts.plan = false pour les sondes internes (aperçu, pas-à-pas) : pas de couche physique inutile.
export function runQuery(sql: string, db: Database, opts: EngineOptions = {}): Result {
  if (!window.SqlEngine) return NO_ENGINE;
  if (!sql.trim()) return { ok: false, kind: "error", error: null, ...EMPTY };
  try {
    const payload = JSON.stringify({
      tables: db.tables.map((t) => ({ name: t.name, columns: t.columns, rows: t.rows, indexes: t.indexes ?? [] })),
      options: opts,
    });
    return JSON.parse(window.SqlEngine.run(sql, payload)) as Result;
  } catch (e) {
    return { ok: false, kind: "error", error: "Erreur d'appel moteur : " + String(e), ...EMPTY };
  }
}
