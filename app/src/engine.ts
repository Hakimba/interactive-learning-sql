// Wrapper typé autour du moteur OCaml (exposé sur window.SqlEngine par js_of_ocaml).
// C'est le SEUL point où l'app parle au moteur : une string SQL + la base en JSON.

import type { Database, Result } from "./types";

declare global {
  interface Window {
    SqlEngine?: { run(sql: string, dbJson: string): string };
  }
}

const NO_ENGINE: Result = {
  ok: false,
  error: "Moteur non chargé (sql_engine.js absent). Reconstruis le bundle OCaml.",
  columns: [],
  rows: [],
  pipeline: [],
};

export function engineReady(): boolean {
  return typeof window !== "undefined" && !!window.SqlEngine;
}

export function runQuery(sql: string, db: Database): Result {
  if (!window.SqlEngine) return NO_ENGINE;
  if (!sql.trim()) return { ok: false, error: null, columns: [], rows: [], pipeline: [] };
  try {
    const payload = JSON.stringify({
      tables: db.tables.map((t) => ({ name: t.name, columns: t.columns, rows: t.rows })),
    });
    return JSON.parse(window.SqlEngine.run(sql, payload)) as Result;
  } catch (e) {
    return { ok: false, error: "Erreur d'appel moteur : " + String(e), columns: [], rows: [], pipeline: [] };
  }
}
