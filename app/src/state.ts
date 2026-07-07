// État global réactif (signals).
import { signal, computed } from "@preact/signals";
import type { Table, Database, Result, Relation } from "./types";
import { runQuery } from "./engine";

export const tables = signal<Table[]>([]);
export const relations = signal<Relation[]>([]);
export const activeTable = signal<string | null>(null);
export const leftView = signal<"schema" | "data">("data");

export const sql = signal<string>("");
export const appliedSql = signal<string | null>(null); // requête "appliquée" (projection réelle)
export const rightTab = signal<"editor" | "step">("editor"); // onglet du panneau droit
export const resultOnly = signal<boolean>(false); // accordion : replier l'aperçu sur le résultat
export const stepSql = signal<string>(""); // requête FIGÉE pour le pas-à-pas (verrouillée)
export const stepIndex = signal<number>(0);

// Entre dans l'onglet Pas-à-pas : on fige la requête courante (lecture seule).
export function enterStep() {
  stepSql.value = sql.value;
  stepIndex.value = 0;
  rightTab.value = "step";
}

export const database = computed<Database>(() => ({ tables: tables.value }));

// Résultat de la requête APPLIQUÉE (pour la projection et le pas-à-pas).
export const appliedResult = computed<Result | null>(() =>
  appliedSql.value != null ? runQuery(appliedSql.value, database.value) : null
);

export function setSql(s: string) {
  sql.value = s;
  stepIndex.value = 0;
}

export function apply() {
  appliedSql.value = sql.value;
  stepIndex.value = 0;
}

// --- Insertion par clic depuis le panneau gauche (smart-constructor) ---
let editorEl: HTMLTextAreaElement | null = null;
let lastCursor = 0;

export function bindEditor(el: HTMLTextAreaElement | null) {
  editorEl = el;
}
export function noteCursor() {
  if (editorEl) lastCursor = editorEl.selectionStart ?? lastCursor;
}
export function insertIdent(name: string) {
  const cur = sql.value;
  const pos = editorEl ? editorEl.selectionStart ?? lastCursor : lastCursor;
  const before = cur.slice(0, pos);
  const after = cur.slice(pos);
  const needSpace = before.length > 0 && !/\s$/.test(before);
  const ins = (needSpace ? " " : "") + name;
  setSql(before + ins + after);
  const newPos = pos + ins.length;
  lastCursor = newPos;
  if (editorEl) {
    const el = editorEl;
    requestAnimationFrame(() => {
      el.focus();
      el.setSelectionRange(newPos, newPos);
    });
  }
}
