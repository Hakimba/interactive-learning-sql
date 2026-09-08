// État global réactif (signals).
import { signal, computed, effect } from "@preact/signals";
import type { Table, Database, Result, Relation, Val, IndexDef, Ddl } from "./types";
import { SCHEMAS, familyOf, type Schema } from "./catalog";
import { runQuery } from "./engine";

export const tables = signal<Table[]>([]);
export const relations = signal<Relation[]>([]);
export const activeTable = signal<string | null>(null);
export const leftView = signal<"schema" | "data">("data");

// --- Schémas créés par l'utilisateur (persistés dans le navigateur) ---
const USER_SCHEMAS_KEY = "sqlsandbox.userSchemas.v1";

function loadUserSchemas(): Record<string, Schema> {
  try {
    const raw = localStorage.getItem(USER_SCHEMAS_KEY);
    const parsed = raw ? (JSON.parse(raw) as Record<string, Schema>) : {};
    return parsed && typeof parsed === "object" ? parsed : {};
  } catch {
    return {};
  }
}

function persist(next: Record<string, Schema>) {
  try {
    localStorage.setItem(USER_SCHEMAS_KEY, JSON.stringify(next));
  } catch {
    /* stockage indisponible (mode privé, quota) : on reste en mémoire */
  }
}

// Schémas utilisateur, et clé du schéma en cours d'édition (null = preset ou bac à sable libre).
export const userSchemas = signal<Record<string, Schema>>(loadUserSchemas());
export const activeSchemaKey = signal<string | null>(null);

function schemaKey(label: string): string {
  const base =
    "u:" +
    (label.trim().toLowerCase().replace(/\s+/g, "_").replace(/[^a-z0-9_]/g, "") || "schema");
  const taken = new Set([...Object.keys(SCHEMAS), ...Object.keys(userSchemas.value)]);
  let k = base;
  for (let i = 2; taken.has(k); i++) k = `${base}_${i}`;
  return k;
}

// Crée un schéma vierge nommé, l'enregistre, et vide la carte pour le construire de zéro.
export function createSchema(label: string): string {
  const key = schemaKey(label);
  const schema: Schema = { label: label.trim(), tables: [], relations: [] };
  const next = { ...userSchemas.value, [key]: schema };
  userSchemas.value = next;
  persist(next);
  activeSchemaKey.value = key;
  tables.value = [];
  relations.value = [];
  activeTable.value = null;
  return key;
}

// Supprime un schéma utilisateur ; s'il était actif, on repasse en bac à sable vide.
export function deleteSchema(key: string) {
  const next = { ...userSchemas.value };
  delete next[key];
  userSchemas.value = next;
  persist(next);
  if (activeSchemaKey.value === key) {
    activeSchemaKey.value = null;
    tables.value = [];
    relations.value = [];
    activeTable.value = null;
  }
}

// Tant qu'un schéma utilisateur est actif, la carte (tables + FK, hors données générées)
// est recopiée en continu dans son enregistrement → la persistance suit la construction.
effect(() => {
  const key = activeSchemaKey.value;
  const ts = tables.value;
  const rels = relations.value;
  if (!key) return;
  if (!userSchemas.peek()[key]) return; // supprimé entre-temps
  const snapshot: Schema = {
    label: userSchemas.peek()[key].label,
    tables: ts.map((t) => ({
      name: t.name,
      x: t.x,
      y: t.y,
      columns: t.columns.map((c) => ({ ...c })),
      rows: [], // les données sont régénérées au chargement, comme pour les presets
      indexes: (t.indexes ?? []).map((i) => ({ ...i })),
    })),
    relations: rels.map((r) => ({ ...r })),
  };
  const next = { ...userSchemas.peek(), [key]: snapshot };
  userSchemas.value = next;
  persist(next);
});

export const sql = signal<string>("");
export const appliedSql = signal<string | null>(null); // requête "appliquée" (projection réelle)
export const rightTab = signal<"editor" | "step" | "exec">("editor"); // onglet du panneau droit
export const resultOnly = signal<boolean>(false); // accordion : replier l'aperçu sur le résultat
export const stepSql = signal<string>(""); // requête FIGÉE pour le pas-à-pas (verrouillée)
export const stepIndex = signal<number>(0);
export const execStep = signal<number>(0); // étape courante du mini-stepper « Exécution »
export const execForce = signal<string | null>(null); // chemin forcé (« seq_scan » ou nom d'index) ; null = choix du modèle

// Entre dans l'onglet Pas-à-pas : on fige la requête courante (lecture seule).
export function enterStep() {
  stepSql.value = sql.value;
  stepIndex.value = 0;
  rightTab.value = "step";
}

// Entre dans l'onglet Exécution (couche physique : chemins d'accès, index, coût, lignes lues).
export function enterExec() {
  execStep.value = 0;
  rightTab.value = "exec";
}

/* ---------------- Index (couche physique) ---------------- */
// La clé primaire est un index unique IMPLICITE (« <table>_pkey », comme Postgres). On le matérialise ici,
// côté app, comme un index ordinaire ; il peut être désactivé pour l'expérience, mais pas supprimé.
export const pkeyOff = signal<Record<string, boolean>>({});

export function pkeyName(table: string) { return `${table}_pkey`; }

export function effectiveIndexes(t: Table): IndexDef[] {
  const declared = t.indexes ?? [];
  const pkCols = t.columns.filter((c) => c.pk).map((c) => c.name);
  if (!pkCols.length || declared.some((i) => i.name.toLowerCase() === pkeyName(t.name).toLowerCase())) return declared;
  return [{ name: pkeyName(t.name), columns: pkCols, unique: true, enabled: !pkeyOff.value[t.name], implicit: true }, ...declared];
}

export function allIndexNames(): string[] {
  return tables.value.flatMap((t) => effectiveIndexes(t).map((i) => i.name));
}

function updateTable(name: string, f: (t: Table) => Table) {
  tables.value = tables.value.map((t) => (t.name.toLowerCase() === name.toLowerCase() ? f(t) : t));
}

export function addIndex(table: string, def: IndexDef) {
  updateTable(table, (t) => ({ ...t, indexes: [...(t.indexes ?? []), { ...def, implicit: false }] }));
}

export function dropIndex(table: string, name: string) {
  updateTable(table, (t) => ({ ...t, indexes: (t.indexes ?? []).filter((i) => i.name.toLowerCase() !== name.toLowerCase()) }));
}

export function toggleIndex(table: string, name: string) {
  if (name.toLowerCase() === pkeyName(table).toLowerCase()) {
    const t = tables.value.find((x) => x.name.toLowerCase() === table.toLowerCase());
    if (t && !(t.indexes ?? []).some((i) => i.name.toLowerCase() === name.toLowerCase())) {
      pkeyOff.value = { ...pkeyOff.value, [t.name]: !pkeyOff.value[t.name] };
      return;
    }
  }
  updateTable(table, (t) => ({ ...t, indexes: (t.indexes ?? []).map((i) => (i.name.toLowerCase() === name.toLowerCase() ? { ...i, enabled: !i.enabled } : i)) }));
}

// Applique un DDL validé par le moteur (l'app reste propriétaire de l'état ; le moteur ne fait que valider).
export function applyDdl(d: Ddl) {
  if (d.op === "create_index") addIndex(d.table, { ...d.index, enabled: true });
  else dropIndex(d.table, d.index.name);
}

// Les cellules éditées à la main sont stockées en chaîne brute (frappe fluide, décimales OK).
// On les coerce vers le type de la colonne UNIQUEMENT ici — seul point où la base part au moteur.
function coerceCell(v: Val, type: string): Val {
  if (v === null || v === undefined) return null;
  const fam = familyOf(type);
  if (fam === "num") {
    if (typeof v === "number") return v;
    const s = String(v).trim();
    if (s === "") return null;
    const n = Number(s);
    return Number.isFinite(n) ? n : String(v); // non numérique → laissé tel quel (le moteur signalera)
  }
  if (fam === "bool") {
    if (typeof v === "boolean") return v;
    const s = String(v).trim().toLowerCase();
    if (s === "") return null;
    if (s === "true" || s === "vrai" || s === "1") return true;
    if (s === "false" || s === "faux" || s === "0") return false;
    return String(v);
  }
  return typeof v === "string" ? v : String(v); // text / time / id
}

export const database = computed<Database>(() => ({
  tables: tables.value.map((t) => ({
    ...t,
    rows: t.rows.map((r) => {
      const out: Record<string, Val> = {};
      for (const c of t.columns) out[c.name] = coerceCell(r[c.name] ?? null, c.type);
      return out;
    }),
    indexes: effectiveIndexes(t), // index déclarés + _pkey implicite
  })),
}));

// Résultat de la requête APPLIQUÉE (pour la projection et le pas-à-pas).
export const appliedResult = computed<Result | null>(() =>
  appliedSql.value != null ? runQuery(appliedSql.value, database.value) : null
);

export function setSql(s: string) {
  sql.value = s;
  stepIndex.value = 0;
  execStep.value = 0;
  execForce.value = null;
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
