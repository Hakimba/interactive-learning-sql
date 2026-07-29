import { render } from "preact";
import "./styles.css";
import { App } from "./app";
import { tables, activeTable, relations, setSql } from "./state";
import { cloneSchema } from "./catalog";
import { generateRows } from "./datagen";

// Démarrage : on charge le schéma "boutique", on génère des données,
// et on pose une requête d'exemple — pour que l'utilisateur voie tout de suite quelque chose.
const s = cloneSchema("boutique");
if (s) {
  s.tables.forEach((t) => (t.rows = generateRows(t, 8)));
  tables.value = s.tables;
  relations.value = s.relations; // FK dessinées dès le démarrage
  activeTable.value = "clients";
}
setSql("SELECT nom, ville, actif FROM clients WHERE ville = 'Paris'");

render(<App />, document.getElementById("app")!);
