import { TablesPanel } from "./components/TablesPanel";
import { Evaluator } from "./components/Evaluator";
import { engineReady } from "./engine";

function toggleTheme() {
  const root = document.documentElement;
  const cur = root.getAttribute("data-theme") || "dark";
  root.setAttribute("data-theme", cur === "dark" ? "light" : "dark");
}

export function App() {
  return (
    <>
      <header class="appbar">
        <div class="brand">
          <span class="logo">▚</span>
          <span class="brand-name">SQL&nbsp;Sandbox</span>
          <span class="brand-sub">apprendre le SQL en le voyant</span>
        </div>
        <div class="appbar-right">
          {!engineReady() ? <span class="warn">⚠ moteur non chargé</span> : null}
          <span class="incr">incrément 1 · SELECT / FROM / WHERE</span>
          <button class="btn ghost" onClick={toggleTheme} title="Thème">◐</button>
        </div>
      </header>
      <main class="layout">
        <TablesPanel />
        <Evaluator />
      </main>
    </>
  );
}
