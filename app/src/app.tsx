import { useState } from "preact/hooks";
import { TablesPanel } from "./components/TablesPanel";
import { Evaluator } from "./components/Evaluator";
import { About } from "./components/About";
import { engineReady } from "./engine";

function toggleTheme() {
  const root = document.documentElement;
  const cur = root.getAttribute("data-theme") || "dark";
  root.setAttribute("data-theme", cur === "dark" ? "light" : "dark");
}

export function App() {
  const [about, setAbout] = useState(false);
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
          <span class="incr">SELECT · FROM · WHERE · JOIN</span>
          <button class="btn ghost" onClick={() => setAbout(true)}>À propos</button>
          <button class="btn ghost" onClick={toggleTheme} title="Thème">◐</button>
        </div>
      </header>
      <main class="layout">
        <TablesPanel />
        <Evaluator />
      </main>
      {about ? <About onClose={() => setAbout(false)} /> : null}
    </>
  );
}
