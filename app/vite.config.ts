import { defineConfig } from "vite";

// Preact via esbuild (runtime JSX automatique), sans preset lourd.
export default defineConfig({
  esbuild: { jsx: "automatic", jsxImportSource: "preact" },
  server: { open: false },
});
