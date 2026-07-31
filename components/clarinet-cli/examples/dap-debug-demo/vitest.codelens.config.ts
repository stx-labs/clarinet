// Config used by the VSCode extension's "Debug with Clarinet" CodeLens button.
// Not for direct use - the extension passes this via --config when running
// a codelens test so the default include pattern is not applied.
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    testTimeout: 120_000,
    hookTimeout: 30_000,
  },
});
