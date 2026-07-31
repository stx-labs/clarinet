import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Only the auto tests run with `pnpm test` (they spawn their own server)
    // The codelens tests (counter.codelens.test.ts) are driven by the
    // "Debug with Clarinet" button in the VSCode extension, which starts
    // the server and sets CLARINET_DEBUG_PORT before running the file.
    include: ["tests/counter.auto.test.ts"],
    testTimeout: 120_000,
    hookTimeout: 30_000,
  },
});
