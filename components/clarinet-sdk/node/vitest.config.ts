import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    silent: "passed-only",
    pool: "forks",
    maxWorkers: 1,
    include: ["./tests/**/*.test.ts", "./vitest-helpers/tests/**/*.test.ts"],
  },
});
