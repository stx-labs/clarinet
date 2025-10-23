/// <reference types="vitest" />
import { defineConfig } from "vite";

export default defineConfig({
  test: {
    silent: "passed-only",
    pool: "forks",
    poolOptions: {
      forks: { singleFork: true },
    },
    include: ["./tests/**/*.test.ts", "./vitest-helpers/tests/**/*.test.ts"],
  },
});
