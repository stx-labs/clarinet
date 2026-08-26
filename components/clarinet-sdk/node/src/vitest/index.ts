import path from "node:path";
import url from "node:url";

import yargs from "yargs";
import { hideBin } from "yargs/helpers";

export function getClarinetVitestsArgv() {
  const argv = hideBin(process.argv);
  const topLevel = yargs(argv).argv;

  // @ts-ignore
  return yargs(topLevel._)
    .option("manifest-path", {
      alias: "manifest",
      type: "string",
      default: "./Clarinet.toml",
    })
    .option("init-before-each", {
      description: "Reinitialize the Clarinet state before each test",
      type: "boolean",
      default: true,
    })
    .option("coverage", {
      alias: "cov",
      type: "boolean",
      default: false,
    })
    .option("costs", {
      alias: "cost",
      type: "boolean",
      default: false,
    })
    .option("coverage-filename", {
      alias: "cov-file",
      type: "string",
      default: "lcov.info",
    })
    .option("costs-filename", {
      alias: "costs-file",
      type: "string",
      default: "costs-reports.json",
    }).argv;
}

// Derive the package root from this module's own location
const sdkDir = path.dirname(url.fileURLToPath(import.meta.url));

// sdkDir is /dist/esm/node/src/vitest, hence the ../../../../../
export const vitestHelpersPath = path.join(sdkDir, "../../../../../vitest-helpers/src/");
export const vitestSetupFilePath = path.join(vitestHelpersPath, "vitest.setup.ts");
