import path from "node:path";
import url from "node:url";
import { parseArgs } from "node:util";

type ParsedValues = Record<string, string | boolean | undefined>;

function readString(values: ParsedValues, flags: string[]) {
  for (const flag of flags) {
    const value = values[flag];
    if (typeof value === "string") return value;
  }
}

function readBoolean(values: ParsedValues, flags: string[]) {
  for (const flag of flags) {
    if (values[`no-${flag}`] === true) return false;
    const value = values[flag];
    if (value !== undefined) return value !== "false";
  }
}

export function getClarinetVitestsArgv() {
  // clarinet options are the ones passed after the separator: `vitest run -- --coverage`
  const separator = process.argv.indexOf("--");
  const args = separator === -1 ? [] : process.argv.slice(separator + 1);

  const { values } = parseArgs({
    args,
    strict: false,
    allowPositionals: true,
    options: {
      "manifest-path": { type: "string" },
      manifest: { type: "string" },
      "coverage-filename": { type: "string" },
      "cov-file": { type: "string" },
      "costs-filename": { type: "string" },
      "costs-file": { type: "string" },
      "boot-contracts-path": { type: "string" },
    },
  });

  const includeBootContracts = readBoolean(values, ["include-boot-contracts"]);
  const bootContractsPath = readString(values, ["boot-contracts-path"]);

  return {
    manifestPath: readString(values, ["manifest-path", "manifest"]) ?? "./Clarinet.toml",
    initBeforeEach: readBoolean(values, ["init-before-each"]) ?? true,
    coverage: readBoolean(values, ["coverage", "cov"]) ?? false,
    costs: readBoolean(values, ["costs", "cost"]) ?? false,
    coverageFilename: readString(values, ["coverage-filename", "cov-file"]) ?? "lcov.info",
    costsFilename: readString(values, ["costs-filename", "costs-file"]) ?? "costs-reports.json",
    // only set when passed, so they don't override a value set in the vitest config
    ...(includeBootContracts !== undefined && { includeBootContracts }),
    ...(bootContractsPath !== undefined && { bootContractsPath }),
  };
}

// Derive the package root from this module's own location
const sdkDir = path.dirname(url.fileURLToPath(import.meta.url));

// sdkDir is /dist/esm/node/src/vitest, hence the ../../../../../
export const vitestHelpersPath = path.join(sdkDir, "../../../../../vitest-helpers/src/");
export const vitestSetupFilePath = path.join(vitestHelpersPath, "vitest.setup.ts");
