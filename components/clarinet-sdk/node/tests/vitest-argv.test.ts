import { afterEach, describe, expect, it } from "vitest";

import { getClarinetVitestsArgv } from "../src/vitest";

const originalArgv = process.argv;

function parse(...args: string[]) {
  process.argv = ["node", "vitest.mjs", ...args];
  return getClarinetVitestsArgv();
}

const defaults = {
  manifestPath: "./Clarinet.toml",
  initBeforeEach: true,
  coverage: false,
  costs: false,
  coverageFilename: "lcov.info",
  costsFilename: "costs-reports.json",
};

afterEach(() => {
  process.argv = originalArgv;
});

describe("getClarinetVitestsArgv", () => {
  it("returns the defaults without arguments", () => {
    expect(parse("run")).toStrictEqual(defaults);
  });

  it("only reads the arguments after the separator", () => {
    expect(parse("run", "--coverage", "--manifest", "nope.toml")).toStrictEqual(defaults);
    expect(parse("run", "tests/a.test.ts", "-t", "a test", "--", "--costs")).toStrictEqual({
      ...defaults,
      costs: true,
    });
    expect(parse("run", "--", "--coverage", "--", "--costs")).toStrictEqual({
      ...defaults,
      coverage: true,
    });
  });

  it("parses the options and their aliases", () => {
    const expected = {
      manifestPath: "./foo/Clarinet.toml",
      initBeforeEach: true,
      coverage: true,
      costs: true,
      coverageFilename: "x.info",
      costsFilename: "y.json",
    };
    expect(
      parse(
        "--",
        "--manifest-path",
        "./foo/Clarinet.toml",
        "--coverage",
        "--costs",
        "--coverage-filename=x.info",
        "--costs-filename=y.json",
      ),
    ).toStrictEqual(expected);
    expect(
      parse(
        "--",
        "--manifest=./foo/Clarinet.toml",
        "--cov",
        "--cost",
        "--cov-file",
        "x.info",
        "--costs-file",
        "y.json",
      ),
    ).toStrictEqual(expected);
  });

  it("negates boolean flags", () => {
    expect(parse("--", "--no-init-before-each")).toStrictEqual({
      ...defaults,
      initBeforeEach: false,
    });
    expect(parse("--", "--init-before-each=false", "--coverage=false")).toStrictEqual({
      ...defaults,
      initBeforeEach: false,
    });
  });

  it("only sets the boot contracts options when passed", () => {
    expect(
      parse("--", "--include-boot-contracts", "--boot-contracts-path", "./boot"),
    ).toStrictEqual({ ...defaults, includeBootContracts: true, bootContractsPath: "./boot" });
  });
});
