#!/usr/bin/node

import { spawn } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";

// directory of the current file
const rootDir = new URL(".", import.meta.url).pathname;

/**
 * build clarinet-sdk-wasm
 */
async function build_wasm_sdk() {
  const isDev = process.argv.includes("--dev");
  const profileFlag = isDev ? "--dev" : "--release";
  console.log(`Building wasm SDK (${profileFlag})`);

  console.log("Deleting pkg-node");
  await rmIfExists(path.join(rootDir, "pkg-node"));
  console.log("Deleting pkg-browser");
  await rmIfExists(path.join(rootDir, "pkg-browser"));

  await Promise.all([
    execCommand("wasm-pack", [
      "build",
      profileFlag,
      "--scope",
      "stacks",
      "--out-dir",
      "pkg-node",
      "--target",
      "nodejs",
    ]),
    execCommand("wasm-pack", [
      "build",
      profileFlag,
      "--scope",
      "stacks",
      "--out-dir",
      "pkg-browser",
      "--target",
      "web",
      "--features",
      "web",
    ]),
  ]);

  await updatePackageName();
  await includeNodeSnippets();
}

// wasm-pack omits `snippets/` from the generated package.json `files` list, so
// it would be stripped on publish. Patch it back in.
async function includeNodeSnippets() {
  const pkgJsonPath = path.join(rootDir, "pkg-node/package.json");
  const pkgRaw = await fs.readFile(pkgJsonPath, "utf-8");
  const pkg = JSON.parse(pkgRaw);
  pkg.files = pkg.files ?? [];
  if (!pkg.files.includes("snippets")) {
    pkg.files.push("snippets");
  }
  await fs.writeFile(pkgJsonPath, JSON.stringify(pkg, null, 2) + "\n", "utf-8");
  console.log("✅ pkg-node/package.json files list patched to include snippets");

  // Sanity check: sync_http.cjs is self-contained (worker + layout inlined)
  // precisely so that nothing else needs to be copied here — wasm-bindgen
  // emits it as the lone snippet because Rust imports from it.
  const snippetsDir = path.join(rootDir, "pkg-node/snippets");
  const entries = await fs.readdir(snippetsDir, { recursive: true });
  if (!entries.some((e) => e.endsWith("sync_http.cjs"))) {
    throw new Error(
      "sync_http.cjs not found under pkg-node/snippets/; wasm-bindgen module path may be wrong",
    );
  }
  console.log("✅ pkg-node/snippets/.../sync_http.cjs in place");
}

/**
 * execCommand
 * @param {string} command
 * @param {string[]} args
 * @returns
 */
export const execCommand = async (command, args, cwd = rootDir) => {
  return new Promise((resolve, reject) => {
    const childProcess = spawn(command, args, {
      cwd,
    });
    childProcess.stdout.on("data", (data) => {
      process.stdout.write(data.toString());
    });
    childProcess.stderr.on("data", (data) => {
      process.stderr.write(data.toString());
    });
    childProcess.on("error", (error) => {
      reject(error);
    });
    childProcess.on("exit", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`❌ Command exited with code ${code}.`));
      }
    });
  });
};

/**
 * rmIfExists
 * @param {string} dirPath
 */
async function rmIfExists(dirPath) {
  try {
    await fs.rm(dirPath, { recursive: true, force: true });
  } catch (error) {
    if (error.code !== "ENOENT") {
      throw error;
    }
  }
}

/**
 * updatePackageName
 */
async function updatePackageName() {
  const filePath = path.join(rootDir, "pkg-browser/package.json");

  const fileData = await fs.readFile(filePath, "utf-8");
  const updatedData = fileData.replace(
    '"name": "@stacks/clarinet-sdk-wasm"',
    '"name": "@stacks/clarinet-sdk-wasm-browser"',
  );
  await fs.writeFile(filePath, updatedData, "utf-8");
  console.log("✅ pkg-browser/package.json name updated");
}

try {
  await build_wasm_sdk();
  console.log("\n✅ Project successfully built.\n🚀 Ready to publish.");
  console.log("Run the following commands to publish");
  console.log("\n```");
  console.log("$ pnpm run publish:sdk-wasm");
  console.log("```\n");
} catch (error) {
  console.error("❌ Error building:", error);
  throw error;
}
