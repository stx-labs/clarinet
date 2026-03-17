import * as path from "path";

import { BrowserType, runTests } from "@vscode/test-web";

function getArgv(name: string) {
  const arg = process.argv.find((a) => a.startsWith(`--${name}=`));
  if (!arg) return undefined;
  return arg.split("=")[1];
}

function isValidBrowserType(browserType: unknown): browserType is BrowserType {
  return (
    !!browserType &&
    typeof browserType === "string" &&
    ["chromium", "firefox", "webkit"].includes(browserType)
  );
}

async function main() {
  const extensionDevelopmentPath = path.resolve(__dirname, "../../../");
  const extensionTestsPath = path.resolve(__dirname, "./suite/index");
  const folderPath = path.resolve(__dirname, "../../../test-data");

  try {
    const waitForDebugger = Number(getArgv("waitForDebugger"));
    const browserType = getArgv("browserType") || "chromium";
    if (!isValidBrowserType(browserType))
      throw new Error("invalid browserType");

    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      folderPath,
      browserType,
      waitForDebugger,
      devTools: !process.env.CI,
      headless: !!process.env.CI,
    });
  } catch (err) {
    console.error(err);
    console.error("Failed to run tests");
    process.exit(1);
  }
}

main();
