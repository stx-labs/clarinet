import * as vscode from "vscode";
import { ExtensionContext } from "vscode";
import { LanguageClientOptions } from "vscode-languageclient";

import { initVFS } from "./customVFS";
import type { LanguageClient } from "./types";

const { window, workspace } = vscode;

const SUPPRESSED_WARNINGS_KEY = "clarityLsp.suppressedWarnings";

function isWarningSuppressed(context: ExtensionContext, id: string): boolean {
  const suppressed = context.globalState.get<string[]>(
    SUPPRESSED_WARNINGS_KEY,
    [],
  );
  return suppressed.includes(id);
}

async function suppressWarning(
  context: ExtensionContext,
  id: string,
): Promise<void> {
  const suppressed = context.globalState.get<string[]>(
    SUPPRESSED_WARNINGS_KEY,
    [],
  );
  if (!suppressed.includes(id)) {
    await context.globalState.update(SUPPRESSED_WARNINGS_KEY, [
      ...suppressed,
      id,
    ]);
  }
}

async function showWarningOnce(
  context: ExtensionContext,
  id: string,
  message: string,
): Promise<void> {
  if (isWarningSuppressed(context, id)) {
    return;
  }

  const result = await vscode.window.showWarningMessage(
    message,
    "Don't show again",
  );

  if (result === "Don't show again") {
    await suppressWarning(context, id);
  }
}

declare const __DEV_MODE__: boolean | undefined;

function getConfig() {
  const config = workspace.getConfiguration("clarity-lsp");
  if (__DEV_MODE__) {
    config.update("debug.logRequestsTimings", true);
  }
  return config;
}

export const clientOpts: LanguageClientOptions = {
  documentSelector: [{ language: "clarity" }, { language: "toml" }],
  diagnosticCollectionName: "Clarity LSP",
  progressOnInitialization: false,
  traceOutputChannel: vscode.window.createOutputChannel(
    "Clarity Language Server Trace",
  ),
  initializationOptions: JSON.stringify(getConfig()),
};

export async function initClient(
  context: ExtensionContext,
  client: LanguageClient,
) {
  if (__DEV_MODE__) {
    // update vscode default config in dev
    if (workspace.getConfiguration("files").autoSave !== "off") {
      vscode.commands.executeCommand("workbench.action.toggleAutoSave");
    }
    if (window.activeColorTheme.kind !== 2) {
      vscode.commands.executeCommand("workbench.action.toggleLightDarkThemes");
    }
  }

  workspace.onDidChangeConfiguration(async () => {
    let requireReload = false;
    const newConfig = getConfig();
    [
      "completion",
      "completionSmartParenthesisWrap",
      "completionIncludeNativePlaceholders",
      "formatting",
      "hover",
      "documentSymbols",
      "goToDefinition",
      "staticCostAnalysis",
    ].forEach((k) => {
      if (newConfig[k] !== config[k]) requireReload = true;
    });

    config = newConfig;

    if (requireReload) {
      const userResponse = await vscode.window.showInformationMessage(
        "Changing Clarity configuration requires to reload VSCode",
        "Reload VSCode",
      );

      if (userResponse) {
        const command = "workbench.action.reloadWindow";
        await vscode.commands.executeCommand(command);
      }
    }
  });

  let config = getConfig();

  // Register no-op command for static cost code lens (display-only, no action on click)
  context.subscriptions.push(
    vscode.commands.registerCommand("clarity.staticCostLens", () => {}),
  );
  client.onNotification(
    "clarity/noManifestWarning",
    async (message: string) => {
      await showWarningOnce(
        context,
        "clarity-lsp.no-clarinet-toml-associated",
        message,
      );
    },
  );

  initVFS(client);
  try {
    await client.start();
  } catch (err) {
    if (err instanceof Error && err.message === "worker timeout") {
      vscode.window.showWarningMessage(
        "Clarity Language Server failed to start",
      );
    }
  }
}
