import * as vscode from "vscode";
import { ExtensionContext } from "vscode";
import { LanguageClientOptions } from "vscode-languageclient";

import { initVFS } from "./customVFS";
import { InsightsViewProvider } from "./Views/InsightsViewProvider";
import { CostDetailsViewProvider } from "./Views/CostDetailsViewProvider";
import type { InsightsData, LanguageClient, CostDetailsData } from "./types";

const { window, workspace } = vscode;

function isValidInsight(data: InsightsData): data is InsightsData {
  return !!data && !!data.fnName && !!data.fnType && Array.isArray(data.fnArgs);
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

  let config = getConfig();

  /* clarity insight webview */
  const insightsViewProvider = new InsightsViewProvider(context.extensionUri);

  context.subscriptions.push(
    window.registerWebviewViewProvider(
      InsightsViewProvider.viewType,
      insightsViewProvider,
    ),
  );

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

  /* clariy lsp */
  async function changeSelectionHandler(
    e: vscode.TextEditorSelectionChangeEvent,
  ) {
    if (!e?.textEditor?.document) return;
    const path = e.textEditor.document.uri.toString();
    const { line, character } = e.selections[0].active;

    try {
      const res = await client.sendRequest("clarity/getFunctionAnalysis", {
        path,
        line: line + 1,
        char: character + 1,
      });
      if (!res) throw new Error("empty res");

      const insights = JSON.parse(res as string);
      if (!isValidInsight(insights)) throw new Error("Invalid insights");
      insightsViewProvider.insights = insights;
    } catch (err) {
      insightsViewProvider.insights = null;
      if (err instanceof Error && err.message === "empty res") return;
      console.warn(err);
    }
  }

  // Register cost details command
  context.subscriptions.push(
    vscode.commands.registerCommand(
      "clarity.showCostDetails",
      async (args: any) => {
        // VSCode code lens commands pass arguments as an array, so extract the first element if needed
        const rawParams = Array.isArray(args) && args.length > 0 ? args[0] : args;

        // Validate and extract parameters
        if (!rawParams || typeof rawParams !== "object") {
          vscode.window.showErrorMessage(
            "Invalid arguments passed to cost details command",
          );
          return;
        }

        // Helper function to extract value from serde_json::Value wrapper
        const extractValue = (value: any): any => {
          if (value === null || value === undefined) {
            return value;
          }
          // Handle serde_json::Value number wrapper: {$serde_json::private::Number: '4'}
          if (typeof value === "object" && "$serde_json::private::Number" in value) {
            return parseInt(value["$serde_json::private::Number"], 10);
          }
          // Handle regular numbers
          if (typeof value === "number") {
            return value;
          }
          // Handle strings that should be numbers
          if (typeof value === "string") {
            const parsed = parseInt(value, 10);
            return isNaN(parsed) ? value : parsed;
          }
          // Handle circular references by converting to JSON string first
          try {
            const jsonStr = JSON.stringify(value);
            const parsed = JSON.parse(jsonStr);
            // Check if it's a wrapped number
            if (typeof parsed === "object" && parsed !== null && "$serde_json::private::Number" in parsed) {
              return parseInt(parsed["$serde_json::private::Number"], 10);
            }
            return parsed;
          } catch {
            return value;
          }
        };

        // Extract and validate parameters
        const lineValue = extractValue(rawParams.line);
        const lineNumber = typeof lineValue === "number" ? lineValue : parseInt(String(lineValue), 10);

        if (isNaN(lineNumber) || lineNumber < 0) {
          console.error("Invalid line number:", rawParams.line, "extracted:", lineValue, "from args:", args);
          vscode.window.showErrorMessage(
            `Invalid line number: ${rawParams.line}`,
          );
          return;
        }

        const functionName = extractValue(rawParams.function);
        const pathValue = extractValue(rawParams.path);

        const params = {
          path: String(pathValue || ""),
          line: lineNumber,
          function: String(functionName || ""),
        };

        console.log("Cost details request params:", params);
        console.log("Raw params:", rawParams);

        if (!params.path || !params.function) {
          vscode.window.showErrorMessage(
            "Missing required parameters (path or function)",
          );
          return;
        }

        try {
          const res = await client.sendRequest("clarity/getCostDetails", {
            path: params.path,
            line: params.line,
            function: params.function,
          });
          if (!res) {
            vscode.window.showWarningMessage(
              "No cost details available for this function",
            );
            return;
          }

          const costDetails: CostDetailsData = JSON.parse(res as string);
          costDetails.path = params.path;
          CostDetailsViewProvider.createOrShow(context.extensionUri, costDetails);
        } catch (err) {
          console.error("Error fetching cost details:", err);
          vscode.window.showErrorMessage(
            "Failed to fetch cost details. See output for details.",
          );
        }
      },
    ),
  );

  initVFS(client);
  try {
    await client.start();

    if (config.panels["insights-panel"]) {
      if (window.activeTextEditor) {
        const { document } = window.activeTextEditor;
        if (document.languageId !== "clarity") return;
        insightsViewProvider.fileName = document;
      }

      context.subscriptions.push(
        window.onDidChangeTextEditorSelection(changeSelectionHandler),
      );
    }
  } catch (err) {
    if (err instanceof Error && err.message === "worker timeout") {
      vscode.window.showWarningMessage(
        "Clarity Language Server failed to start",
      );
    }
  }
}
