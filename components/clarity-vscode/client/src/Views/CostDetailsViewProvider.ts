import * as vscode from "vscode";
import {
  costDetailsBody,
  costDetailsStyles,
  head,
  getNonce,
} from "../utils/html";
import type { CostDetailsData } from "../types";

export class CostDetailsViewProvider {
  private static _panels: Map<string, vscode.WebviewPanel> = new Map();

  public static createOrShow(
    extensionUri: vscode.Uri,
    costDetails: CostDetailsData,
  ) {
    const functionKey = `${costDetails.path}:${costDetails.function}`;
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If panel already exists, reveal it
    if (CostDetailsViewProvider._panels.has(functionKey)) {
      const panel = CostDetailsViewProvider._panels.get(functionKey)!;
      panel.reveal(column);
      panel.webview.html = CostDetailsViewProvider._getHtmlForWebview(
        panel.webview,
        extensionUri,
        costDetails,
      );
      return;
    }

    // Otherwise, create a new panel
    const panel = vscode.window.createWebviewPanel(
      "clarityCostDetails",
      `Cost Details: ${costDetails.function}`,
      column || vscode.ViewColumn.One,
      {
        enableScripts: false,
        localResourceRoots: [extensionUri],
      },
    );

    panel.webview.html = CostDetailsViewProvider._getHtmlForWebview(
      panel.webview,
      extensionUri,
      costDetails,
    );

    // Clean up when panel is closed
    panel.onDidDispose(
      () => {
        CostDetailsViewProvider._panels.delete(functionKey);
      },
      null,
    );

    CostDetailsViewProvider._panels.set(functionKey, panel);
  }

  private static _getHtmlForWebview(
    webview: vscode.Webview,
    extensionUri: vscode.Uri,
    costDetails: CostDetailsData,
  ) {
    const nonce = getNonce();

    const styleSrc = vscode.Uri.joinPath(
      extensionUri,
      "./assets/styles/costDetailsView.css",
    );

    const additionalStyles = costDetailsStyles(costDetails.percentages);

    return /* html */ `<!DOCTYPE html>
      <html lang="en">
      ${head(webview, styleSrc, nonce, additionalStyles)}

      <body>
        ${costDetailsBody(costDetails)}
      </body>
      </html>`;
  }
}
