import * as path from "path";
import * as net from "net";
import { spawn } from "child_process";
import { ExtensionContext } from "vscode";
import * as vscode from "vscode";

import {
  LanguageClient,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import { initClient, clientOpts } from "./common";

// ---------------------------------------------------------------------------
// CodeLens provider: show "Debug with Clarinet" above test blocks in files
// that use connectDebugServer / startDebugServer from @stacks/clarinet-sdk.
// ---------------------------------------------------------------------------

// Only match tests that connect to an externally-managed server. Tests that
// call `startDebugServer` spin up their own process and don't need the CodeLens.
const DEBUG_PATTERN = /\b(?:connectDebugServer|DebugClient)\b/;
const TEST_PATTERN = /^\s*(?:it|test)\s*\(\s*(['"`])(.*?)\1/;

class ClarityDebugTestLensProvider implements vscode.CodeLensProvider {
  provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
    const text = document.getText();
    if (!DEBUG_PATTERN.test(text)) return [];

    const lenses: vscode.CodeLens[] = [];
    const lines = text.split("\n");
    for (let i = 0; i < lines.length; i++) {
      const m = TEST_PATTERN.exec(lines[i]);
      if (!m) continue;
      const range = new vscode.Range(i, 0, i, lines[i].length);
      lenses.push(
        new vscode.CodeLens(range, {
          title: "$(debug) Debug with Clarinet",
          command: "clarity.debugTest",
          arguments: [document.uri, m[2]],
        }),
      );
    }
    return lenses;
  }
}

// ---------------------------------------------------------------------------
// Find a free local TCP port by briefly binding to port 0.
// ---------------------------------------------------------------------------
function getFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, "127.0.0.1", () => {
      const addr = srv.address() as net.AddressInfo;
      srv.close(() => resolve(addr.port));
    });
    srv.on("error", reject);
  });
}

let client: LanguageClient;
export async function activate(context: ExtensionContext) {
  const serverModule = context.asAbsolutePath("server/dist/serverNode.js");
  const debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions,
    },
  };

  client = new LanguageClient(
    "clarity-lsp",
    "Clarity LSP",
    serverOptions,
    clientOpts,
  );
  initClient(context, client);

  // CodeLens: "Debug with Clarinet" buttons in TypeScript test files.
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      { language: "typescript" },
      new ClarityDebugTestLensProvider(),
    ),
  );

  // Command: spawn clarinet dap, attach VSCode, run the test in a terminal.
  context.subscriptions.push(
    vscode.commands.registerCommand(
      "clarity.debugTest",
      async (fileUri: vscode.Uri, testName: string) => {
        const workspaceFolder = vscode.workspace.getWorkspaceFolder(fileUri);
        if (!workspaceFolder) {
          vscode.window.showErrorMessage("clarity.debugTest: no workspace folder found");
          return;
        }

        const [dapPort, sdkPort] = await Promise.all([getFreePort(), getFreePort()]);
        const manifestPath = path.join(workspaceFolder.uri.fsPath, "Clarinet.toml");
        const relativeFile = path.relative(workspaceFolder.uri.fsPath, fileUri.fsPath);

        // Spawn the clarinet dap server. Both listeners are bound before
        // CLARINET_DAP_SDK_READY is printed, so VSCode can attach right away.
        const dapProcess = spawn(
          "clarinet",
          [
            "dap",
            "--dap-port", String(dapPort),
            "--sdk-port", String(sdkPort),
            "--manifest", manifestPath,
          ],
          { cwd: workspaceFolder.uri.fsPath, stdio: ["ignore", "ignore", "pipe"] },
        );

        dapProcess.on("error", (err) => {
          vscode.window.showErrorMessage(`Failed to start clarinet dap: ${err.message}`);
        });

        // Wait for the server to be ready before attaching.
        await new Promise<void>((resolve, reject) => {
          const token = `CLARINET_DAP_SDK_READY:${sdkPort}`;
          let buf = "";
          const timeout = setTimeout(
            () => reject(new Error("clarinet dap server did not start within 15 s")),
            15_000,
          );
          dapProcess.stderr!.on("data", (chunk: Buffer) => {
            buf += chunk.toString("utf8");
            if (buf.includes(token)) {
              clearTimeout(timeout);
              resolve();
            }
          });
          dapProcess.on("exit", (code) => {
            clearTimeout(timeout);
            reject(new Error(`clarinet dap exited with code ${code}`));
          });
        }).catch((err: Error) => {
          vscode.window.showErrorMessage(`clarinet dap: ${err.message}`);
          dapProcess.kill();
          return Promise.reject(err);
        });

        // Attach the VSCode debugger — no launch.json required.
        const started = await vscode.debug.startDebugging(workspaceFolder, {
          type: "clarinet",
          request: "attach",
          name: `Debug: ${testName}`,
          port: dapPort,
        });

        if (!started) {
          vscode.window.showErrorMessage("Failed to attach Clarinet debugger");
          dapProcess.kill();
          return;
        }

        // Kill the server when the debug session ends.
        const disposable = vscode.debug.onDidTerminateDebugSession(() => {
          dapProcess.kill();
          disposable.dispose();
        });
        context.subscriptions.push(disposable);

        // Run the specific test in a terminal; it will connect to sdk-port.
        const terminal = vscode.window.createTerminal({
          name: `Clarinet Debug: ${testName}`,
          cwd: workspaceFolder.uri.fsPath,
          env: { CLARINET_DEBUG_PORT: String(sdkPort) },
        });
        terminal.show();
        terminal.sendText(
          `npx vitest run ${relativeFile} -t ${JSON.stringify(testName)}`,
        );
      },
    ),
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) return undefined;
  return client.stop();
}
