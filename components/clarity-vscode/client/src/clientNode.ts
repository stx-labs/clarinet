import * as path from "path";
import * as fs from "fs";
import * as os from "os";
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
// CodeLens provider: show "Debug" above every it/test block in
// TypeScript test files that live inside a Clarinet project
// (i.e. a Clarinet.toml exists somewhere in the workspace).
// ---------------------------------------------------------------------------

const TEST_PATTERN = /^\s*(?:it|test)\s*\(\s*(['"`])(.*?)\1/;
// Light filter: only show the button in files that look like Clarinet tests
// (they import from @stacks/clarinet-sdk or use the `simnet` global) or that
// explicitly use the debug API.  This avoids the button appearing in plain
// Vitest projects that happen to be open in the same workspace.
const CLARINET_USAGE_PATTERN = /\b(?:clarinet-sdk|simnet|startDebugServer|connectDebugServer)\b/;

class ClarityDebugTestLensProvider implements vscode.CodeLensProvider {
  /** Cache: workspace root → whether a Clarinet.toml was found anywhere in it. */
  private readonly _cache = new Map<string, boolean>();

  provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
    const text = document.getText();
    // Quick text-content filter before doing filesystem work.
    if (!CLARINET_USAGE_PATTERN.test(text)) return [];

    // Check (cached) that a Clarinet.toml exists in the workspace.
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
    if (!workspaceFolder) return [];
    const root = workspaceFolder.uri.fsPath;
    if (!this._cache.has(root)) {
      const found = vscode.workspace.findFiles(
        new vscode.RelativePattern(workspaceFolder, "**/Clarinet.toml"),
        "**/node_modules/**",
        1,
      );
      // findFiles is async; we kick it off and update the cache for next time.
      // On the first call we default to showing the lens (optimistic) and
      // update after the promise resolves.
      this._cache.set(root, true); // optimistic
      found.then((files) => {
        this._cache.set(root, files.length > 0);
      });
    }
    if (!this._cache.get(root)) return [];

    const lenses: vscode.CodeLens[] = [];
    const lines = text.split("\n");
    for (let i = 0; i < lines.length; i++) {
      const m = TEST_PATTERN.exec(lines[i]);
      if (!m) continue;
      const range = new vscode.Range(i, 0, i, lines[i].length);
      lenses.push(
        new vscode.CodeLens(range, {
          title: "$(debug) Debug",
          command: "clarity.debugTest",
          arguments: [document.uri, m[2]],
        }),
      );
    }
    return lenses;
  }
}

// ---------------------------------------------------------------------------
// Walk up the directory tree from `startDir` until we find a Clarinet.toml,
// stopping at `stopDir` (the workspace root).  Returns undefined if not found.
// ---------------------------------------------------------------------------
async function findManifest(
  startDir: string,
  stopDir: string,
): Promise<string | undefined> {
  let dir = startDir;
  while (true) {
    const candidate = path.join(dir, "Clarinet.toml");
    try {
      await fs.promises.access(candidate, fs.constants.R_OK);
      return candidate;
    } catch {
      // not here
    }
    if (dir === stopDir || path.dirname(dir) === dir) break;
    dir = path.dirname(dir);
  }
  return undefined;
}

// ---------------------------------------------------------------------------
// Locate the clarinet binary.
//
// VSCode extensions on macOS/Linux don't inherit the user's interactive-shell
// PATH (e.g. ~/.cargo/bin is not visible when VSCode is opened from the Dock).
// We check well-known install locations before falling back to a bare name so
// that `spawn` can at least try system PATH as a last resort.
// ---------------------------------------------------------------------------
async function findClarinet(): Promise<string> {
  // Allow the user to pin an explicit path in settings.
  const configured = vscode.workspace
    .getConfiguration("clarity-lsp")
    .get<string>("clarinetPath");
  if (configured) {
    if (path.isAbsolute(configured)) {
      try {
        await fs.promises.access(configured, fs.constants.X_OK);
        return configured;
      } catch {
        debugOutput.warn(`configured clarinetPath is not executable, falling back to auto-detect: ${configured}`);
      }
    } else {
      debugOutput.warn(`configured clarinetPath is not absolute, falling back to auto-detect: ${configured}`);
    }
  }
  const candidates = [
    path.join(os.homedir(), ".cargo", "bin", "clarinet"), // cargo install
    "/opt/homebrew/bin/clarinet",                          // Homebrew (Apple Silicon)
    "/usr/local/bin/clarinet",                             // Homebrew (Intel) / manual
    "/usr/bin/clarinet",
  ];

  for (const bin of candidates) {
    try {
      await fs.promises.access(bin, fs.constants.X_OK);
      return bin;
    } catch {
      // not found at this location, try next
    }
  }

  // Fall back to bare name and let the OS resolve via PATH.
  return "clarinet";
}

// ---------------------------------------------------------------------------
// Try to use `preferred`; if it is already in use let the OS pick a free port.
// ---------------------------------------------------------------------------
function resolvePort(preferred: number): Promise<number> {
  return new Promise((resolve) => {
    const srv = net.createServer();
    srv.listen(preferred, "127.0.0.1", () => {
      srv.close(() => resolve(preferred));
    });
    srv.on("error", () => {
      const fallback = net.createServer();
      fallback.listen(0, "127.0.0.1", () => {
        const addr = fallback.address() as net.AddressInfo;
        fallback.close(() => resolve(addr.port));
      });
    });
  });
}

// Persistent output channel — shows every step of the debug-test command.
const debugOutput = vscode.window.createOutputChannel("Clarinet Debug", { log: true });

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

  // CodeLens: "Debug" buttons in TypeScript test files.
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
        debugOutput.show(true);
        debugOutput.info(`clarity.debugTest triggered`);
        debugOutput.info(`  file: ${fileUri.fsPath}`);
        debugOutput.info(`  test: ${testName}`);

        try {
          const workspaceFolder = vscode.workspace.getWorkspaceFolder(fileUri);
          if (!workspaceFolder) {
            throw new Error("no workspace folder found for this file");
          }
          debugOutput.info(`  workspace: ${workspaceFolder.uri.fsPath}`);

          const [dapPort, sdkPort, clarinet] = await Promise.all([
            resolvePort(7777),
            resolvePort(7778),
            findClarinet(),
          ]);
          debugOutput.info(`  clarinet: ${clarinet}`);
          debugOutput.info(`  dap port: ${dapPort}  sdk port: ${sdkPort}`);

          const testDir = path.dirname(fileUri.fsPath);
          const manifestPath = await findManifest(testDir, workspaceFolder.uri.fsPath);
          if (!manifestPath) {
            throw new Error(
              "Could not find Clarinet.toml near this file. Make sure the test lives inside a Clarinet project.",
            );
          }
          const projectRoot = path.dirname(manifestPath);
          const relativeFile = path.relative(projectRoot, fileUri.fsPath);
          debugOutput.info(`  manifest: ${manifestPath}`);
          debugOutput.info(`  relativeFile: ${relativeFile}`);

          // Spawn the clarinet dap server.
          const args = [
            "dap",
            "--dap-port", String(dapPort),
            "--sdk-port", String(sdkPort),
            "--manifest", manifestPath,
          ];
          debugOutput.info(`  spawning: ${clarinet} ${args.join(" ")}`);
          const dapProcess = spawn(clarinet, args, {
            cwd: projectRoot,
            stdio: ["ignore", "ignore", "pipe"],
          });

          dapProcess.on("error", (err) => {
            debugOutput.error(`  spawn error: ${err.message}`);
            vscode.window.showErrorMessage(`Failed to start clarinet dap: ${err.message}`);
          });

          // Forward all server stderr to the output channel.
          dapProcess.stderr!.on("data", (chunk: Buffer) => {
            for (const line of chunk.toString("utf8").trimEnd().split("\n")) {
              debugOutput.info(`  [dap] ${line}`);
            }
          });

          // Wait for the READY signal.
          debugOutput.info("  waiting for CLARINET_DAP_SDK_READY...");
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
          });
          debugOutput.info("  server ready");

          // Attach the VSCode debugger directly to the TCP port via `debugServer`.
          // This bypasses the debug.js adapter relay: VSCode opens a raw socket
          // to the clarinet dap server and speaks DAP directly, which is exactly
          // what the server expects.  The `type: "clarinet"` keeps breakpoint
          // support active (VSCode knows .clar files are relevant).
          debugOutput.info(`  calling vscode.debug.startDebugging on port ${dapPort}...`);
          const started = await vscode.debug.startDebugging(workspaceFolder, {
            type: "clarinet",
            request: "attach",
            name: `Debug: ${testName}`,
            debugServer: dapPort,
          });
          debugOutput.info(`  startDebugging returned: ${started}`);

          if (!started) {
            dapProcess.kill();
            throw new Error("vscode.debug.startDebugging returned false — check the Debug Console for adapter errors");
          }

          // Kill only the server owned by this command's debug session.
          const sessionName = `Debug: ${testName}`;
          const disposable = vscode.debug.onDidTerminateDebugSession((session) => {
            if (session.name !== sessionName) return;
            debugOutput.info("  debug session terminated — killing dap server");
            dapProcess.kill();
            disposable.dispose();
          });

          // Run the selected test file directly. Passing the file path avoids
          // relying on project-specific Vitest include patterns or extra config files.
          // Single-quote escaping ensures no shell metacharacters in the file path
          // or test name are interpreted. CLARINET_DEBUG_PORT is passed via the
          // terminal env rather than inline in the shell command.
          const shellQuote = (s: string) => "'" + s.replace(/'/g, "'\\''") + "'";
          const cmd = `npx vitest run ${shellQuote(relativeFile)} -t ${shellQuote(testName)} --testTimeout=0`;
          debugOutput.info(`  opening terminal: ${cmd}`);
          const terminal = vscode.window.createTerminal({
            name: `Clarinet Debug: ${testName}`,
            cwd: projectRoot,
            env: { CLARINET_DEBUG_PORT: String(sdkPort) },
          });
          terminal.show();
          terminal.sendText(cmd);
          debugOutput.info("  done — test is running in terminal");
        } catch (err) {
          const msg = err instanceof Error ? err.message : String(err);
          debugOutput.error(`  ERROR: ${msg}`);
          vscode.window.showErrorMessage(`Clarinet debugTest: ${msg}`);
        }
      },
    ),
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) return undefined;
  return client.stop();
}
