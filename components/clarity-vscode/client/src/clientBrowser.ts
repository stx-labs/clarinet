import { ExtensionContext, Uri } from "vscode";
import { LanguageClient } from "vscode-languageclient/browser";

import { INIT_WASM_METHOD } from "../../shared/lspWorkerProtocol";
import type { InitWasmMessage } from "../../shared/lspWorkerProtocol";
import { clientOpts, initClient } from "./common";

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  const serverMain = Uri.joinPath(
    context.extensionUri,
    "server/dist/serverBrowser.js",
  );

  const worker = new Worker(serverMain.toString(true));

  // VS Code starts this worker from a blob URL on an isolated origin, so it
  // can't resolve the wasm module relatively to itself. `extensionUri` is the
  // only reliable source for it: it points at whichever registry served the
  // extension (Marketplace, Open VSX, or the local dev server).
  const initWasm: InitWasmMessage = {
    method: INIT_WASM_METHOD,
    wasmURL: Uri.joinPath(
      context.extensionUri,
      "server/dist/lsp-browser_bg.wasm",
    ).toString(true),
  };
  worker.postMessage(initWasm);

  let serverWorkerReady: ((value: unknown) => void) | null = null;
  let workerTimeout: ReturnType<typeof setTimeout> | null = null;
  const serverWorkerPromise = new Promise((resolve, reject) => {
    serverWorkerReady = resolve;
    workerTimeout = setTimeout(() => {
      reject(new Error("worker timeout"));
    }, 10000);
  });

  worker.addEventListener(
    "message",
    function onServerWorkerReady(e: MessageEvent) {
      if (e.data.method !== "serverWorkerReady") return;
      worker.removeEventListener("message", onServerWorkerReady);
      serverWorkerReady!(true);
      clearTimeout(workerTimeout!);
    },
  );

  await serverWorkerPromise;
  client = new LanguageClient("clarity-lsp", "Clarity LSP", worker, clientOpts);

  await initClient(context, client);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) return undefined;
  return client.stop();
}
