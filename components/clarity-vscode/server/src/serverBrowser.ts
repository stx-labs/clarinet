import {
  createConnection,
  BrowserMessageReader,
  BrowserMessageWriter,
} from "vscode-languageserver/browser";

import { isInitWasmMessage } from "../../shared/lspWorkerProtocol";
import { initSync, LspVscodeBridge } from "./clarity-lsp-browser/lsp-browser";
import { initConnection } from "./common";

const INIT_WASM_TIMEOUT = 10_000;

// This worker runs from a blob URL on an isolated origin, so neither
// `self.location` nor a build-time constant can locate the extension assets:
// the host differs between the VS Code Marketplace, Open VSX and local
// development. Only the client knows the real URL, and it hands it over here.
function waitForWasmURL(): Promise<string> {
  return new Promise((resolve, reject) => {
    function onInitWasm({ data }: MessageEvent) {
      if (!isInitWasmMessage(data)) return;
      self.removeEventListener("message", onInitWasm);
      clearTimeout(timeout);
      resolve(data.wasmURL);
    }

    const timeout = setTimeout(() => {
      self.removeEventListener("message", onInitWasm);
      reject(new Error("timed out waiting for the wasm url from the client"));
    }, INIT_WASM_TIMEOUT);

    // registered synchronously, before the worker yields for the first time,
    // so that the client message can not be missed
    self.addEventListener("message", onInitWasm);
  });
}

async function startServer() {
  // the connection can only be opened once the init message has been consumed:
  // `BrowserMessageReader` would otherwise forward it to the language server as
  // an unknown notification. The client doesn't send any LSP message before it
  // receives `serverWorkerReady`, so nothing can be lost in the meantime.
  const wasmURL = await waitForWasmURL();

  const wasmModule = fetch(wasmURL, {
    headers: {
      "Accept-Encoding": "Accept-Encoding: gzip",
    },
  }).then((wasm) => wasm.arrayBuffer());

  const connection = createConnection(
    new BrowserMessageReader(self),
    new BrowserMessageWriter(self),
  );

  initSync({ module: await wasmModule });

  const bridge = new LspVscodeBridge(
    connection.sendDiagnostics,
    connection.sendNotification,
    connection.sendRequest,
  );

  initConnection(connection, bridge);
  connection.sendNotification("serverWorkerReady");
}

startServer().catch((err) => {
  // the client can only report its own generic worker timeout, so make the
  // actual cause visible instead of leaving an unhandled rejection behind
  console.error("failed to start the clarity language server", err);
});
