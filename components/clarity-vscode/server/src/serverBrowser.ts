import {
  createConnection,
  BrowserMessageReader,
  BrowserMessageWriter,
} from "vscode-languageserver/browser";

import { initSync, LspVscodeBridge } from "./clarity-lsp-browser/lsp-browser";
import { initConnection } from "./common";

declare const __EXTENSION_URL__: string;

(async function startServer() {
  const wasmURL = new URL("server/dist/lsp-browser_bg.wasm", __EXTENSION_URL__);

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
})();
