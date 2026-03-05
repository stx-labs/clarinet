import {
  DidOpenTextDocumentParams,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  DidSaveTextDocumentNotification,
  InitializeRequest,
} from "vscode-languageserver";
import type { Connection } from "vscode-languageserver";

// this type is the same for the browser and node but node isn't always built in dev
import { LspVscodeBridge } from "./clarity-lsp-browser/lsp-browser";

const VALID_PROTOCOLS = ["file", "vscode-vfs", "vscode-test-web"];

export function initConnection(
  connection: Connection,
  bridge: LspVscodeBridge,
) {
  let initializationOptions: { [key: string]: any } = {};
  connection.onInitialize((params) => {
    try {
      initializationOptions = JSON.parse(params.initializationOptions);
      params.initializationOptions = initializationOptions;
    } catch (err) {
      console.error("Invalid initialization options");
      throw err;
    }

    return bridge.onRequest(InitializeRequest.method, params);
  });

  const notifications: [string, unknown][] = [];
  async function consumeNotification() {
    const notification = notifications[notifications.length - 1];
    if (!notification) return;
    const [method] = notification;
    try {
      await bridge.onNotification(...notification);
      if (
        method === DidSaveTextDocumentNotification.method ||
        method === DidOpenTextDocumentNotification.method
      ) {
        connection.sendRequest("workspace/codeLens/refresh").catch(() => {});
      }
    } catch (err) {
      console.warn(err);
    }
    notifications.pop();
    if (notifications.length > 0) consumeNotification();
  }

  connection.onNotification((method: string, params: unknown) => {
    // vscode.dev sends didOpen notification twice
    // including a notification with a read only github:// url
    // instead of vscode-vfs://
    if (
      method === DidOpenTextDocumentNotification.method ||
      method === DidCloseTextDocumentNotification.method
    ) {
      const [protocol] = (
        params as DidOpenTextDocumentParams
      ).textDocument.uri.split("://");
      if (!VALID_PROTOCOLS.includes(protocol)) return;
    }

    notifications.push([method, params]);
    if (notifications.length === 1) consumeNotification();
  });

  const ignoreMethodsLog = ["textDocument/documentSymbol"];

  connection.onRequest((method: string, params: unknown) => {
    if (notifications.length > 0) return null;

    if (
      !initializationOptions.debug?.logRequestsTimings ||
      ignoreMethodsLog.includes(method)
    ) {
      return bridge.onRequest(method, params);
    }

    const id = Math.random().toString(16).slice(2, 18);
    const label = `${method} (${id})`;
    console.time(label);
    const r = bridge.onRequest(method, params);
    console.timeEnd(label);
    return r;
  });

  connection.listen();
}
