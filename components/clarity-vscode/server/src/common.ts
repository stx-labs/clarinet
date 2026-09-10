import {
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  DocumentSymbolRequest,
  InitializeRequest,
} from "vscode-languageserver";
import type {
  Connection,
  DidOpenTextDocumentParams,
} from "vscode-languageserver";

// this type is the same for the browser and node but node isn't always built in dev
// it has to stay type-only, `server/tests` loads this file unbuilt
import type { LspVscodeBridge } from "./clarity-lsp-browser/lsp-browser";

const VALID_PROTOCOLS = ["file", "vscode-vfs", "vscode-test-web"];

function documentUri(params: unknown): string | undefined {
  return (params as DidOpenTextDocumentParams | undefined)?.textDocument?.uri;
}

// fast and high-frequency, they would drown out everything else
const ignoreMethodsLog: string[] = [
  DocumentSymbolRequest.method,
  DidChangeTextDocumentNotification.method,
];

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

  function startTimingsLog(method: string) {
    if (
      !initializationOptions.debug?.logRequestsTimings ||
      ignoreMethodsLog.includes(method)
    ) {
      return null;
    }

    const id = Math.random().toString(16).slice(2, 18);
    const start = performance.now();
    return () => {
      const ms = (performance.now() - start).toFixed(3);
      console.log(`${method} (${id}): ${ms}ms`);
    };
  }

  // the in-flight entry stays at the front of the queue, `onNotification` and
  // `onRequest` read a non-empty queue as "the bridge is busy"
  const notifications: [string, unknown][] = [];
  async function consumeNotifications() {
    while (notifications.length > 0) {
      const [method, params] = notifications[0];
      const logTimings = startTimingsLog(method);
      try {
        await bridge.onNotification(method, params);
      } catch (err) {
        console.warn(err);
      } finally {
        // dequeue even on failure, a leftover entry would stall the queue
        notifications.shift();
        logTimings?.();
      }
    }
  }

  // document sync is full (see `capabilities.rs`): a newer didChange carries a
  // snapshot that fully supersedes the queued one, so analyzing the queued
  // snapshot is wasted work. A didOpen/didSave/didClose for the same document
  // is a barrier, snapshots can't be merged across it
  function replaceQueuedDidChange(params: unknown) {
    const uri = documentUri(params);
    if (!uri) return false;

    // entry 0 is in flight
    for (let i = notifications.length - 1; i > 0; i--) {
      const [queuedMethod, queuedParams] = notifications[i];
      if (documentUri(queuedParams) !== uri) continue;
      if (queuedMethod !== DidChangeTextDocumentNotification.method) {
        return false;
      }
      notifications[i][1] = params;
      return true;
    }
    return false;
  }

  connection.onNotification((method: string, params: unknown) => {
    // vscode.dev sends didOpen notification twice
    // including a notification with a read only github:// url
    // instead of vscode-vfs://
    if (
      method === DidOpenTextDocumentNotification.method ||
      method === DidCloseTextDocumentNotification.method
    ) {
      const [protocol] = documentUri(params)?.split("://") ?? [];
      if (!VALID_PROTOCOLS.includes(protocol)) return;
    }

    if (
      method === DidChangeTextDocumentNotification.method &&
      replaceQueuedDidChange(params)
    ) {
      return;
    }

    notifications.push([method, params]);
    if (notifications.length === 1) consumeNotifications();
  });

  connection.onRequest((method: string, params: unknown) => {
    if (notifications.length > 0) return null;

    // the request bridge is synchronous, the call itself is the whole cost
    const logTimings = startTimingsLog(method);
    try {
      return bridge.onRequest(method, params);
    } finally {
      logTimings?.();
    }
  });

  connection.listen();
}
