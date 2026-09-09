import { createConnection } from "vscode-languageserver/node";

import { LspVscodeBridge } from "./clarity-lsp-node";
import { initConnection } from "./common";

const connection = createConnection();
const bridge = new LspVscodeBridge(
  connection.sendDiagnostics,
  connection.sendNotification,
  connection.sendRequest,
);

initConnection(connection, bridge);
