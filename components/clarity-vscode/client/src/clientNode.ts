import { ExtensionContext } from "vscode";

import {
  LanguageClient,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import { initClient, clientOpts } from "./common";

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
  await initClient(context, client);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) return undefined;
  return client.stop();
}
