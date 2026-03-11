import type { LanguageClient as LanguageClientBrowser } from "vscode-languageclient/browser";
import type { LanguageClient as LanguageClientNode } from "vscode-languageclient/node";

export type LanguageClient = LanguageClientBrowser | LanguageClientNode;

export type FileEvent = {
  path: string;
};
