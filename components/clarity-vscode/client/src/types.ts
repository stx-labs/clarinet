import type { LanguageClient as LanguageClientBrowser } from "vscode-languageclient/browser";
import type { LanguageClient as LanguageClientNode } from "vscode-languageclient/node";

export type LanguageClient = LanguageClientBrowser | LanguageClientNode;

type ClarityArg = {
  name: string;
  signature: any;
};

export type InsightsData = {
  fnType: string;
  fnName: string;
  fnArgs: ClarityArg[];
  fnReturns: any;
};

export type CursorMove = {
  path: string;
  line: number;
  char: number;
};

export type FileEvent = {
  path: string;
};
