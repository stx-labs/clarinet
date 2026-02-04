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

export type CostDetailsData = {
  function: string;
  path: string;
  cost: {
    runtime: { min: number; max: number };
    read_count: { min: number; max: number };
    read_length: { min: number; max: number };
    write_count: { min: number; max: number };
    write_length: { min: number; max: number };
  };
  percentages: {
    runtime: number;
    read_count: number;
    read_length: number;
    write_count: number;
    write_length: number;
  };
  limits: {
    runtime: number;
    read_count: number;
    read_length: number;
    write_count: number;
    write_length: number;
  };
  trait_count: number;
};
