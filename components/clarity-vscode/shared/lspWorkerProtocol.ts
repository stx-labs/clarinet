// Contract between the web extension client and the LSP server worker, shared
// so that a rename can't silently desynchronise the two bundles.
//
// The worker is started from a blob URL on an isolated origin and can't resolve
// the extension assets by itself, so the client sends it the wasm URL derived
// from `extensionUri` as the very first message.

export const INIT_WASM_METHOD = "clarity-lsp/initWasm";

export interface InitWasmMessage {
  method: typeof INIT_WASM_METHOD;
  wasmURL: string;
}

export function isInitWasmMessage(data: unknown): data is InitWasmMessage {
  if (typeof data !== "object" || data === null) return false;
  const { method, wasmURL } = data as Partial<InitWasmMessage>;
  return method === INIT_WASM_METHOD && typeof wasmURL === "string";
}
