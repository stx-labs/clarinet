import init, { SDK } from "@stacks/clarinet-sdk-wasm-browser";

import { Simnet, getSessionProxy } from "./sdkProxy.js";
import { defaultVfs } from "./defaultVfs.js";

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#use_within_json
// @ts-ignore
BigInt.prototype.toJSON = function () {
  return this.toString();
};

export {
  tx,
  type ClarityEvent,
  type ParsedTransactionResult,
  type DeployContractOptions,
  type Tx,
  type TransferSTX,
} from "../../common/src/sdkProxyHelpers.js";

export { init, SDK, getSessionProxy, type Simnet };
export { defaultVfs, defaultFileStore } from "./defaultVfs.js";

// @ts-ignore
globalThis.vfs = defaultVfs;

export const initSimnet = async (virtualFileSystem?: Function) => {
  await init();
  const vfs = virtualFileSystem ? virtualFileSystem : defaultVfs;
  return new Proxy(new SDK(vfs), getSessionProxy()) as unknown as Simnet;
};
