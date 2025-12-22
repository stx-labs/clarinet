export {
  tx,
  type ClarityEvent,
  type ParsedTransactionResult,
  type DeployContractOptions,
  type Tx,
  type TransferSTX,
} from "../../common/src/sdkProxyHelpers.js";

import { vfs } from "./vfs.js";
import { Simnet, getSessionProxy } from "./sdkProxy.js";

export { type Simnet } from "./sdkProxy.js";

const wasmModule = import("@stacks/clarinet-sdk-wasm");

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#use_within_json
// @ts-ignore
BigInt.prototype.toJSON = function () {
  return this.toString();
};

type Options = {
  // sdk options
  trackCosts?: boolean;
  trackCoverage?: boolean;
  trackPerformance?: boolean;
  // session options
  /** Use the existing deployment plan without updating it */
  forceOnDisk?: boolean;
};

export async function getSDK(options?: Options): Promise<Simnet> {
  const module = await wasmModule;
  let sdkOptions = new module.SDKOptions(
    !!options?.trackCosts,
    !!options?.trackCoverage,
    !!options?.trackPerformance,
  );

  const simnet = new Proxy(new module.SDK(vfs, sdkOptions), getSessionProxy()) as unknown as Simnet;
  return simnet;
}

// wrapper around `simnet.generateDeploymentPlan()` that loads wasm and pass process.cwd()
export async function generateDeployement(manifestPath = "./Clarinet.toml") {
  const simnet = await getSDK();

  try {
    await simnet.generateDeploymentPlan(process.cwd(), manifestPath);
    return true;
  } catch (e) {
    console.warn(e)
    return false;
  }
}

// load wasm only once and memoize it
function memoizedInit() {
  let simnet: Simnet | null = null;

  return async (
    manifestPath = "./Clarinet.toml",
    noCache = false,
    options?: Options
  ) => {
    if (noCache || !simnet) {
      simnet = await getSDK(options);
    }

    await simnet.initSession(process.cwd(), manifestPath, !!options?.forceOnDisk);
    return simnet;
  };
}



export const initSimnet = memoizedInit();
