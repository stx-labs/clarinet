export {
  tx,
  type ClarityEvent,
  type ParsedTransactionResult,
  type DeployContractOptions,
  type Tx,
  type TransferSTX,
} from "../../common/src/sdkProxyHelpers.js";

import type { SDK } from "@stacks/clarinet-sdk-wasm";

import { vfs } from "./vfs.js";
import { Simnet, getSessionProxy } from "./sdkProxy.js";
import { connectSyncSocket } from "./syncDebugSocket.js";
import type { SyncDebugSimnet } from "./syncDebugSimnet.js";
import { createSyncDebugSimnet } from "./syncDebugSimnet.js";

export { type Simnet } from "./sdkProxy.js";

const wasmModule = import("@stacks/clarinet-sdk-wasm");

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#use_within_json
// @ts-ignore
BigInt.prototype.toJSON = function () {
  return this.toString();
};

type Options = {
  trackCosts: boolean;
  trackCoverage: boolean;
  trackPerformance?: boolean;
};

export async function getSDK(options?: Options): Promise<Simnet> {
  const module = await wasmModule;
  let sdkOptions = new module.SDKOptions(
    !!options?.trackCosts,
    !!options?.trackCoverage,
    !!options?.trackPerformance,
  );

  const simnet = new Proxy(
    new module.SDK(vfs, sdkOptions),
    getSessionProxy() as ProxyHandler<SDK>,
  ) as unknown as Simnet;
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
  // When CLARINET_DEBUG_PORT is set, a single debug proxy is shared across all calls.
  let debugSimnet: SyncDebugSimnet | null = null;

  return async (
    manifestPath = "./Clarinet.toml",
    noCache = false,
    options?: {
      trackCosts: boolean;
      trackCoverage: boolean;
      trackPerformance?: boolean;
      performanceCostField?: string;
      apiUrl?: string;
    },
  ) => {
    const debugPort = process.env["CLARINET_DEBUG_PORT"]
      ? Number(process.env["CLARINET_DEBUG_PORT"])
      : undefined;

    if (debugPort != null) {
      // Connect to the debug server (no-op if already connected).
      await connectSyncSocket(debugPort);
      if (!debugSimnet || noCache) {
        debugSimnet = createSyncDebugSimnet();
      }
      // Tell the server to reinitialise its session (mirrors initSession semantics).
      await debugSimnet.initSession(process.cwd(), manifestPath, options);
      return debugSimnet as unknown as Simnet;
    }

    if (noCache || !simnet) {
      simnet = await getSDK(options);
    }

    // start a new simnet session
    await simnet.initSession(process.cwd(), manifestPath, options?.apiUrl ?? null);
    return simnet;
  };
}

export const initSimnet = memoizedInit();

export {
  startDebugServer,
  DebugClient,
  type DebugCallResult,
} from "./debugClient.js";
