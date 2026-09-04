import {
  CallFnArgs,
  ContractOptions,
  DeployContractArgs,
  TransferSTXArgs,
  type SDK,
} from "@stacks/clarinet-sdk-wasm";

import { createSessionProxy, type ProxiedSimnet } from "../../common/src/sdkProxy.js";

export type Simnet = ProxiedSimnet<SDK>;

export function getSessionProxy(): ProxyHandler<SDK> {
  return createSessionProxy<SDK, ContractOptions>({
    CallFnArgs,
    ContractOptions,
    DeployContractArgs,
    TransferSTXArgs,
  });
}
