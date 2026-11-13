import type { Simnet } from "../src/sdkProxy";

declare const simnet: Simnet;

simnet.free();
simnet[Symbol.dispose]();
