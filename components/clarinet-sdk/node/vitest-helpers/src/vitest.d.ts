import type { Assertion, AsymmetricMatchersContaining, ExpectStatic } from "vitest";
import type { ClarityType, ClarityValue } from "@stacks/transactions";

interface ClarityValuesMatchers<R = unknown> {
  toHaveClarityType(expectedType: ClarityType): R;

  toBeOk(expected: ExpectStatic | ClarityValue): R;
  toBeErr(expected: ExpectStatic | ClarityValue): R;

  toBeSome(expected: ExpectStatic | ClarityValue): R;
  toBeNone(): R;

  toBeBool(expected: boolean): R;
  toBeInt(rexpected: number | bigint): R;
  toBeUint(expected: number | bigint): R;
  toBeAscii(expected: string): R;
  toBeUtf8(expected: string): R;
  toBePrincipal(expected: string): R;
  /** Validate that a BufferCV is equal to the expected Uint8Array or hex string */
  toBeBuff(expected: Uint8Array | string): R;

  toBeList(expected: ExpectStatic[] | ClarityValue[]): R;
  toBeTuple(expected: Record<string, ExpectStatic | ClarityValue>): R;
}

declare module "vitest" {
  interface Assertion<T = any> extends ClarityValuesMatchers<T> {}
  interface AsymmetricMatchersContaining extends ClarityValuesMatchers<ExpectStatic> {}
}
