import { expect, ExpectStatic, assert } from "vitest";
import {
  Cl,
  ClarityValue,
  ClarityType,
  ResponseOkCV,
  NoneCV,
  SomeCV,
  ResponseErrorCV,
  IntCV,
  UIntCV,
  StringAsciiCV,
  StringUtf8CV,
  ContractPrincipalCV,
  StandardPrincipalCV,
  ListCV,
  TupleCV,
  BufferCV,
  TrueCV,
  FalseCV,
  BooleanCV,
  cvToString,
} from "@stacks/transactions";

import { MatcherState } from "@vitest/expect";

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#use_within_json
// @ts-ignore
BigInt.prototype.toJSON = function () {
  return this.toString();
};

function notStr(isNot: boolean) {
  return isNot ? "not " : "";
}

function formatMessage(this: MatcherState, received: string, expected: string) {
  return `expected ${received} ${notStr(this.isNot)}to be ${expected}`;
}

export class ClarityTypeError extends Error {
  actual?: any;
  expected?: any;

  constructor({ message, actual, expected }: { message: string; actual?: any; expected?: any }) {
    super(message);
    this.actual = actual;
    this.expected = expected;

    Object.setPrototypeOf(this, ClarityTypeError.prototype);
  }
}

type ClarityTypetoValue = {
  [ClarityType.OptionalNone]: NoneCV;
  [ClarityType.OptionalSome]: SomeCV;
  [ClarityType.ResponseOk]: ResponseOkCV;
  [ClarityType.ResponseErr]: ResponseErrorCV;
  [ClarityType.BoolTrue]: TrueCV;
  [ClarityType.BoolFalse]: FalseCV;
  [ClarityType.Int]: IntCV;
  [ClarityType.UInt]: UIntCV;
  [ClarityType.StringASCII]: StringAsciiCV;
  [ClarityType.StringUTF8]: StringUtf8CV;
  [ClarityType.PrincipalStandard]: StandardPrincipalCV;
  [ClarityType.PrincipalContract]: ContractPrincipalCV;
  [ClarityType.List]: ListCV;
  [ClarityType.Tuple]: TupleCV;
  [ClarityType.Buffer]: BufferCV;
};

const ClarityTypeReversed = Object.fromEntries(Object.entries(ClarityType).map(([k, v]) => [v, k]));

// the "simple clarity values" are CVs that can't be nested and have `value` property
type SimpleCV = BooleanCV | IntCV | UIntCV | StringAsciiCV | StringUtf8CV;
type SimpleCVTypes =
  | ClarityType.BoolFalse
  | ClarityType.BoolTrue
  | ClarityType.Int
  | ClarityType.UInt
  | ClarityType.StringASCII
  | ClarityType.StringUTF8;

const validClarityTypes = Object.values(ClarityType).filter(
  (t) => typeof t === "string",
) as string[];

function isClarityValue(input: unknown): input is ClarityValue {
  if (!input || typeof input !== "object") return false;
  if (!("type" in input) || typeof input.type !== "string") return false;
  if (!validClarityTypes.includes(input.type)) return false;

  return true;
}

function isClarityValueWithType<T extends ClarityType>(
  input: unknown,
  withType: T,
): input is ClarityTypetoValue[T] {
  if (!isClarityValue(input)) return false;
  if (input.type !== withType) return false;

  return true;
}

function checkCVType<T extends ClarityType>(
  actual: unknown,
  expectedType: T,
  isNot: boolean,
): actual is ClarityTypetoValue[T] {
  const isCV = isClarityValue(actual);

  if (!isCV) {
    throw new ClarityTypeError({
      message: `actual value must ${notStr(isNot)}be a Clarity "${
        ClarityTypeReversed[expectedType]
      }", received "${typeof actual}"`,
    });
  }

  const isCVWithType = isClarityValueWithType(actual, expectedType);

  if (!isCVWithType) {
    // for readability, the error diff is kept short if the developers uses the wrong `expect<ClarityType>`
    // ideally, we should have a way to display short message diffs even if the actual and/or expected data are big lists/tuples/buffers

    // for now, we make an exception and display the full error message if the actual value is a ResponseErr
    const errorCode = actual.type === ClarityType.ResponseErr ? ` ${Cl.prettyPrint(actual)}` : "";

    throw new ClarityTypeError({
      // generic and short message
      message: `actual value must ${notStr(isNot)}be a Clarity "${
        ClarityTypeReversed[expectedType]
      }", received "${ClarityTypeReversed[actual.type]}"${errorCode}`,
      actual: ClarityTypeReversed[actual.type],
      expected: ClarityTypeReversed[expectedType],
    });
  }

  return true;
}

function errorToAssertionResult(this: MatcherState, err: any) {
  return {
    pass: false,
    message: () => err.message,
    actual: err.actual,
    expected: err.expected,
  };
}

function simpleAssertion(
  this: MatcherState,
  cvType: SimpleCVTypes,
  actualRaw: unknown,
  expectedRaw: SimpleCV,
) {
  try {
    const isCV = checkCVType(actualRaw, cvType, this.isNot);
    assert(isCV);
  } catch (e: any) {
    return errorToAssertionResult.call(this, e);
  }

  return {
    pass: this.equals(actualRaw, expectedRaw, undefined, true),
    message: () =>
      `expected ${Cl.prettyPrint(actualRaw)} ${notStr(this.isNot)}to be ${Cl.prettyPrint(
        expectedRaw,
      )}`,
    actual: Cl.prettyPrint(actualRaw, 2),
    expected: Cl.prettyPrint(expectedRaw, 2),
  };
}

const typeToCvMethod = {
  [ClarityType.ResponseOk]: Cl.ok,
  [ClarityType.ResponseErr]: Cl.error,
  [ClarityType.OptionalSome]: Cl.some,
};

// simple composite types are `ok`, `err`, `some`
function simpleCompositeAssertion(
  this: MatcherState,
  expectedType: ClarityType.ResponseOk | ClarityType.ResponseErr | ClarityType.OptionalSome,
  actualRaw: unknown,
  expectedValue: ClarityValue | ExpectStatic,
) {
  try {
    const isCV = checkCVType(actualRaw, expectedType, this.isNot);
    assert(isCV);
  } catch (e: any) {
    return errorToAssertionResult.call(this, e);
  }

  const clMethod = typeToCvMethod[expectedType];

  const expectedIsCV = isClarityValue(expectedValue);
  const expectedOneLine = expectedIsCV
    ? Cl.prettyPrint(clMethod(expectedValue))
    : JSON.stringify(expectedValue);
  const expected = expectedIsCV
    ? Cl.prettyPrint(clMethod(expectedValue), 2)
    : JSON.stringify(expectedValue);

  return {
    pass: this.equals(actualRaw.value, expectedValue, undefined, true),
    message: () => formatMessage.call(this, Cl.prettyPrint(actualRaw), expectedOneLine),
    actual: Cl.prettyPrint(actualRaw, 2),
    expected,
  };
}

expect.extend({
  toHaveClarityType(actual: unknown, expectedType: ClarityType) {
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    return {
      pass: true,
      message: () =>
        `actual value must ${notStr(this.isNot)}be a Clarity "${ClarityTypeReversed[expectedType]}"`,
    };
  },

  toBeBool(actual: unknown, expected: boolean) {
    const expectedType = expected ? ClarityType.BoolTrue : ClarityType.BoolFalse;
    return simpleAssertion.call(this, expectedType, actual, Cl.bool(expected));
  },

  toBeInt(actual: unknown, expected: number | bigint) {
    return simpleAssertion.call(this, ClarityType.Int, actual, Cl.int(expected));
  },

  toBeUint(actual: unknown, expected: number | bigint) {
    return simpleAssertion.call(this, ClarityType.UInt, actual, Cl.uint(expected));
  },

  toBeAscii(actual: unknown, expected: string) {
    return simpleAssertion.call(this, ClarityType.StringASCII, actual, Cl.stringAscii(expected));
  },

  toBeUtf8(actual: unknown, expected: string) {
    return simpleAssertion.call(this, ClarityType.StringUTF8, actual, Cl.stringUtf8(expected));
  },

  toBeOk(actual: unknown, expectedValue: ExpectStatic | ClarityValue) {
    return simpleCompositeAssertion.call(this, ClarityType.ResponseOk, actual, expectedValue);
  },

  toBeErr(actual: unknown, expectedValue: ExpectStatic | ClarityValue) {
    return simpleCompositeAssertion.call(this, ClarityType.ResponseErr, actual, expectedValue);
  },

  toBeSome(actual: unknown, expectedValue: ExpectStatic | ClarityValue) {
    return simpleCompositeAssertion.call(this, ClarityType.OptionalSome, actual, expectedValue);
  },

  toBeNone(actual: unknown) {
    const expectedType = ClarityType.OptionalNone;
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    const expected = Cl.none();
    return {
      pass: this.equals(actual, expected, undefined, true),
      message: () => formatMessage.call(this, Cl.prettyPrint(actual), Cl.prettyPrint(actual)),
      actual: Cl.prettyPrint(actual, 2),
      expected: Cl.prettyPrint(actual, 2),
    };
  },

  toBePrincipal(actual: unknown, expectedString: string) {
    const isStandard = !expectedString.includes(".");
    let expected: StandardPrincipalCV | ContractPrincipalCV;

    const expectedType = isStandard ? ClarityType.PrincipalStandard : ClarityType.PrincipalContract;
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    const actualString = cvToString(actual, "tryAscii");

    try {
      expected = isStandard
        ? Cl.standardPrincipal(expectedString)
        : Cl.contractPrincipal(...(expectedString.split(".") as [string, string]));
    } catch {
      return {
        pass: false,
        message: () => `expected ${expectedString} ${notStr(this.isNot)}to be a principal`,
        actual: actualString,
        expected: expectedString,
      };
    }

    return {
      pass: this.equals(actual, expected, undefined, true),
      message: () => formatMessage.call(this, actualString, expectedString),
      actual: Cl.prettyPrint(actual, 2),
      expected: Cl.prettyPrint(expected, 2),
    };
  },

  toBeBuff(actual: unknown, expectedRaw: Uint8Array | string) {
    const expectedType = ClarityType.Buffer;
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    const expected = expectedRaw instanceof Uint8Array ? Cl.buffer(expectedRaw) : Cl.bufferFromHex(expectedRaw);
    return {
      pass: this.equals(actual, expected, undefined, true),
      // note: throw a simple message and rely on `actual` and `expected` to display the diff
      message: () => `the received Buffer does ${this.isNot ? "" : "not "}match the expected one`,
      actual: Cl.prettyPrint(actual, 2),
      expected: Cl.prettyPrint(expected, 2),
    };
  },

  toBeList(actual: unknown, expectedItems: ExpectStatic[] | ClarityValue[]) {
    const expectedType = ClarityType.List;
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    const isListArray = checkIsListArray(expectedItems);
    const expected = isListArray ? Cl.prettyPrint(Cl.list(expectedItems), 2) : expectedItems;

    return {
      pass: this.equals(actual.value, expectedItems, undefined, true),
      // note: throw a simple message and rely on `actual` and `expected` to display the diff
      message: () => `the received List does ${this.isNot ? "" : "not "}match the expected one`,
      actual: Cl.prettyPrint(actual, 2),
      expected,
    };
  },

  toBeTuple(actual: unknown, expectedData: Record<string, ExpectStatic | ClarityValue>) {
    const expectedType = ClarityType.Tuple;
    try {
      const isCV = checkCVType(actual, expectedType, this.isNot);
      assert(isCV);
    } catch (e: any) {
      return errorToAssertionResult.call(this, e);
    }

    const isTupleData = checkIsTupleData(expectedData);
    const expected = isTupleData ? Cl.prettyPrint(Cl.tuple(expectedData), 2) : expectedData;

    return {
      pass: this.equals(actual.value, expectedData, undefined, true),
      // note: throw a simple message and rely on `actual` and `expected` to display the diff
      message: () => `the received Tuple does ${this.isNot ? "" : "not "}match the expected one`,
      actual: Cl.prettyPrint(actual, 2),
      expected,
    };
  },
});

// for composite types, matchers need to narrow the type of the expected value
// to know if it contains AsymmetricMatchers or if it's only ClarityValues

function checkIsTupleData(
  expected: Record<string, ExpectStatic | ClarityValue>,
): expected is Record<string, ClarityValue> {
  return Object.values(expected).every((v) => isClarityValue(v));
}

function checkIsListArray(expected: ExpectStatic[] | ClarityValue[]): expected is ClarityValue[] {
  return expected.every((v) => isClarityValue(v));
}
