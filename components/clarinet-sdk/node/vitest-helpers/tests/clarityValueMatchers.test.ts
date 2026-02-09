import { describe, it, expect } from "vitest";
import { Cl, ClarityType } from "@stacks/transactions";

import "../src/clarityValuesMatchers";

describe("tests the Clarity Type of a CV", () => {
  it("tests any CV type", () => {
    expect(Cl.ok(Cl.int(1))).toHaveClarityType(ClarityType.ResponseOk);
    expect(Cl.error(Cl.int(1))).toHaveClarityType(ClarityType.ResponseErr);
    expect(Cl.some(Cl.int(1))).toHaveClarityType(ClarityType.OptionalSome);
    expect(Cl.none()).toHaveClarityType(ClarityType.OptionalNone);

    expect(Cl.int(1)).toHaveClarityType(ClarityType.Int);
    expect(Cl.uint(1n)).toHaveClarityType(ClarityType.UInt);
    expect(Cl.stringAscii("hello")).toHaveClarityType(ClarityType.StringASCII);
    expect(Cl.stringUtf8("hello")).toHaveClarityType(ClarityType.StringUTF8);

    try {
      expect(1).toHaveClarityType(ClarityType.ResponseOk);
    } catch (e: any) {
      expect(e.message).toBe('actual value must be a Clarity "ResponseOk", received "number"');
    }
    try {
      expect(Cl.uint(1)).toHaveClarityType(ClarityType.ResponseOk);
    } catch (e: any) {
      expect(e.message).toBe('actual value must be a Clarity "ResponseOk", received "UInt"');
    }

    try {
      expect(Cl.ok(Cl.uint(1))).not.toHaveClarityType(ClarityType.ResponseOk);
    } catch (e: any) {
      expect(e.message).toBe('actual value must not be a Clarity "ResponseOk"');
    }
  });
});

describe("test simple clarity values assertions", () => {
  it("tests ok", () => {
    const okRes = Cl.ok(Cl.uint(1));
    expect(okRes).toEqual(Cl.ok(Cl.uint(1)));

    expect(Cl.ok(Cl.uint(1))).toBeOk(Cl.uint(1));

    expect(Cl.ok(Cl.uint(1))).toBeOk(expect.toBeUint(1));
    expect(Cl.ok(Cl.uint(1))).toBeOk(expect.not.toBeUint(2));
    expect(Cl.error(Cl.uint(1))).not.toBeOk(expect.toBeUint(1));

    const nestedOks = Cl.ok(Cl.ok(Cl.uint(1)));
    expect(nestedOks).toBeOk(expect.toBeOk(expect.toBeUint(1)));

    expect(() => expect(Cl.uint(1)).toBeOk(Cl.uint(1))).toThrow(
      'actual value must be a Clarity "ResponseOk", received "UInt"',
    );

    try {
      const failingTest = () => expect(Cl.ok(Cl.uint(1))).toBeOk(Cl.uint(2));
      expect(failingTest).toThrow("expected (ok u1) to be (ok u2)");
    } catch (e: any) {
      expect(e.actual).toBe("(ok u1)");
      expect(e.expected).toBe("(ok u2)");
    }
  });

  it("tests error", () => {
    expect(Cl.error(Cl.uint(1))).toBeErr(expect.toBeUint(1));

    try {
      const failingTest = () => expect(Cl.error(Cl.uint(1))).toBeErr(Cl.uint(2));
      expect(failingTest).toThrow("expected (err u1) to be (err u2)");
    } catch (e: any) {
      expect(e.actual).toBe("(err u1)");
      expect(e.expected).toBe("(err u2)");
    }
  });

  it("tests some", () => {
    expect(Cl.some(Cl.uint(1))).toBeSome(expect.toBeUint(1));
    expect(Cl.some(Cl.uint(1))).toBeSome(Cl.uint(1));

    expect(() => expect(Cl.some(Cl.uint(1))).toBeSome(Cl.uint(2))).toThrow(
      "expected (some u1) to be (some u2)",
    );
  });

  it("tests none", () => {
    expect(Cl.none()).toBeNone();
    expect(Cl.uint(1)).not.toBeNone();

    try {
      expect(Cl.uint(1)).toBeNone();
    } catch (e: any) {
      expect(e.message).toBe('actual value must be a Clarity "OptionalNone", received "UInt"');
    }
  });

  it("tests bool", () => {
    expect(Cl.bool(true)).toBeBool(true);
    expect(Cl.bool(false)).toBeBool(false);

    expect(() => expect(Cl.uint(1)).toBeBool(false)).toThrow(
      'actual value must be a Clarity "BoolFalse", received "UInt"',
    );
    expect(() => expect(false).toBeBool(false)).toThrow(
      'actual value must be a Clarity "BoolFalse", received "boolean"',
    );
  });

  it("tests int", () => {
    expect(Cl.int(1)).toBeInt(1);
    expect(Cl.int(1)).toBeInt(1n);

    expect(() => expect(Cl.uint(1)).toBeInt(1)).toThrow(
      'actual value must be a Clarity "Int", received "UInt"',
    );

    try {
      const failingTest = () => expect(Cl.int(2)).toBeInt(1);
      expect(failingTest).toThrow("expected 2 to be 1");
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe("2");
      expect(e.expected).toBe("1");
    }
  });

  it("tests uint", () => {
    expect(Cl.uint(1)).toBeUint(1);
    expect(Cl.uint(1)).toBeUint(1n);

    expect(() => expect(Cl.int(1)).toBeUint(1)).toThrow(
      'actual value must be a Clarity "UInt", received "Int"',
    );

    try {
      const failingTest = () => expect(Cl.uint(2)).toBeUint(1);
      expect(failingTest).toThrow("expected u2 to be u1");
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe("u2");
      expect(e.expected).toBe("u1");
    }
  });

  it("tests string-ascii", () => {
    expect(Cl.stringAscii("hello world")).toBeAscii("hello world");

    expect(() => expect(Cl.int(1)).toBeAscii("hello world")).toThrow(
      'actual value must be a Clarity "StringASCII", received "Int"',
    );

    try {
      const failingTest = () => expect(Cl.stringAscii("hello")).toBeAscii("hello world");
      expect(failingTest).toThrow('expected "hello" to be "hello world"');
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe('"hello"');
      expect(e.expected).toBe('"hello world"');
    }
  });

  it("tests string-utf8", () => {
    expect(Cl.stringUtf8("hello world")).toBeUtf8("hello world");

    expect(() => expect(Cl.int(1)).toBeUtf8("hello world")).toThrow(
      'actual value must be a Clarity "StringUTF8", received "Int"',
    );

    try {
      const failingTest = () => expect(Cl.stringUtf8("hello")).toBeUtf8("hello world");
      expect(failingTest).toThrow('expected u"hello" to be u"hello world"');
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe('u"hello"');
      expect(e.expected).toBe('u"hello world"');
    }
  });

  it("tests standard principal", () => {
    const addr = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
    expect(Cl.standardPrincipal(addr)).toBePrincipal(addr);

    const addr2 = "STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6";
    try {
      const failingTest = () => expect(Cl.standardPrincipal(addr)).toBePrincipal(addr2);
      expect(failingTest).toThrow(`expected ${addr} to be ${addr2}`);
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe("'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG");
      expect(e.expected).toBe("'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6");
    }

    expect(() => expect(Cl.standardPrincipal(addr)).toBePrincipal("INVALID")).toThrow(
      "expected INVALID to be a principal",
    );

    const contractAddress = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG.contract";
    expect(
      Cl.contractPrincipal("ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG", "contract"),
    ).toBePrincipal(contractAddress);
  });

  it("tests contract principal", () => {
    const addr = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
    expect(Cl.contractPrincipal(addr, "counter")).toBePrincipal(`${addr}.counter`);
  });

  it("tests buffer", () => {
    const val = [98, 116, 99];
    expect(Cl.buffer(Uint8Array.from(val))).toBeBuff(Uint8Array.from(val));
    expect(Cl.buffer(Uint8Array.from(val))).toStrictEqual(Cl.bufferFromUtf8("btc"));

    expect(() => expect(Cl.int(1)).toBeBuff(Uint8Array.from(val))).toThrow(
      'actual value must be a Clarity "Buffer", received "Int"',
    );

    try {
      const failingTest = () =>
        expect(Cl.buffer(Uint8Array.from([99, 117, 100]))).toBeBuff(Uint8Array.from(val));
      // make sure that it actually throws and therefore that the `catch` branch below is executed
      expect(failingTest).toThrow("the received Buffer does not match the expected one");
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe("0x637564");
      expect(e.expected).toBe("0x627463");
    }

    expect(() =>
      expect(Cl.buffer(Uint8Array.from(val))).not.toBeBuff(Uint8Array.from(val)),
    ).toThrow("the received Buffer does match the expected one");

    // Allow hex strings
    expect(Cl.buffer(Uint8Array.from(val))).toBeBuff("627463");
  });
});

describe("tests lists", () => {
  it("test simple list", () => {
    expect(Cl.list([Cl.uint(1), Cl.uint(2)])).toBeList([Cl.uint(1), Cl.uint(2)]);
    expect(Cl.list([Cl.uint(1), Cl.uint(2)])).toBeList([expect.toBeUint(1), expect.toBeUint(2)]);

    expect(() =>
      expect(Cl.list([Cl.uint(1), Cl.uint(2)])).toBeList([Cl.uint(1), Cl.uint(3)]),
    ).toThrow("the received List does not match the expected one");

    expect(() =>
      expect(Cl.list([Cl.uint(1), Cl.uint(2)])).not.toBeList([Cl.uint(1), Cl.uint(2)]),
    ).toThrow("the received List does match the expected one");
  });

  it("tests failing list with pretty diff format", () => {
    const list = Cl.list([Cl.uint(1), Cl.uint(1)]);

    expect(list).toHaveClarityType(ClarityType.List);

    try {
      const failingTest = () => expect(list).toBeList([Cl.uint(1), Cl.uint(2)]);
      expect(failingTest).toThrow();
      failingTest();
    } catch (e: any) {
      expect(e.message).toStrictEqual("the received List does not match the expected one");
      expect(e.actual).toBe("(list\n  u1\n  u1\n)");
      expect(e.expected).toBe("(list\n  u1\n  u2\n)");
    }
  });
});

describe("tests tuple", () => {
  it("tests simple tuple", () => {
    const addr = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
    const tuple = Cl.tuple({
      count: Cl.int(1),
      owner: Cl.standardPrincipal(addr),
    });

    expect(tuple).toHaveClarityType(ClarityType.Tuple);
    expect(tuple).toBeTuple({
      count: Cl.int(1),
      owner: Cl.standardPrincipal(addr),
    });

    expect(tuple).toBeTuple({
      count: expect.toBeInt(1),
      owner: expect.toBePrincipal(addr),
    });

    expect(() =>
      expect(tuple).toBeTuple({
        count: Cl.int(2),
        owner: Cl.standardPrincipal(addr),
      }),
    ).toThrow("the received Tuple does not match the expected one");

    expect(() =>
      expect(tuple).not.toBeTuple({
        count: Cl.int(1),
        owner: Cl.standardPrincipal(addr),
      }),
    ).toThrow("the received Tuple does match the expected one");
  });

  it("tests failing tuple with pretty diff format", () => {
    const tuple = Cl.tuple({
      id: Cl.uint(1),
      message: Cl.stringAscii("hello world"),
    });

    expect(tuple).toHaveClarityType(ClarityType.Tuple);

    try {
      const failingTest = () =>
        expect(tuple).toBeTuple({
          id: Cl.uint(2),
          message: Cl.stringAscii("hello world"),
        });
      expect(failingTest).toThrow();
      failingTest();
    } catch (e: any) {
      expect(e.message).toStrictEqual("the received Tuple does not match the expected one");
      expect(e.actual).toBe('{\n  id: u1,\n  message: "hello world"\n}');
      expect(e.expected).toBe('{\n  id: u2,\n  message: "hello world"\n}');
    }
  });

  it("properly orders tuple keys", () => {
    // keys in non-alphabetical order
    const tuple = Cl.tuple({
      b: Cl.int(1),
      a: Cl.int(1),
      c: Cl.int(1),
    });

    try {
      const failingTest = () =>
        // keys in non-alphabetical order
        expect(tuple).toBeTuple({
          c: Cl.int(1),
          b: Cl.int(1),
          // different value here
          a: Cl.int(2),
        });
      expect(failingTest).toThrow();
      failingTest();
    } catch (e: any) {
      expect(e.message).toStrictEqual("the received Tuple does not match the expected one");
      expect(e.actual).toBe("{\n  a: 1,\n  b: 1,\n  c: 1\n}");
      expect(e.expected).toBe("{\n  a: 2,\n  b: 1,\n  c: 1\n}");
    }
  });

  it("properly orders tuple keys even in nested types", () => {
    // keys in non-alphabetical order
    const value = Cl.some(
      Cl.tuple({
        b: Cl.int(1),
        a: Cl.int(1),
        c: Cl.int(1),
      }),
    );

    try {
      const failingTest = () =>
        // keys in non-alphabetical order
        expect(value).toBeSome(
          Cl.tuple({
            c: Cl.int(1),
            b: Cl.int(1),
            // different value here
            a: Cl.int(2),
          }),
        );
      expect(failingTest).toThrow();
      failingTest();
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        "expected (some { a: 1, b: 1, c: 1 }) to be (some { a: 2, b: 1, c: 1 })",
      );
      expect(e.actual).toBe("(some {\n  a: 1,\n  b: 1,\n  c: 1\n})");
      expect(e.expected).toBe("(some {\n  a: 2,\n  b: 1,\n  c: 1\n})");
    }
  });
});

describe("test nested types", () => {
  it("test tuple with complex types", () => {
    const addr = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
    const complexTuple = Cl.tuple({
      id: Cl.uint(1),
      items: Cl.list([
        Cl.tuple({
          id: Cl.uint(1),
          owner: Cl.some(Cl.standardPrincipal(addr)),
          valid: Cl.ok(Cl.uint(2)),
        }),
        Cl.tuple({
          id: Cl.uint(2),
          owner: Cl.none(),
          valid: Cl.error(Cl.uint(1000)),
        }),
      ]),
    });

    // show multiple ways of testing

    // toEqual
    expect(complexTuple).toEqual(complexTuple);

    // toBeTuple with a Cl.tuple
    expect(complexTuple).toBeTuple(complexTuple.value);

    // toBeTuple asymmetric matched
    expect(complexTuple).toBeTuple({
      id: expect.toBeUint(1),
      items: expect.toBeList([
        expect.toBeTuple({
          id: expect.toBeUint(1),
          owner: expect.toBeSome(expect.toBePrincipal(addr)),
          // test for ok and value
          valid: expect.toBeOk(expect.toBeUint(2)),
        }),
        expect.toBeTuple({
          id: expect.toBeUint(2),
          owner: expect.toBeNone(),
          // test for error but not the value within
          valid: expect.toHaveClarityType(ClarityType.ResponseErr),
        }),
      ]),
    });
  });

  it("displays values on one line in message", () => {
    const failingTest = () =>
      expect(Cl.ok(Cl.tuple({ counter: Cl.uint(1) }))).toBeOk(Cl.tuple({ counter: Cl.uint(2) }));

    expect(failingTest).toThrow("expected (ok { counter: u1 }) to be (ok { counter: u2 })");
    try {
      failingTest();
    } catch (e: any) {
      expect(e.actual).toBe("(ok {\n  counter: u1\n})");
      expect(e.expected).toBe("(ok {\n  counter: u2\n})");
    }
  });

  it("displays short message of the matchers does not match the actual clarity type", () => {
    const value = Cl.ok(Cl.uint(1));
    const failingTest = () => expect(value).toBeList([]);

    expect(failingTest).toThrow('actual value must be a Clarity "List", received "ResponseOk"');
  });

  it("displays error code even if `err` is not the expected type", () => {
    const value = Cl.error(Cl.uint(1));
    const failingTest = () => expect(value).toBeOk(Cl.tuple({ counter: Cl.uint(2) }));

    expect(failingTest).toThrow(
      'actual value must be a Clarity "ResponseOk", received "ResponseErr" (err u1)',
    );
  });
});
