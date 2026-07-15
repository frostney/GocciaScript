/*---
description: Tagged native call fast paths preserve ordinary call semantics
features: [global-properties, globalThis]
---*/

describe("native intrinsic call fast paths", () => {
  test("decode intrinsics accept the scalar string hot shape", () => {
    const decode = decodeURIComponent;

    expect(decode("%F0%9F%98%80")).toBe("😀");
    expect(decodeURI("%2F")).toBe("%2F");
  });

  test("decode intrinsics fall back for observable coercion", () => {
    let calls = 0;
    const input = {
      toString() {
        calls++;
        return "%41";
      },
    };

    expect(decodeURIComponent(input)).toBe("A");
    expect(calls).toBe(1);
  });

  test("replaced decode globals are called normally", () => {
    const descriptor = Object.getOwnPropertyDescriptor(
      globalThis,
      "decodeURIComponent",
    );
    const replacement = (value) => `replacement:${value}`;

    try {
      globalThis.decodeURIComponent = replacement;
      expect(decodeURIComponent("%41")).toBe("replacement:%41");
    } finally {
      Object.defineProperty(globalThis, "decodeURIComponent", descriptor);
    }
  });

  test("String.fromCharCode accepts two scalar code units", () => {
    expect(String.fromCharCode(0xd83d, 0xde00)).toBe("😀");
    expect(String.fromCharCode(-1, 0x41)).toBe("\uffffA");
  });

  test("String.fromCharCode falls back for coercive arguments", () => {
    let calls = 0;
    const first = {
      valueOf() {
        calls++;
        return 0x41;
      },
    };

    expect(String.fromCharCode(first, 0x42)).toBe("AB");
    expect(calls).toBe(1);
    expect(String.fromCharCode(Infinity, NaN)).toBe("\u0000\u0000");
    expect(String.fromCharCode(...[0x43, 0x44])).toBe("CD");
  });

  test("replaced String.fromCharCode methods are called normally", () => {
    const descriptor = Object.getOwnPropertyDescriptor(String, "fromCharCode");
    const replacement = (first, second) => `${first}:${second}`;

    try {
      String.fromCharCode = replacement;
      expect(String.fromCharCode(1, 2)).toBe("1:2");
    } finally {
      Object.defineProperty(String, "fromCharCode", descriptor);
    }
  });
});
