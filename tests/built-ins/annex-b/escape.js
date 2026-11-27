/*---
description: >
  The Annex B global escape (ES2026 B.2.1.1) under compat-annex-b: the
  unescaped set, %XX and %uXXXX output, lone surrogates, ToString, property
  attributes, and no [[Construct]].
features: [compat-annex-b, escape]
---*/

const UNESCAPED =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789@*_+-./";
const HEX = "0123456789ABCDEF";
const EVERY_CODE_UNIT = Array.from({ length: 0x10000 }, (_, unit) => unit);

// B.2.1.1's result for one code unit, computed independently of escape
const expected = (unit) => {
  const ch = String.fromCharCode(unit);
  if (UNESCAPED.includes(ch)) return ch;
  const digit = (shift) => HEX[(unit >> shift) & 15];
  return unit < 256
    ? `%${digit(4)}${digit(0)}`
    : `%u${digit(12)}${digit(8)}${digit(4)}${digit(0)}`;
};

describe("escape", () => {
  test("is a global function", () => {
    expect(typeof escape).toBe("function");
  });

  test("is writable, non-enumerable and configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, "escape");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test("has length 1 and name escape", () => {
    expect(escape.length).toBe(1);
    expect(escape.name).toBe("escape");
  });

  test("is not a constructor", () => {
    expect(Object.prototype.hasOwnProperty.call(escape, "prototype")).toBe(false);
    expect(() => new escape("a")).toThrow(TypeError);
  });

  test("leaves the unescaped set as it is", () => {
    expect(escape(UNESCAPED)).toBe(UNESCAPED);
  });

  test("escapes code units below 256 as %XX with uppercase digits", () => {
    expect(escape("\0")).toBe("%00");
    expect(escape(" ")).toBe("%20");
    expect(escape("~")).toBe("%7E");
    expect(escape("%")).toBe("%25");
    expect(escape("\u00FC")).toBe("%FC");
    expect(escape("\u00FF")).toBe("%FF");
  });

  test("escapes code units from 256 as %uXXXX with uppercase digits", () => {
    expect(escape("\u0100")).toBe("%u0100");
    expect(escape("\u20AC")).toBe("%u20AC");
    expect(escape("\uFFFF")).toBe("%uFFFF");
  });

  test("escapes each code unit of a surrogate pair and lone surrogates", () => {
    expect(escape("\u{1F600}")).toBe("%uD83D%uDE00");
    expect(escape("\uD800")).toBe("%uD800");
    expect(escape("\uDC00a")).toBe("%uDC00a");
  });

  test("escapes the issue's example", () => {
    expect(escape("a b/\u00FC\u20AC")).toBe("a%20b/%FC%u20AC");
  });

  test("matches B.2.1.1 for every code unit", () => {
    const mismatches = EVERY_CODE_UNIT.filter(
      (unit) => escape(String.fromCharCode(unit)) !== expected(unit),
    );
    expect(mismatches).toEqual([]);
  });

  test("converts its argument with ToString", () => {
    expect(escape()).toBe("undefined");
    expect(escape(null)).toBe("null");
    expect(escape(1.5)).toBe("1.5");
    expect(escape(true)).toBe("true");
    expect(escape({ toString: () => "a b", valueOf: () => 1 })).toBe("a%20b");
    expect(escape(["a", "b"])).toBe("a%2Cb");
  });

  test("throws a TypeError for a Symbol", () => {
    expect(() => escape(Symbol("s"))).toThrow(TypeError);
  });

  test("does not call String.prototype methods a script replaces", () => {
    const { charAt, charCodeAt, indexOf } = String.prototype;
    String.prototype.charAt = () => "%";
    String.prototype.charCodeAt = () => 0;
    String.prototype.indexOf = () => -1;
    let result;
    try {
      result = escape("a b\u0100");
    } finally {
      String.prototype.charAt = charAt;
      String.prototype.charCodeAt = charCodeAt;
      String.prototype.indexOf = indexOf;
    }
    expect(result).toBe("a%20b%u0100");
  });
});
