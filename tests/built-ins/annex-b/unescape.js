/*---
description: >
  The Annex B global unescape (ES2026 B.2.1.2) under compat-annex-b: %XX and
  %uXXXX decoding, malformed sequences, lone surrogates, ToString, property
  attributes, and no [[Construct]].
features: [compat-annex-b, unescape]
---*/

describe("unescape", () => {
  test("is a global function", () => {
    expect(typeof unescape).toBe("function");
  });

  test("is writable, non-enumerable and configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, "unescape");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test("has length 1 and name unescape", () => {
    expect(unescape.length).toBe(1);
    expect(unescape.name).toBe("unescape");
  });

  test("is not a constructor", () => {
    expect(Object.prototype.hasOwnProperty.call(unescape, "prototype")).toBe(false);
    expect(() => new unescape("a")).toThrow(TypeError);
  });

  test("decodes %XX with either case of hex digit", () => {
    expect(unescape("%41")).toBe("A");
    expect(unescape("%e9%E9")).toBe("\u00E9\u00E9");
    expect(unescape("%00")).toBe("\0");
  });

  test("decodes %uXXXX with either case of hex digit", () => {
    expect(unescape("%u0041")).toBe("A");
    expect(unescape("%u20ac%u20AC")).toBe("\u20AC\u20AC");
    expect(unescape("%uFFFF")).toBe("\uFFFF");
  });

  test("decodes a sequence that ends the string", () => {
    expect(unescape("a%42")).toBe("aB");
    expect(unescape("a%u0042")).toBe("aB");
  });

  test("decodes surrogate pairs and lone surrogates as code units", () => {
    expect(unescape("%uD83D%uDE00")).toBe("\u{1F600}");
    const lone = unescape("%uD800");
    expect(lone.length).toBe(1);
    expect(lone.charCodeAt(0)).toBe(0xd800);
    expect(unescape("%uDC00x")).toBe("\uDC00x");
  });

  test("leaves malformed sequences as they are", () => {
    expect(unescape("%")).toBe("%");
    expect(unescape("%4")).toBe("%4");
    expect(unescape("%G1")).toBe("%G1");
    expect(unescape("%4G")).toBe("%4G");
    expect(unescape("%u")).toBe("%u");
    expect(unescape("%u12")).toBe("%u12");
    expect(unescape("%u004")).toBe("%u004");
    expect(unescape("%u00G1")).toBe("%u00G1");
    expect(unescape("%u+041")).toBe("%u+041");
  });

  test("starts a new sequence after a % it leaves as it is", () => {
    expect(unescape("%%41")).toBe("%A");
    expect(unescape("%u%41")).toBe("%uA");
    expect(unescape("%u00%41")).toBe("%u00A");
  });

  test("decodes each sequence once", () => {
    expect(unescape("%2541")).toBe("%41");
    expect(unescape("%u0025u0041")).toBe("%u0041");
  });

  test("inverts escape for the issue's example and every code unit", () => {
    expect(unescape("a%20b/%FC%u20AC")).toBe("a b/\u00FC\u20AC");
    const mismatches = Array.from({ length: 0x10000 }, (_, unit) => unit).filter(
      (unit) => unescape(escape(String.fromCharCode(unit))) !== String.fromCharCode(unit),
    );
    expect(mismatches).toEqual([]);
  });

  test("converts its argument with ToString", () => {
    expect(unescape()).toBe("undefined");
    expect(unescape(null)).toBe("null");
    expect(unescape(41)).toBe("41");
    expect(unescape({ toString: () => "%41", valueOf: () => 1 })).toBe("A");
  });

  test("throws a TypeError for a Symbol", () => {
    expect(() => unescape(Symbol("s"))).toThrow(TypeError);
  });

  test("does not call String.prototype methods a script replaces", () => {
    const { charAt, charCodeAt, indexOf } = String.prototype;
    String.prototype.charAt = () => "x";
    String.prototype.charCodeAt = () => 0;
    String.prototype.indexOf = () => -1;
    let result;
    try {
      result = unescape("%41%u0042c");
    } finally {
      String.prototype.charAt = charAt;
      String.prototype.charCodeAt = charCodeAt;
      String.prototype.indexOf = indexOf;
    }
    expect(result).toBe("ABc");
  });
});
