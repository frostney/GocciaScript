/*---
description: Symbols cannot be implicitly converted to strings or numbers (ECMAScript 2020 §6.1.5)
features: [Symbol]
---*/

// === Implicit string coercion — must throw TypeError ===

describe("implicit string coercion throws TypeError", () => {
  test("template literal", () => {
    const s = Symbol("foo");
    expect(() => { `${s}`; }).toThrow(TypeError);
  });

  test("string + Symbol", () => {
    const s = Symbol("foo");
    expect(() => { "hello" + s; }).toThrow(TypeError);
  });

  test("Symbol + string", () => {
    const s = Symbol("foo");
    expect(() => { s + "hello"; }).toThrow(TypeError);
  });

  test("Symbol + empty string", () => {
    const s = Symbol("foo");
    expect(() => { s + ""; }).toThrow(TypeError);
  });

  test("empty string + Symbol", () => {
    const s = Symbol("foo");
    expect(() => { "" + s; }).toThrow(TypeError);
  });

  test("Symbol + Symbol via addition", () => {
    const s1 = Symbol("a");
    const s2 = Symbol("b");
    expect(() => { s1 + s2; }).toThrow(TypeError);
  });

  test("String.prototype.concat with Symbol", () => {
    const s = Symbol("foo");
    expect(() => { "hello".concat(s); }).toThrow(TypeError);
  });
});

// === Implicit number coercion — must throw TypeError ===

describe("arithmetic operators throw TypeError", () => {
  test("Symbol + number", () => {
    const s = Symbol("foo");
    expect(() => { s + 1; }).toThrow(TypeError);
  });

  test("number + Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 1 + s; }).toThrow(TypeError);
  });

  test("Symbol - number", () => {
    const s = Symbol("foo");
    expect(() => { s - 1; }).toThrow(TypeError);
  });

  test("number - Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 1 - s; }).toThrow(TypeError);
  });

  test("Symbol * number", () => {
    const s = Symbol("foo");
    expect(() => { s * 2; }).toThrow(TypeError);
  });

  test("number * Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 2 * s; }).toThrow(TypeError);
  });

  test("Symbol / number", () => {
    const s = Symbol("foo");
    expect(() => { s / 2; }).toThrow(TypeError);
  });

  test("number / Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 2 / s; }).toThrow(TypeError);
  });

  test("Symbol % number", () => {
    const s = Symbol("foo");
    expect(() => { s % 2; }).toThrow(TypeError);
  });

  test("number % Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 2 % s; }).toThrow(TypeError);
  });

  test("Symbol ** number", () => {
    const s = Symbol("foo");
    expect(() => { s ** 2; }).toThrow(TypeError);
  });

  test("number ** Symbol", () => {
    const s = Symbol("foo");
    expect(() => { 2 ** s; }).toThrow(TypeError);
  });
});

describe("unary operators throw TypeError", () => {
  test("unary +", () => {
    const s = Symbol("foo");
    expect(() => { +s; }).toThrow(TypeError);
  });

  test("unary -", () => {
    const s = Symbol("foo");
    expect(() => { -s; }).toThrow(TypeError);
  });

  test("bitwise NOT (~)", () => {
    const s = Symbol("foo");
    expect(() => { ~s; }).toThrow(TypeError);
  });
});

describe("bitwise operators throw TypeError", () => {
  test("Symbol | number", () => {
    const s = Symbol("foo");
    expect(() => { s | 0; }).toThrow(TypeError);
  });

  test("Symbol & number", () => {
    const s = Symbol("foo");
    expect(() => { s & 0; }).toThrow(TypeError);
  });

  test("Symbol ^ number", () => {
    const s = Symbol("foo");
    expect(() => { s ^ 0; }).toThrow(TypeError);
  });

  test("Symbol << number", () => {
    const s = Symbol("foo");
    expect(() => { s << 1; }).toThrow(TypeError);
  });

  test("Symbol >> number", () => {
    const s = Symbol("foo");
    expect(() => { s >> 1; }).toThrow(TypeError);
  });

  test("Symbol >>> number", () => {
    const s = Symbol("foo");
    expect(() => { s >>> 1; }).toThrow(TypeError);
  });
});

describe("comparison operators throw TypeError", () => {
  test("Symbol > number", () => {
    const s = Symbol("foo");
    expect(() => { s > 0; }).toThrow(TypeError);
  });

  test("Symbol < number", () => {
    const s = Symbol("foo");
    expect(() => { s < 0; }).toThrow(TypeError);
  });

  test("Symbol >= number", () => {
    const s = Symbol("foo");
    expect(() => { s >= 0; }).toThrow(TypeError);
  });

  test("Symbol <= number", () => {
    const s = Symbol("foo");
    expect(() => { s <= 0; }).toThrow(TypeError);
  });
});

describe("Number() throws TypeError", () => {
  test("Number(symbol) throws", () => {
    const s = Symbol("foo");
    expect(() => { Number(s); }).toThrow(TypeError);
  });
});

// === Explicit conversions — must work ===

describe("explicit conversions work", () => {
  test("String(symbol) returns descriptive string", () => {
    expect(String(Symbol("test"))).toBe("Symbol(test)");
    expect(String(Symbol())).toBe("Symbol()");
  });

  test("Boolean(symbol) returns true", () => {
    expect(Boolean(Symbol("foo"))).toBe(true);
    expect(Boolean(Symbol())).toBe(true);
  });

  test("!!symbol returns true", () => {
    const s = Symbol("foo");
    expect(!!s).toBe(true);
  });
});

// === Operations that must NOT throw ===

describe("non-coercing operations work", () => {
  test("typeof returns 'symbol'", () => {
    expect(typeof Symbol("foo")).toBe("symbol");
    expect(typeof Symbol()).toBe("symbol");
  });

  test("strict equality does not throw", () => {
    const s1 = Symbol("foo");
    const s2 = Symbol("foo");
    expect(s1 === s1).toBe(true);
    expect(s1 === s2).toBe(false);
    expect(s1 !== s2).toBe(true);
  });

  test("Symbol as object property key", () => {
    const key = Symbol("key");
    const obj = {};
    obj[key] = 42;
    expect(obj[key]).toBe(42);
  });

  test("Symbol in conditional context", () => {
    const s = Symbol("foo");
    let result = false;
    if (s) {
      result = true;
    }
    expect(result).toBe(true);
  });

  test("Symbol with ternary operator", () => {
    const s = Symbol("foo");
    expect(s ? "yes" : "no").toBe("yes");
  });

  test("Symbol with logical operators", () => {
    const s = Symbol("foo");
    expect(s && true).toBe(true);
    expect(false || s).toBe(s);
    expect(s ?? "default").toBe(s);
  });
});
