/*---
description: Symbol coercion rules, @@toPrimitive protocol, and IsPrimitive semantics (ES2026 §6.1, §7.1.1)
features: [Symbol, Symbol.toPrimitive]
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

  test("symbol.toString() returns descriptive string", () => {
    const s = Symbol("test");
    expect(s.toString()).toBe("Symbol(test)");
    expect(s.toString()).toBe(String(s));
    expect(Symbol().toString()).toBe("Symbol()");
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

// === Symbol.toPrimitive (@@toPrimitive) protocol — ES2026 §7.1.1 ===

describe("Symbol.toPrimitive protocol", () => {
  test("String() passes 'string' hint", () => {
    const obj = {
      [Symbol.toPrimitive](hint) { return "hint:" + hint; },
    };
    expect(String(obj)).toBe("hint:string");
  });

  test("Number() passes 'number' hint", () => {
    const hints = [];
    const obj = {
      [Symbol.toPrimitive](hint) {
        hints.push(hint);
        return 42;
      },
    };
    expect(Number(obj)).toBe(42);
    expect(hints[0]).toBe("number");
  });

  test("template literal passes 'string' hint", () => {
    const obj = {
      [Symbol.toPrimitive](hint) { return "hint:" + hint; },
    };
    expect(`${obj}`).toBe("hint:string");
  });

  test("addition passes 'default' hint", () => {
    const obj = {
      [Symbol.toPrimitive](hint) { return "hint:" + hint; },
    };
    expect(obj + "").toBe("hint:default");
  });

  test("Symbol.toPrimitive takes precedence over valueOf and toString", () => {
    const obj = {
      valueOf() { return "valueOf"; },
      toString() { return "toString"; },
      [Symbol.toPrimitive](hint) { return "exotic(" + hint + ")"; },
    };
    expect(String(obj)).toBe("exotic(string)");
    expect(`${obj}`).toBe("exotic(string)");
    expect(obj + "").toBe("exotic(default)");
  });

  test("Symbol.toPrimitive returning non-primitive throws TypeError", () => {
    const obj = {
      [Symbol.toPrimitive]() { return {}; },
    };
    expect(() => String(obj)).toThrow(TypeError);
    expect(() => Number(obj)).toThrow(TypeError);
  });

  test("Symbol.toPrimitive returning array throws TypeError", () => {
    const obj = {
      [Symbol.toPrimitive]() { return []; },
    };
    expect(() => String(obj)).toThrow(TypeError);
  });

  test("absent Symbol.toPrimitive falls through to valueOf/toString", () => {
    const obj = { valueOf() { return 99; } };
    expect(Number(obj)).toBe(99);
    const obj2 = { toString() { return "hello"; } };
    expect(String(obj2)).toBe("hello");
  });

  test("non-callable Symbol.toPrimitive throws TypeError", () => {
    const obj = {
      [Symbol.toPrimitive]: 42,
      valueOf() { return "fallback"; },
    };
    expect(() => { obj + ""; }).toThrow(TypeError);
  });

  test("Symbol.toPrimitive can return null", () => {
    const obj = {
      [Symbol.toPrimitive]() { return null; },
    };
    expect(String(obj)).toBe("null");
  });

  test("Symbol.toPrimitive can return undefined", () => {
    const obj = {
      [Symbol.toPrimitive]() { return undefined; },
    };
    expect(String(obj)).toBe("undefined");
  });

  test("Symbol.toPrimitive can return a boolean", () => {
    const obj = {
      [Symbol.toPrimitive]() { return true; },
    };
    expect(Number(obj)).toBe(1);
  });

  test("Symbol.toPrimitive can return a bigint", () => {
    const obj = {
      [Symbol.toPrimitive]() { return 42n; },
    };
    expect(String(obj)).toBe("42");
  });

  test("Symbol.toPrimitive receives the object as this", () => {
    const obj = {
      name: "test",
      [Symbol.toPrimitive](hint) { return this.name; },
    };
    expect(String(obj)).toBe("test");
  });
});

// === Symbol is a primitive type — ES2026 §6.1 ===

describe("Symbol as primitive return from valueOf/toString", () => {
  test("toString returning a Symbol is accepted by ToPrimitive", () => {
    const sym = Symbol("foo");
    const target = { [sym]: "match" };
    const key = { toString() { return sym; } };
    expect(target[key]).toBe("match");
  });

  test("valueOf returning a Symbol is accepted when toString returns non-primitive", () => {
    const sym = Symbol("bar");
    const target = { [sym]: "found" };
    const key = { toString() { return {}; }, valueOf() { return sym; } };
    expect(target[key]).toBe("found");
  });
});

// === Object(symbol) wrapper — ES2026 §7.1.18 ToObject ===

describe("Object(symbol) wrapper", () => {
  test("typeof Object(symbol) is 'object'", () => {
    expect(typeof Object(Symbol("x"))).toBe("object");
  });

  test("wrapper preserves description", () => {
    const w = Object(Symbol("desc"));
    expect(w.description).toBe("desc");
  });

  test("wrapper description is undefined for Symbol()", () => {
    expect(Object(Symbol()).description).toBe(undefined);
  });

  test("wrapper toString returns display string", () => {
    expect(Object(Symbol("t")).toString()).toBe("Symbol(t)");
  });

  test("wrapper valueOf returns the underlying symbol", () => {
    const s = Symbol("v");
    expect(Object(s).valueOf()).toBe(s);
  });

  test("wrapper toPrimitive returns the underlying symbol", () => {
    const s = Symbol("p");
    expect(Object(s)[Symbol.toPrimitive]("default")).toBe(s);
  });

  test("wrapper prototype is Symbol.prototype", () => {
    expect(Object.getPrototypeOf(Object(Symbol("x")))).toBe(Symbol.prototype);
  });

  test("Object.getPrototypeOf(symbol) is Symbol.prototype", () => {
    expect(Object.getPrototypeOf(Symbol("x"))).toBe(Symbol.prototype);
  });

  test("Symbol.prototype methods work with call on wrapper", () => {
    const s = Symbol("call");
    const w = Object(s);
    expect(Symbol.prototype.valueOf.call(w)).toBe(s);
    expect(Symbol.prototype.toString.call(w)).toBe("Symbol(call)");
  });
});

// === Symbol wrapper as property key — ToPropertyKey dispatch ===

describe("Object(symbol) as property key", () => {
  const sym = Symbol("key");
  const wrapped = Object(sym);

  test("obj[wrapper] = value and obj[wrapper] read back", () => {
    const obj = {};
    obj[wrapped] = "set";
    expect(obj[wrapped]).toBe("set");
    expect(obj[sym]).toBe("set");
  });

  test("wrapper in obj", () => {
    const obj = { [sym]: 1 };
    expect(wrapped in obj).toBe(true);
  });

  test("delete obj[wrapper]", () => {
    const obj = { [sym]: 1 };
    expect(delete obj[wrapped]).toBe(true);
    expect(sym in obj).toBe(false);
  });

  test("Object.hasOwn with wrapper key", () => {
    const obj = { [sym]: 1 };
    expect(Object.hasOwn(obj, wrapped)).toBe(true);
  });

  test("obj.hasOwnProperty with wrapper key", () => {
    const obj = { [sym]: 1 };
    expect(obj.hasOwnProperty(wrapped)).toBe(true);
  });

  test("Object.getOwnPropertyDescriptor with wrapper key", () => {
    const obj = { [sym]: 42 };
    const desc = Object.getOwnPropertyDescriptor(obj, wrapped);
    expect(desc.value).toBe(42);
    expect(desc.writable).toBe(true);
  });

  test("Object.defineProperty with wrapper key", () => {
    const obj = {};
    Object.defineProperty(obj, wrapped, { value: "defined", writable: false });
    expect(obj[sym]).toBe("defined");
    const desc = Object.getOwnPropertyDescriptor(obj, sym);
    expect(desc.writable).toBe(false);
  });

  test("Reflect.get with wrapper key", () => {
    const obj = { [sym]: "rget" };
    expect(Reflect.get(obj, wrapped)).toBe("rget");
  });

  test("Reflect.set with wrapper key", () => {
    const obj = {};
    expect(Reflect.set(obj, wrapped, "rset")).toBe(true);
    expect(obj[sym]).toBe("rset");
  });

  test("Reflect.has with wrapper key", () => {
    const obj = { [sym]: 1 };
    expect(Reflect.has(obj, wrapped)).toBe(true);
  });

  test("Reflect.deleteProperty with wrapper key", () => {
    const obj = { [sym]: 1 };
    expect(Reflect.deleteProperty(obj, wrapped)).toBe(true);
    expect(sym in obj).toBe(false);
  });

  test("Reflect.getOwnPropertyDescriptor with wrapper key", () => {
    const obj = { [sym]: "rdesc" };
    const desc = Reflect.getOwnPropertyDescriptor(obj, wrapped);
    expect(desc.value).toBe("rdesc");
  });

  test("Reflect.defineProperty with wrapper key", () => {
    const obj = {};
    Reflect.defineProperty(obj, wrapped, { value: "rdef", configurable: true });
    expect(obj[sym]).toBe("rdef");
  });

  test("property increment with wrapper key", () => {
    const obj = { [sym]: 10 };
    obj[wrapped]++;
    expect(obj[sym]).toBe(11);
  });
});
