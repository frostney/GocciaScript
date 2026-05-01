/*---
description: Symbol creation and basic behavior
features: [Symbol]
---*/

test("Symbol creates unique values", () => {
  const s1 = Symbol();
  const s2 = Symbol();
  expect(s1).not.toBe(s2);
});

test("Symbol with description via String()", () => {
  const s = Symbol("test");
  expect(String(s)).toBe("Symbol(test)");
});

test("Symbol description is converted with ToString", () => {
  const calls = [];
  const s = Symbol({
    valueOf() {
      calls.push("valueOf");
      return {};
    },
    toString() {
      calls.push("toString");
      return "object description";
    },
  });

  expect(s.description).toBe("object description");
  expect(calls).toEqual(["toString"]);
});

test("Symbol description conversion rejects symbols", () => {
  expect(() => Symbol(Symbol("description"))).toThrow(TypeError);
});

test("Symbol without description via String()", () => {
  const s = Symbol();
  expect(String(s)).toBe("Symbol()");
});

test("typeof Symbol returns symbol", () => {
  const s = Symbol("foo");
  expect(typeof s).toBe("symbol");
});

test("Symbols with same description are not equal", () => {
  const s1 = Symbol("same");
  const s2 = Symbol("same");
  expect(s1).not.toBe(s2);
  expect(s1 === s2).toBe(false);
});

test("Symbol as object property key", () => {
  const key = Symbol("myKey");
  const obj = {};
  obj[key] = "value";
  expect(obj[key]).toBe("value");
});

test("Symbol properties are not enumerable by string keys", () => {
  const sym = Symbol("hidden");
  const obj = { visible: true, [sym]: "secret" };
  expect(Object.keys(obj)).toEqual(["visible"]);
});

test("Symbol in computed property of object literal", () => {
  const sym = Symbol("name");
  const obj = { [sym]: 42 };
  expect(obj[sym]).toBe(42);
});

test("Symbol.prototype exists", () => {
  expect(typeof Symbol.prototype).toBe("object");
});

test("symbol.toString() returns descriptive string", () => {
  const s = Symbol("foo");
  expect(s.toString()).toBe("Symbol(foo)");
  expect(Symbol().toString()).toBe("Symbol()");
});

test("symbol.description returns the description", () => {
  expect(Symbol("foo").description).toBe("foo");
  expect(Symbol().description).toBe(undefined);
});

test("symbol.toString with non-symbol receiver throws TypeError", () => {
  const fn = Symbol("foo").toString;
  expect(() => fn.call(42)).toThrow(TypeError);
  expect(() => fn.call("hello")).toThrow(TypeError);
  expect(() => fn.call({})).toThrow(TypeError);
  expect(() => fn.call(null)).toThrow(TypeError);
  expect(() => fn.call(undefined)).toThrow(TypeError);
});

test("symbol.valueOf returns the symbol", () => {
  const s = Symbol("foo");
  expect(s.valueOf()).toBe(s);
  expect(Symbol.prototype.valueOf.call(s)).toBe(s);
});

test("Symbol.prototype.valueOf with non-symbol receiver throws TypeError", () => {
  const fn = Symbol.prototype.valueOf;
  expect(() => fn.call(42)).toThrow(TypeError);
  expect(() => fn.call("hello")).toThrow(TypeError);
  expect(() => fn.call({})).toThrow(TypeError);
  expect(() => fn.call(null)).toThrow(TypeError);
  expect(() => fn.call(undefined)).toThrow(TypeError);
});

test("Symbol.prototype[Symbol.toPrimitive] returns the symbol regardless of hint", () => {
  const s = Symbol("foo");
  expect(s[Symbol.toPrimitive]("string")).toBe(s);
  expect(s[Symbol.toPrimitive]("number")).toBe(s);
  expect(s[Symbol.toPrimitive]("default")).toBe(s);
  expect(Symbol.prototype[Symbol.toPrimitive].call(s)).toBe(s);
});

test("Symbol.prototype[Symbol.toPrimitive] with non-symbol receiver throws TypeError", () => {
  const fn = Symbol.prototype[Symbol.toPrimitive];
  expect(() => fn.call(42, "string")).toThrow(TypeError);
  expect(() => fn.call("hello", "string")).toThrow(TypeError);
  expect(() => fn.call({}, "string")).toThrow(TypeError);
  expect(() => fn.call(null, "string")).toThrow(TypeError);
  expect(() => fn.call(undefined, "string")).toThrow(TypeError);
});

test("Symbol.prototype[Symbol.toStringTag] is 'Symbol'", () => {
  expect(Symbol.prototype[Symbol.toStringTag]).toBe("Symbol");
});

test("Symbol.prototype.constructor is Symbol", () => {
  expect(Symbol.prototype.constructor).toBe(Symbol);
});

test("symbol-keyed property lookup on Symbol primitive resolves via prototype", () => {
  const s = Symbol("foo");
  expect(typeof s[Symbol.toPrimitive]).toBe("function");
  expect(s[Symbol.toPrimitive]).toBe(Symbol.prototype[Symbol.toPrimitive]);
  expect(s[Symbol.toStringTag]).toBe("Symbol");
});

test("Symbol.toPrimitive[Symbol.toPrimitive]() returns the well-known symbol itself", () => {
  expect(Symbol.toPrimitive[Symbol.toPrimitive]()).toBe(Symbol.toPrimitive);
});

test("Symbol.unscopables is a unique well-known symbol", () => {
  expect(typeof Symbol.unscopables).toBe("symbol");
  expect(Symbol.unscopables).not.toBe(Symbol.iterator);
});

test("new Symbol() throws TypeError", () => {
  expect(() => new Symbol()).toThrow(TypeError);
  expect(() => new Symbol("desc")).toThrow(TypeError);
});

test("Symbol is constructable as a newTarget only", () => {
  const value = Reflect.construct(class {}, [], Symbol);
  expect(Object.getPrototypeOf(value)).toBe(Symbol.prototype);
});

test("Symbol() distinguishes undefined description from empty-string description", () => {
  expect(Symbol().description).toBe(undefined);
  expect(Symbol(undefined).description).toBe(undefined);
  expect(Symbol("").description).toBe("");
  expect(String(Symbol(""))).toBe("Symbol()");
});

test("Symbol.for and Symbol.keyFor are not constructable", () => {
  // bracket notation because `for` is a reserved word
  const symbolFor = Symbol["for"];
  const symbolKeyFor = Symbol.keyFor;
  expect(() => new symbolFor("k")).toThrow();
  expect(() => new symbolKeyFor(Symbol("k"))).toThrow();
});

test("Symbol.prototype.toString and valueOf are not constructable", () => {
  const toStr = Symbol.prototype.toString;
  const valOf = Symbol.prototype.valueOf;
  expect(() => new toStr()).toThrow();
  expect(() => new valOf()).toThrow();
});

test("Symbol.prototype's [[Prototype]] is Object.prototype", () => {
  expect(Object.getPrototypeOf(Symbol.prototype)).toBe(Object.prototype);
});

test("Object.getPrototypeOf(symbolPrimitive) returns Symbol.prototype", () => {
  const s = Symbol("x");
  expect(Object.getPrototypeOf(s)).toBe(Symbol.prototype);
});

test("symbol primitives do not have an own description property", () => {
  const s = Symbol("desc");
  expect(s.hasOwnProperty("description")).toBe(false);
  expect(Object.hasOwn(s, "description")).toBe(false);
});

test("assigning symbol-keyed properties to symbol primitives throws", () => {
  const s = Symbol("target");
  const key = Symbol("key");

  expect(() => { s[Symbol.toStringTag] = "tag"; }).toThrow(TypeError);
  expect(() => { s[key] = "value"; }).toThrow(TypeError);
});
