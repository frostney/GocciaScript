/*---
description: A prototype cycle closed through a Proxy makes a lookup that walks it throw RangeError
features: [Proxy, Reflect, prototype-chain]
---*/

// ES2026 §10.1.2.1 OrdinarySetPrototypeOf stops its cycle check at an object
// whose [[GetPrototypeOf]] is not the ordinary one, so a cycle through a Proxy
// is accepted. A lookup that has to go round it never ends; the engine ends it
// with a RangeError, as engines do for a native stack overflow.

const makeCycle = () => {
  const object = {};
  const proxy = new Proxy(Object.create(object), {});
  Object.setPrototypeOf(object, proxy);
  return { object, proxy };
};

describe("lookups around a prototype cycle through a Proxy", () => {
  const operations = [
    ["a missing property read", (o) => o.missing],
    ["an index read", (o) => o[0]],
    ["a symbol-keyed read", (o) => o[Symbol.iterator]],
    ["an in check", (o) => "k" in o],
    ["Reflect.get", (o) => Reflect.get(o, "k")],
    ["Reflect.has", (o) => Reflect.has(o, "k")],
    ["Reflect.set", (o) => Reflect.set(o, "k", 1)],
    ["an assignment", (o) => { o.missing = 1; }],
    ["a symbol-keyed assignment", (o) => { o[Symbol("key")] = 1; }],
    ["a toString call", (o) => o.toString()],
    ["String conversion", (o) => String(o)],
    ["a template literal", (o) => `${o}`],
    ["string concatenation", (o) => o + ""],
    ["JSON.stringify", (o) => JSON.stringify(o)],
    ["a hasOwnProperty call", (o) => o.hasOwnProperty("x")],
  ];

  test.each(operations)("%s from the ordinary object throws RangeError", (_, operation) => {
    const { object } = makeCycle();
    expect(() => operation(object)).toThrow(RangeError);
  });

  test.each(operations)("%s from the Proxy throws RangeError", (_, operation) => {
    const { proxy } = makeCycle();
    expect(() => operation(proxy)).toThrow(RangeError);
  });

  test("instanceof throws RangeError", () => {
    const { object } = makeCycle();
    class Unrelated {}
    expect(() => object instanceof Unrelated).toThrow(RangeError);
  });

  test("isPrototypeOf throws RangeError", () => {
    const { object } = makeCycle();
    class Unrelated {}
    expect(() => Unrelated.prototype.isPrototypeOf(object)).toThrow(RangeError);
  });

  test("an assignment leaves the object unchanged", () => {
    const { object } = makeCycle();
    expect(() => { object.missing = 1; }).toThrow(RangeError);
    expect(Object.hasOwn(object, "missing")).toBe(false);
  });

  test("an own property is still found without walking the cycle", () => {
    const { object, proxy } = makeCycle();
    // Assigning a new property walks the cycle for a setter, so define it.
    Object.defineProperty(object, "own", { value: "here" });
    expect(object.own).toBe("here");
    expect(proxy.own).toBe("here");
    expect("own" in proxy).toBe(true);
  });

  test("the engine keeps working after the RangeError", () => {
    const { object } = makeCycle();
    for (const i of [1, 2, 3]) {
      expect(() => object.missing).toThrow(RangeError);
    }
    const parent = { inherited: 1 };
    expect(Object.create(parent).inherited).toBe(1);
  });
});
