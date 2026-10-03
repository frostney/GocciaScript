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

  test("a symbol-keyed in check from the Proxy throws RangeError", () => {
    const { proxy } = makeCycle();
    expect(() => Symbol.iterator in proxy).toThrow(RangeError);
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

describe("a prototype cycle through a Proxy that also holds other objects", () => {
  test("a read around a cycle through 30 arrays throws RangeError", () => {
    const first = [];
    let last = first;
    for (const i of Array.from({ length: 29 })) {
      const next = [];
      Object.setPrototypeOf(last, next);
      last = next;
    }
    Object.setPrototypeOf(last, new Proxy(Object.create(first), {}));
    expect(() => first.missing).toThrow(RangeError);
    expect(() => "missing" in first).toThrow(RangeError);
    expect(() => { first.missing = 1; }).toThrow(RangeError);
  });

  test("a read around a cycle through a class instance throws RangeError", () => {
    class Plain {}
    const instance = new Plain();
    Object.setPrototypeOf(instance, new Proxy(Object.create(instance), {}));
    expect(() => instance.missing).toThrow(RangeError);
  });
});

describe("Proxies nested as targets", () => {
  const nest = (depth) => {
    let proxy = { x: "found" };
    for (const i of Array.from({ length: depth })) {
      proxy = new Proxy(proxy, {});
    }
    return proxy;
  };

  // Each Proxy forwarding to its target is one native call.
  test("a read through 1,000 nested Proxies reaches the innermost target", () => {
    expect(nest(1000).x).toBe("found");
  });

  test("a read through 100,000 nested Proxies throws RangeError", () => {
    expect(() => nest(100000).x).toThrow(RangeError);
  });

  test("instanceof and isPrototypeOf through 1,000 nested Proxies reach the target's prototype", () => {
    class Found {}
    let proxy = new Found();
    for (const i of Array.from({ length: 1000 })) {
      proxy = new Proxy(proxy, {});
    }
    expect(proxy instanceof Found).toBe(true);
    expect(Found.prototype.isPrototypeOf(proxy)).toBe(true);
  });

  test("instanceof, isPrototypeOf and Object.getPrototypeOf through 100,000 nested Proxies throw RangeError", () => {
    class Unrelated {}
    const proxy = nest(100000);
    expect(() => proxy instanceof Unrelated).toThrow(RangeError);
    expect(() => Unrelated.prototype.isPrototypeOf(proxy)).toThrow(RangeError);
    expect(() => Object.getPrototypeOf(proxy)).toThrow(RangeError);
  });
});
