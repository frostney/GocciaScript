/*---
description: Every internal method forwards through Proxies nested as targets, and a deep nest ends in RangeError instead of a crash
features: [Proxy, Reflect]
---*/

// ES2026 §10.5: a Proxy without a trap forwards each internal method to its
// target, which may be another Proxy. Each forward is a native call, so a deep
// enough nest runs out of native stack; the engine stops it with RangeError.
const nest = (depth, target, handler = {}) => {
  let proxy = target;
  for (const i of Array.from({ length: depth })) {
    proxy = new Proxy(proxy, handler);
  }
  return proxy;
};

describe("Proxies nested 500 deep forward every internal method", () => {
  test("getOwnPropertyDescriptor, ownKeys and keys reach the target", () => {
    const proxy = nest(500, { x: 1, [Symbol.iterator]: 2 });
    expect(Object.getOwnPropertyDescriptor(proxy, "x").value).toBe(1);
    expect(Object.getOwnPropertyDescriptor(proxy, Symbol.iterator).value).toBe(2);
    expect(Object.keys(proxy)).toEqual(["x"]);
    expect(Reflect.ownKeys(proxy).length).toBe(2);
  });

  test("defineProperty, deleteProperty and setPrototypeOf change the target", () => {
    const target = { x: 1 };
    const proxy = nest(500, target);
    Object.defineProperty(proxy, "z", { value: 3 });
    expect(target.z).toBe(3);
    expect(delete proxy.x).toBe(true);
    expect("x" in target).toBe(false);
    const proto = {};
    Object.setPrototypeOf(proxy, proto);
    expect(Object.getPrototypeOf(target)).toBe(proto);
  });

  test("isExtensible, preventExtensions and isFrozen reflect the target", () => {
    const target = {};
    const proxy = nest(500, target);
    expect(Object.isExtensible(proxy)).toBe(true);
    Object.preventExtensions(proxy);
    expect(Object.isExtensible(target)).toBe(false);
    expect(Object.isFrozen(proxy)).toBe(true);
  });

  test("call and new reach the target function and class", () => {
    expect(nest(500, () => 7)()).toBe(7);
    class Made {}
    expect(new (nest(500, Made))() instanceof Made).toBe(true);
    expect(typeof nest(500, () => 7)).toBe("function");
  });

  test("traps that are native functions forward through the nest", () => {
    // Each level checks the trap's result against its target's keys, which
    // asks the rest of the nest again, so the work doubles per level: keep
    // this nest shallow.
    const proxy = nest(8, { x: 1 }, { ownKeys: Reflect.ownKeys, getOwnPropertyDescriptor: Reflect.getOwnPropertyDescriptor });
    expect(Reflect.ownKeys(proxy)).toEqual(["x"]);
    expect(Object.getOwnPropertyDescriptor(proxy, "x").value).toBe(1);
  });
});

describe("Proxies nested 1,200 deep", () => {
  // An assignment and a `new` go down the nest twice: the innermost [[Set]]
  // defines the property on the outermost Proxy, and [[Construct]] reads
  // `prototype` from it.
  test("an assignment and a construction reach the target", () => {
    const target = {};
    const proxy = nest(1200, target);
    proxy.y = 2;
    expect(target.y).toBe(2);
    expect(Reflect.set(proxy, "z", 3)).toBe(true);
    class Made {}
    expect(new (nest(1200, Made))() instanceof Made).toBe(true);
  });
});

// The engine bounds the native calls through a nest (MAX_PROPERTY_DELEGATION_DEPTH,
// 2,500), so a 3,000-deep nest throws RangeError however much native stack is
// left. Node.js has no such bound and completes these.
describe.runIf(typeof Goccia !== "undefined")("Proxies nested 3,000 deep, past the engine's bound", () => {
  const proxy = nest(3000, { x: 1 });
  const callable = nest(3000, () => 1);
  const constructable = nest(3000, class {});
  const handlerChain = (() => {
    let handler = {};
    for (const i of Array.from({ length: 3000 })) {
      handler = new Proxy({}, handler);
    }
    return new Proxy({ x: 1 }, handler);
  })();
  const operations = [
    ["getOwnPropertyDescriptor", () => Object.getOwnPropertyDescriptor(proxy, "x")],
    ["a symbol-keyed getOwnPropertyDescriptor", () => Object.getOwnPropertyDescriptor(proxy, Symbol.iterator)],
    ["Object.keys", () => Object.keys(proxy)],
    ["Reflect.ownKeys", () => Reflect.ownKeys(proxy)],
    ["defineProperty", () => Object.defineProperty(proxy, "z", { value: 3 })],
    ["delete", () => delete proxy.x],
    ["isExtensible", () => Object.isExtensible(proxy)],
    ["preventExtensions", () => Object.preventExtensions(nest(3000, {}))],
    ["setPrototypeOf", () => Object.setPrototypeOf(proxy, {})],
    ["a call", () => callable()],
    ["new", () => new constructable()],
    ["a read through a chain of handlers", () => handlerChain.x],
  ];

  test.each(operations)("%s throws RangeError", (_, operation) => {
    expect(operation).toThrow(RangeError);
  });

  test("typeof steps through the nest without a bound", () => {
    expect(typeof callable).toBe("function");
  });
});

// 20,000 nested Proxies are deep enough to exhaust the native stack without
// the bound, and take tens of megabytes in interpreted mode, too much for a
// 32-bit process running parallel test workers.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

describe("Proxies nested 20,000 deep", () => {
  // Node.js completes some of these and throws RangeError for others; either
  // is allowed, a crash is not.
  const outcome = (operation) => {
    try {
      operation();
      return "completed";
    } catch (error) {
      return error.constructor.name;
    }
  };
  const allowed = ["completed", "RangeError"];

  test.runIf(is64Bit)("every forwarded internal method and a native trap complete or throw RangeError", () => {
    // The ownKeys trap on every level is a native function, so ownKeys and
    // keys recurse through trap calls; the other operations forward.
    const proxy = nest(20000, { x: 1 }, { ownKeys: Reflect.ownKeys });
    const operations = [
      () => Object.getOwnPropertyDescriptor(proxy, "x"),
      () => Object.getOwnPropertyDescriptor(proxy, Symbol.iterator),
      () => Object.keys(proxy),
      () => Reflect.ownKeys(proxy),
      () => Object.defineProperty(proxy, "z", { value: 3 }),
      () => delete proxy.x,
      () => Object.isExtensible(proxy),
      () => Object.isFrozen(proxy),
      () => Object.setPrototypeOf(proxy, {}),
      () => Object.preventExtensions(proxy),
    ];
    for (const operation of operations) {
      expect(allowed.includes(outcome(operation))).toBe(true);
    }
    expect(Object.getOwnPropertyDescriptor({ y: 2 }, "y").value).toBe(2);
  });

  test.runIf(is64Bit)("call and new complete or throw RangeError, and typeof sees the function", () => {
    const callable = nest(20000, () => 1);
    expect(typeof callable).toBe("function");
    expect(allowed.includes(outcome(() => callable()))).toBe(true);
    expect(allowed.includes(outcome(() => new (nest(20000, class {}))()))).toBe(true);
  });
});
