/*---
description: Chains of bound functions, chains that alternate Proxies with bound functions, and classes that extend a Proxy forward [[Call]] and [[Construct]], and a deep chain ends in RangeError instead of a crash
features: [Proxy, Reflect, class]
---*/

// ES2026 §10.4.1.1 and §10.4.1.2: a bound function's [[Call]] and
// [[Construct]] forward to its [[BoundTargetFunction]] with [[BoundArguments]]
// in front, and §10.5.12 and §10.5.13 forward a Proxy without a trap to its
// [[ProxyTarget]]. The specification bounds none of these chains.

// `bind` reads the target's name and builds "bound " + name, so a long chain
// would carry names whose total length grows with the square of its depth.
// Reading `name` through these handlers gives undefined, which keeps every
// bound name short.
const nameless = { get: (target, key, receiver) => (key === "name" ? undefined : Reflect.get(target, key, receiver)) };
const alternate = (depth, target) => {
  let f = target;
  for (const _ of Array.from({ length: depth })) {
    f = Function.prototype.bind.call(new Proxy(f, nameless), null);
  }
  return f;
};
const bindChain = (depth, target, ...args) => {
  let f = target;
  for (const _ of Array.from({ length: depth })) {
    f = f.bind(null, ...args);
    Object.defineProperty(f, "name", { value: "" });
  }
  return f;
};
const classesExtendingProxies = (depth, base) => {
  let C = base;
  for (const _ of Array.from({ length: depth })) {
    const B = new Proxy(C, {});
    C = class extends B {};
  }
  return C;
};

describe("a chain of bound functions", () => {
  test("a call reaches the target with the innermost bound this and every bound argument, innermost first", () => {
    const target = ({ m(...args) { return [this.tag, ...args].join(","); } }).m;
    const inner = target.bind({ tag: "inner" }, "a");
    const outer = inner.bind({ tag: "outer" }, "b").bind({ tag: "outermost" }, "c");
    expect(outer("d")).toBe("inner,a,b,c,d");
  });

  test("a chain 5,000 deep is called, constructed and reports constructability", () => {
    class Made {}
    const callable = bindChain(5000, (...args) => args.length, 1);
    expect(callable(2)).toBe(5001);
    const constructable = bindChain(5000, Made);
    expect(new constructable() instanceof Made).toBe(true);
    expect(Reflect.construct(constructable, []) instanceof Made).toBe(true);
    expect(typeof Reflect.construct(class {}, [], constructable)).toBe("object");
    expect(() => Reflect.construct(class {}, [], bindChain(5000, () => 1))).toThrow(TypeError);
  });

  test("new.target is the innermost target when the bound function is new.target", () => {
    let seen;
    class Made { constructor() { seen = new.target; } }
    new (bindChain(3, Made))();
    expect(seen).toBe(Made);
  });
});

describe("a chain that alternates Proxies with bound functions", () => {
  test("500 levels forward a call and a construction", () => {
    expect(alternate(500, () => 1)()).toBe(1);
    expect(typeof alternate(500, () => 1)).toBe("function");
    // new.target stays the outermost Proxy (§10.4.1.2 step 5 replaces only a
    // bound function that is new.target itself), and its "prototype" is the
    // bound function's, which is undefined; §10.1.14
    // GetPrototypeFromConstructor then falls back to %Object.prototype%.
    let constructed = false;
    class Made { constructor() { constructed = true; } }
    const instance = new (alternate(500, Made))();
    expect(constructed).toBe(true);
    expect(Object.getPrototypeOf(instance)).toBe(Object.prototype);
  });

  test("constructability steps through the chain", () => {
    expect(typeof Reflect.construct(class {}, [], alternate(500, class {}))).toBe("object");
    expect(() => Reflect.construct(class {}, [], alternate(500, () => 1))).toThrow(TypeError);
    expect(() => new (alternate(500, () => 1))()).toThrow(TypeError);
  });
});

describe("classes that extend a Proxy of the previous class", () => {
  test("300 levels construct an instance of every class in the chain", () => {
    class Base { constructor() { this.base = true; } }
    const C = classesExtendingProxies(300, Base);
    const instance = new C();
    expect(instance instanceof C).toBe(true);
    expect(instance instanceof Base).toBe(true);
    expect(instance.base).toBe(true);
  });
});

// The engine counts each forward through these chains against the same bound
// as a Proxy nest (MAX_PROPERTY_DELEGATION_DEPTH, 2,000), and checks the
// native stack once they nest, so these chains end in RangeError however much
// stack is left. Node.js has no such bound and completes the call and `new`
// through an alternating chain.
describe.runIf(typeof Goccia !== "undefined")("chains past the engine's bound", () => {
  test("a call and a construction through 3,000 alternating levels throw RangeError", () => {
    expect(() => alternate(3000, () => 1)()).toThrow(RangeError);
    expect(() => new (alternate(3000, class {}))()).toThrow(RangeError);
    expect(() => Reflect.construct(alternate(3000, class {}), [])).toThrow(RangeError);
    expect(typeof alternate(3000, () => 1)).toBe("function");
  });

  test("constructing the last of 3,000 classes that extend Proxies throws RangeError", () => {
    expect(() => new (classesExtendingProxies(3000, class {}))()).toThrow(RangeError);
  });

  test("instanceof through a chain of 3,000 bound functions throws RangeError", () => {
    class Made {}
    expect(() => new Made() instanceof bindChain(3000, Made)).toThrow(RangeError);
  });
});

// Deep enough to exhaust the native stack without the bound; GocciaScript only,
// on 64-bit builds, like the 20,000-deep Proxy nests in nested-targets.js.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

describe.runIf(is64Bit)("chains 20,000 deep", () => {
  test("alternating Proxies and bound functions throw RangeError, and the engine keeps working", () => {
    expect(() => alternate(20000, () => 1)()).toThrow(RangeError);
    expect(() => new (alternate(20000, class {}))()).toThrow(RangeError);
    expect([1, 2].map((x) => x * 2)).toEqual([2, 4]);
  });

  test("classes extending Proxies throw RangeError, and the engine keeps working", () => {
    expect(() => new (classesExtendingProxies(20000, class {}))()).toThrow(RangeError);
    expect(new (class {})() instanceof Object).toBe(true);
  });

  test("a chain of bound functions is called and constructed in a loop", () => {
    class Made {}
    expect(bindChain(20000, () => 1)()).toBe(1);
    expect(new (bindChain(20000, Made))() instanceof Made).toBe(true);
  });
});
