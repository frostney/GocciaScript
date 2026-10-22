/*---
description: A Proxy has [[Call]] and [[Construct]] exactly when its target had them when the Proxy was created, through chains, bound functions, classes and revocation
features: [Proxy, Reflect, class]
---*/

// ES2026 §10.5.15 ProxyCreate steps 5-6 give a Proxy [[Call]] only when its
// target is callable and [[Construct]] only when its target is a constructor,
// and nothing removes them later: a revoked Proxy keeps both, and its [[Call]]
// and [[Construct]] throw TypeError (§10.5.12 and §10.5.13 step 1,
// ValidateNonRevokedProxy). typeof answers "function" exactly when the object
// has [[Call]] (§13.5.3.1).
const nest = (depth, target) => {
  let proxy = target;
  for (const _ of Array.from({ length: depth })) {
    proxy = new Proxy(proxy, {});
  }
  return proxy;
};

// A trap answers instead of the target, so these ask only whether the Proxy
// around the value has [[Call]] or [[Construct]], which it takes from the
// value: they work for a revoked value too.
const hasCall = (value) => {
  try {
    return new Proxy(value, { apply: () => true })();
  } catch (error) {
    if (error instanceof TypeError) return false;
    throw error;
  }
};
const hasConstruct = (value) => {
  try {
    return typeof new (new Proxy(value, { construct: () => ({}) }))() === "object";
  } catch (error) {
    if (error instanceof TypeError) return false;
    throw error;
  }
};

describe("a chain of Proxies 1,000 deep", () => {
  test("around an arrow function is callable and not a constructor", () => {
    const proxy = nest(1000, (a) => a + 1);
    expect(typeof proxy).toBe("function");
    expect(proxy(1)).toBe(2);
    expect(hasCall(proxy)).toBe(true);
    expect(hasConstruct(proxy)).toBe(false);
    expect(() => new proxy()).toThrow(TypeError);
  });

  test("around a method is callable and not a constructor", () => {
    const proxy = nest(1000, ({ m() { return 9; } }).m);
    expect(typeof proxy).toBe("function");
    expect(proxy()).toBe(9);
    expect(hasConstruct(proxy)).toBe(false);
  });

  test("around a class is a constructor, and calling it throws", () => {
    class Point {
      constructor(x) { this.x = x; }
    }
    const proxy = nest(1000, Point);
    expect(typeof proxy).toBe("function");
    expect(hasCall(proxy)).toBe(true);
    expect(hasConstruct(proxy)).toBe(true);
    const point = new proxy(3);
    expect(point instanceof Point).toBe(true);
    expect(point.x).toBe(3);
    expect(Reflect.construct(proxy, [4]).x).toBe(4);
    expect(() => proxy(1)).toThrow(TypeError);
  });

  test("around an ordinary object is neither callable nor a constructor", () => {
    const proxy = nest(1000, {});
    expect(typeof proxy).toBe("object");
    expect(hasCall(proxy)).toBe(false);
    expect(hasConstruct(proxy)).toBe(false);
    expect(() => proxy()).toThrow(TypeError);
    expect(() => new proxy()).toThrow(TypeError);
    expect(() => Reflect.apply(proxy, undefined, [])).toThrow(TypeError);
  });
});

describe("Proxies around bound functions", () => {
  test("a bound arrow function is callable and not a constructor", () => {
    const proxy = nest(10, ((a, b) => a + b).bind(null, 1));
    expect(typeof proxy).toBe("function");
    expect(proxy(2)).toBe(3);
    expect(hasConstruct(proxy)).toBe(false);
    expect(() => new proxy()).toThrow(TypeError);
  });

  test("a bound class is a constructor that receives the bound arguments", () => {
    class Pair {
      constructor(a, b) { this.sum = a + b; }
    }
    const proxy = nest(10, Pair.bind(null, 1));
    expect(hasConstruct(proxy)).toBe(true);
    expect(new proxy(2).sum).toBe(3);
    // newTarget is the Proxy, whose "prototype" is the bound function's, which
    // has none, so the instance gets %Object.prototype% (§10.1.14 step 3).
    expect(Object.getPrototypeOf(new proxy(2))).toBe(Object.prototype);
  });

  test("a chain that alternates Proxies with bound functions keeps the class's [[Construct]]", () => {
    class Made {}
    let f = Made;
    for (const _ of Array.from({ length: 500 })) {
      f = Function.prototype.bind.call(new Proxy(f, {}), null);
    }
    const proxy = new Proxy(f, {});
    expect(typeof proxy).toBe("function");
    expect(hasConstruct(proxy)).toBe(true);
    let constructed = 0;
    const counted = new Proxy(class { constructor() { constructed += 1; } }, {});
    let h = counted;
    for (const _ of Array.from({ length: 500 })) {
      h = Function.prototype.bind.call(new Proxy(h, {}), null);
    }
    expect(typeof new (new Proxy(h, {}))()).toBe("object");
    expect(constructed).toBe(1);

    let g = () => 5;
    for (const _ of Array.from({ length: 500 })) {
      g = Function.prototype.bind.call(new Proxy(g, {}), null);
    }
    expect(new Proxy(g, {})()).toBe(5);
    expect(hasConstruct(new Proxy(g, {}))).toBe(false);
  });
});

describe("a class extending a chain of Proxies", () => {
  test("constructs through the chain", () => {
    class Base {
      constructor() { this.base = true; }
    }
    class Derived extends nest(100, Base) {}
    const made = new Derived();
    expect(made.base).toBe(true);
    expect(made instanceof Derived).toBe(true);
    expect(made instanceof Base).toBe(true);
  });
});

describe("a revoked Proxy", () => {
  test("keeps typeof and throws TypeError when called or constructed", () => {
    const fn = Proxy.revocable(() => 1, {});
    const cls = Proxy.revocable(class {}, {});
    const obj = Proxy.revocable({}, {});
    fn.revoke();
    cls.revoke();
    obj.revoke();
    expect(typeof fn.proxy).toBe("function");
    expect(typeof cls.proxy).toBe("function");
    expect(typeof obj.proxy).toBe("object");
    expect(() => fn.proxy()).toThrow(TypeError);
    expect(() => Reflect.apply(fn.proxy, undefined, [])).toThrow(TypeError);
    expect(() => new cls.proxy()).toThrow(TypeError);
    expect(() => Reflect.construct(cls.proxy, [])).toThrow(TypeError);
  });

  test("keeps [[Call]] and [[Construct]], so a Proxy around it is still callable and a constructor", () => {
    const fn = Proxy.revocable(() => 1, {});
    const cls = Proxy.revocable(class {}, {});
    const obj = Proxy.revocable({}, {});
    fn.revoke();
    cls.revoke();
    obj.revoke();
    expect(hasCall(fn.proxy)).toBe(true);
    expect(hasConstruct(fn.proxy)).toBe(false);
    expect(hasCall(cls.proxy)).toBe(true);
    expect(hasConstruct(cls.proxy)).toBe(true);
    expect(hasCall(obj.proxy)).toBe(false);
    expect(hasConstruct(obj.proxy)).toBe(false);
    expect(Function.prototype.toString.call(fn.proxy)).toContain("[native code]");
  });

  test("revoked inside a chain leaves the outer Proxies callable, and a call through them throws TypeError", () => {
    const { proxy, revoke } = Proxy.revocable(nest(100, () => 1), {});
    const outer = nest(100, proxy);
    expect(outer()).toBe(1);
    revoke();
    expect(typeof outer).toBe("function");
    expect(hasCall(outer)).toBe(true);
    expect(() => outer()).toThrow(TypeError);
  });

  test("revoked inside a chain around a class leaves the outer Proxies constructors, and new through them throws TypeError", () => {
    const { proxy, revoke } = Proxy.revocable(class {}, {});
    const outer = nest(100, proxy);
    revoke();
    expect(hasConstruct(outer)).toBe(true);
    expect(() => new outer()).toThrow(TypeError);
  });

  test("created over a revoked Proxy takes its [[Call]] and [[Construct]]", () => {
    const cls = Proxy.revocable(class {}, {});
    cls.revoke();
    const later = new Proxy(cls.proxy, {});
    expect(typeof later).toBe("function");
    expect(hasConstruct(later)).toBe(true);
    expect(() => later()).toThrow(TypeError);
    expect(() => new later()).toThrow(TypeError);
  });
});
