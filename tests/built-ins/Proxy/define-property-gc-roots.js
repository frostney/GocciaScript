/*---
description: Proxy defineProperty and getOwnPropertyDescriptor traps keep the descriptor they carry reachable across the guest code they run
features: [Goccia.gc, Proxy, Reflect, Symbol]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// A bare gc() usually leaves a freed slot readable; the allocation churn after
// it is what makes a collected temporary observable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

// A handler whose `defineProperty` is read through an accessor: reading the
// trap off the handler is guest code, and it runs before anything has looked
// at the descriptor the caller handed in.
const churningHandler = (record) => ({
  get defineProperty() {
    churn();
    return (target, key, desc) => {
      record.seen = desc.value;
      return Reflect.defineProperty(target, key, desc);
    };
  },
});

describe.runIf(hasGoccia)("Proxy descriptor GC roots", () => {
  test("defineProperty keeps the descriptor value alive across the trap lookup", () => {
    const record = {};
    const target = {};
    const proxy = new Proxy(target, churningHandler(record));

    Object.defineProperty(proxy, "p", {
      get value() {
        return { tag: "fresh" };
      },
      enumerable: true,
      configurable: true,
    });

    expect(record.seen.tag).toBe("fresh");
    expect(target.p.tag).toBe("fresh");
  });

  test("Reflect.defineProperty keeps the descriptor value alive across the trap lookup", () => {
    const record = {};
    const target = {};
    const proxy = new Proxy(target, churningHandler(record));

    const ok = Reflect.defineProperty(proxy, "p", {
      get value() {
        return { tag: "reflected" };
      },
      enumerable: true,
      configurable: true,
    });

    expect(ok).toBe(true);
    expect(record.seen.tag).toBe("reflected");
    expect(target.p.tag).toBe("reflected");
  });

  test("a symbol-keyed defineProperty keeps its descriptor value alive", () => {
    const key = Symbol("k");
    const record = {};
    const target = {};
    const proxy = new Proxy(target, churningHandler(record));

    Object.defineProperty(proxy, key, {
      get value() {
        return { tag: "symbol" };
      },
      enumerable: true,
      configurable: true,
    });

    expect(record.seen.tag).toBe("symbol");
    expect(target[key].tag).toBe("symbol");
  });

  test("an accessor descriptor survives the trap lookup", () => {
    const target = {};
    const proxy = new Proxy(target, {
      get defineProperty() {
        churn();
        return (t, k, desc) => Reflect.defineProperty(t, k, desc);
      },
    });

    Object.defineProperty(proxy, "p", {
      get get() {
        return () => "from accessor";
      },
      enumerable: true,
      configurable: true,
    });

    expect(target.p).toBe("from accessor");
  });

  test("the descriptor survives the post-trap invariant check", () => {
    // ValidateProxyDefineTrapResult re-reads the descriptor after the trap
    // returns, and it consults the target's extensibility — another trap when
    // the target is itself a proxy.
    const inner = {};
    const innerProxy = new Proxy(inner, {
      isExtensible(t) {
        churn();
        return Reflect.isExtensible(t);
      },
    });
    const outer = new Proxy(innerProxy, {
      defineProperty(t, k, desc) {
        return Reflect.defineProperty(t, k, desc);
      },
    });

    Object.defineProperty(outer, "p", {
      get value() {
        return { tag: "validated" };
      },
      enumerable: true,
      configurable: true,
    });

    expect(inner.p.tag).toBe("validated");
  });

  test("getOwnPropertyDescriptor keeps the trapped value alive across the extensibility check", () => {
    // The completed descriptor is the sole holder of the value the trap
    // produced while ProxyTargetIsExtensible runs the inner isExtensible trap.
    const marker = { tag: "trapped" };
    const inner = { p: 0 };
    const innerProxy = new Proxy(inner, {
      isExtensible(t) {
        churn();
        return Reflect.isExtensible(t);
      },
    });
    const outer = new Proxy(innerProxy, {
      getOwnPropertyDescriptor() {
        return {
          value: { tag: "trapped" },
          writable: true,
          enumerable: true,
          configurable: true,
        };
      },
    });

    const desc = Object.getOwnPropertyDescriptor(outer, "p");
    expect(desc.value.tag).toBe("trapped");
    expect(marker.tag).toBe("trapped");
  });

  test("a symbol-keyed getOwnPropertyDescriptor keeps its trapped value alive", () => {
    const key = Symbol("k");
    const inner = {};
    inner[key] = 0;
    const innerProxy = new Proxy(inner, {
      isExtensible(t) {
        churn();
        return Reflect.isExtensible(t);
      },
    });
    const outer = new Proxy(innerProxy, {
      getOwnPropertyDescriptor() {
        return {
          value: { tag: "symbol-trapped" },
          writable: true,
          enumerable: true,
          configurable: true,
        };
      },
    });

    const desc = Object.getOwnPropertyDescriptor(outer, key);
    expect(desc.value.tag).toBe("symbol-trapped");
  });

  test("a proxy properties object drives defineProperties without losing values", () => {
    const backing = {
      a: { value: { tag: "a" }, enumerable: true, configurable: true },
      b: { value: { tag: "b" }, enumerable: true, configurable: true },
    };
    const properties = new Proxy(backing, {
      getOwnPropertyDescriptor(t, k) {
        churn();
        return Reflect.getOwnPropertyDescriptor(t, k);
      },
    });

    const target = {};
    Object.defineProperties(target, properties);
    expect(target.a.tag).toBe("a");
    expect(target.b.tag).toBe("b");
  });
});
