/*---
description: Descriptor fields and staged descriptor batches stay reachable across the guest code that reads the next field
features: [Goccia.gc, Proxy, Symbol]
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

describe.runIf(hasGoccia)("property descriptor GC roots", () => {
  test("a descriptor value survives the writable getter that follows it", () => {
    // ES2026 §6.2.5.5 reads `value` before `writable`. The object the value
    // getter returns is referenced by nothing but the extraction's own local
    // while the writable getter runs.
    const target = {};
    Object.defineProperty(target, "p", {
      get value() {
        return { tag: "fresh" };
      },
      get writable() {
        churn();
        return true;
      },
      enumerable: true,
      configurable: true,
    });

    expect(target.p.tag).toBe("fresh");
  });

  test("a descriptor value survives the get and set probes that follow it", () => {
    // `value` is read at step 5, `get` at step 7 and `set` at step 8. A
    // descriptor that only *probes* for get/set still runs guest code on a
    // Proxy, and the mixed-descriptor check happens after all three reads.
    const source = {
      get value() {
        return { tag: "probed" };
      },
      get enumerable() {
        return true;
      },
      get configurable() {
        return true;
      },
    };
    const probing = new Proxy(source, {
      has(t, k) {
        churn();
        return k in t;
      },
      get(t, k, r) {
        churn();
        return Reflect.get(t, k, r);
      },
    });

    const target = {};
    Object.defineProperty(target, "p", probing);
    expect(target.p.tag).toBe("probed");
  });

  test("a descriptor getter survives the setter read that follows it", () => {
    const target = {};
    Object.defineProperty(target, "p", {
      get get() {
        return () => "from accessor";
      },
      get set() {
        churn();
        return undefined;
      },
      enumerable: true,
      configurable: true,
    });

    expect(target.p).toBe("from accessor");
  });

  test("a staged batch survives a later key's descriptor read", () => {
    // §20.1.2.3.1 collects every descriptor before defining any of them. The
    // staged descriptors are plain native records the collector cannot trace,
    // so the value captured for `a` is reachable from nowhere while `b`'s
    // value getter runs.
    const target = {};
    Object.defineProperties(target, {
      a: {
        get value() {
          return { tag: "a" };
        },
        enumerable: true,
        configurable: true,
      },
      b: {
        get value() {
          churn();
          return { tag: "b" };
        },
        enumerable: true,
        configurable: true,
      },
    });

    expect(target.a.tag).toBe("a");
    expect(target.b.tag).toBe("b");
  });

  test("a staged accessor batch survives a later key's descriptor read", () => {
    const target = {};
    Object.defineProperties(target, {
      a: {
        get get() {
          return () => "a value";
        },
        enumerable: true,
        configurable: true,
      },
      b: {
        get get() {
          churn();
          return () => "b value";
        },
        enumerable: true,
        configurable: true,
      },
    });

    expect(target.a).toBe("a value");
    expect(target.b).toBe("b value");
  });

  test("a staged symbol batch survives a later key's descriptor read", () => {
    const first = Symbol("first");
    const second = Symbol("second");
    const properties = {};
    properties[first] = {
      get value() {
        return { tag: "first" };
      },
      enumerable: true,
      configurable: true,
    };
    properties[second] = {
      get value() {
        churn();
        return { tag: "second" };
      },
      enumerable: true,
      configurable: true,
    };

    const target = {};
    Object.defineProperties(target, properties);
    expect(target[first].tag).toBe("first");
    expect(target[second].tag).toBe("second");
  });

  test("Object.create stages its properties under the same roots", () => {
    const created = Object.create(null, {
      a: {
        get value() {
          return { tag: "a" };
        },
        enumerable: true,
        configurable: true,
      },
      b: {
        get value() {
          churn();
          return { tag: "b" };
        },
        enumerable: true,
        configurable: true,
      },
    });

    expect(created.a.tag).toBe("a");
    expect(created.b.tag).toBe("b");
  });

  test("a proxy properties object's key list survives its own descriptor reads", () => {
    // The keys come back from ownKeys as fresh values held only by a native
    // array, and the capture loop runs a getter between one key and the next.
    const backing = {
      a: {
        get value() {
          return { tag: "a" };
        },
        enumerable: true,
        configurable: true,
      },
      b: {
        get value() {
          churn();
          return { tag: "b" };
        },
        enumerable: true,
        configurable: true,
      },
    };
    const properties = new Proxy(backing, {
      ownKeys(t) {
        return ["a" + "", "b" + ""];
      },
    });

    const target = {};
    Object.defineProperties(target, properties);
    expect(target.a.tag).toBe("a");
    expect(target.b.tag).toBe("b");
  });

  test("Reflect.defineProperty extracts under the same roots", () => {
    const target = {};
    const ok = Reflect.defineProperty(target, "p", {
      get value() {
        return { tag: "reflected" };
      },
      get writable() {
        churn();
        return true;
      },
      enumerable: true,
      configurable: true,
    });

    expect(ok).toBe(true);
    expect(target.p.tag).toBe("reflected");
  });
});
