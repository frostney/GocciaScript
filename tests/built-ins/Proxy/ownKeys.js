describe("Proxy ownKeys trap", () => {
  test("intercepts Object.keys", () => {
    const target = { a: 1, b: 2, secret: 3 };
    const proxy = new Proxy(target, {
      ownKeys: (t) => {
        return ["a", "b"];
      },
    });
    expect(Object.keys(proxy)).toEqual(["a", "b"]);
  });

  test("intercepts Object.getOwnPropertyNames", () => {
    const target = { x: 1, y: 2 };
    const proxy = new Proxy(target, {
      ownKeys: () => ["x", "y", "z"],
    });
    const names = Object.getOwnPropertyNames(proxy);
    expect(names).toEqual(["x", "y", "z"]);
  });

  test("falls back to target when no ownKeys trap", () => {
    const target = { a: 1, b: 2 };
    const proxy = new Proxy(target, {});
    expect(Object.keys(proxy)).toEqual(["a", "b"]);
  });

  test("receives correct target", () => {
    const target = { x: 1 };
    let receivedTarget;
    const proxy = new Proxy(target, {
      ownKeys: (t) => {
        receivedTarget = t;
        return Object.keys(t);
      },
    });
    Object.keys(proxy);
    expect(receivedTarget).toBe(target);
  });

  test("can add virtual keys", () => {
    const proxy = new Proxy(
      {},
      {
        ownKeys: () => ["virtual1", "virtual2"],
      }
    );
    const keys = Object.getOwnPropertyNames(proxy);
    expect(keys).toEqual(["virtual1", "virtual2"]);
  });

  test("rejects extra keys through nested non-extensible proxy targets", () => {
    const target = {};
    Object.preventExtensions(target);

    const inner = new Proxy(target, {
      isExtensible() {
        return false;
      },
    });
    const outer = new Proxy(inner, {
      ownKeys() {
        return ["x"];
      },
    });

    expect(() => Reflect.ownKeys(outer)).toThrow(TypeError);
  });

  test("rejects duplicate symbol entries even when caller filters strings", () => {
    const symbol = Symbol("duplicate");
    const proxy = new Proxy({}, {
      ownKeys: () => [symbol, symbol],
    });

    expect(() => Object.keys(proxy)).toThrow(TypeError);
  });

  test("Reflect.ownKeys preserves symbol entries and trap order", () => {
    const symbol = Symbol("s");
    const proxy = new Proxy({}, {
      ownKeys: () => [symbol, "a"],
      getOwnPropertyDescriptor() {
        return {
          configurable: true,
          enumerable: true,
          value: 1,
          writable: true,
        };
      },
    });

    const keys = Reflect.ownKeys(proxy);
    expect(keys.length).toBe(2);
    expect(keys[0]).toBe(symbol);
    expect(keys[1]).toBe("a");
  });

  test("Object.getOwnPropertySymbols observes proxy ownKeys symbols", () => {
    const symbol = Symbol("visible");
    const proxy = new Proxy({}, {
      ownKeys: () => ["a", symbol],
      getOwnPropertyDescriptor() {
        return {
          configurable: true,
          enumerable: true,
          value: 1,
          writable: true,
        };
      },
    });

    expect(Object.getOwnPropertySymbols(proxy)).toEqual([symbol]);
  });

  test("rejects duplicate string entries", () => {
    const proxy = new Proxy({}, {
      ownKeys: () => ["a", "a"],
    });

    expect(() => Reflect.ownKeys(proxy)).toThrow(TypeError);
  });

  test("rejects a result missing a non-configurable string key of the target", () => {
    const target = {};
    Object.defineProperty(target, "fixed", { value: 1, configurable: false });
    const proxy = new Proxy(target, { ownKeys: () => [] });

    expect(() => Reflect.ownKeys(proxy)).toThrow(TypeError);
  });

  test("rejects a result missing a non-configurable symbol key of the target", () => {
    const symbol = Symbol("fixed");
    const target = {};
    Object.defineProperty(target, symbol, { value: 1, configurable: false });
    const proxy = new Proxy(target, { ownKeys: () => [] });

    expect(() => Reflect.ownKeys(proxy)).toThrow(TypeError);
  });

  test("accepts extra keys when the target is extensible", () => {
    const target = {};
    Object.defineProperty(target, "fixed", { value: 1, configurable: false });
    const proxy = new Proxy(target, { ownKeys: () => ["extra", "fixed"] });

    expect(Reflect.ownKeys(proxy)).toEqual(["extra", "fixed"]);
  });

  test("rejects a result missing a configurable key of a non-extensible target", () => {
    const symbol = Symbol("s");
    const target = Object.preventExtensions({ a: 1, [symbol]: 2 });

    expect(() => Reflect.ownKeys(new Proxy(target, { ownKeys: () => ["a"] })))
      .toThrow(TypeError);
    expect(() => Reflect.ownKeys(new Proxy(target, { ownKeys: () => [symbol] })))
      .toThrow(TypeError);
    expect(Reflect.ownKeys(new Proxy(target, { ownKeys: () => [symbol, "a"] })))
      .toEqual([symbol, "a"]);
  });

  test("rejects an extra key for a non-extensible target", () => {
    const target = Object.preventExtensions({ a: 1 });
    const proxy = new Proxy(target, { ownKeys: () => ["a", Symbol("extra")] });

    expect(() => Reflect.ownKeys(proxy)).toThrow(TypeError);
  });
});

// ES2026 §10.5.11 steps 10, 11 and 16: the invariant check asks the target
// IsExtensible, then reads its keys once, then reads each key's descriptor in
// that order. Expected values are from Node.js 24.
describe("Proxy ownKeys invariant check reads its target once", () => {
  const nest = (n, target, makeHandler) =>
    Array.from({ length: n }).reduce((t) => new Proxy(t, makeHandler()), target);

  test("a nest of d ownKeys traps runs 2^d - 1 of them", () => {
    let calls = 0;
    const counts = [];
    for (const depth of [1, 2, 3, 4, 5, 6, 8, 10]) {
      calls = 0;
      Reflect.ownKeys(nest(depth, { x: 1 }, () => ({
        ownKeys: (t) => {
          calls++;
          return Reflect.ownKeys(t);
        },
      })));
      counts.push(depth + ":" + calls);
    }

    expect(counts.join(" ")).toBe("1:1 2:3 3:7 4:15 5:31 6:63 8:255 10:1023");
  });

  test("asks IsExtensible before reading the target's keys once", () => {
    const log = [];
    const symbol = Symbol("s");
    const inner = new Proxy({ a: 1, [symbol]: 2 }, {
      ownKeys(t) {
        log.push("ownKeys");
        return Reflect.ownKeys(t);
      },
      isExtensible(t) {
        log.push("isExtensible");
        return Reflect.isExtensible(t);
      },
      getOwnPropertyDescriptor(t, k) {
        log.push("gOPD:" + String(k));
        return Reflect.getOwnPropertyDescriptor(t, k);
      },
    });
    const outer = new Proxy(inner, {
      ownKeys(t) {
        log.push("outer.ownKeys");
        return Reflect.ownKeys(t);
      },
    });

    Reflect.ownKeys(outer);
    expect(log.join(" ")).toBe(
      "outer.ownKeys ownKeys isExtensible ownKeys gOPD:a gOPD:Symbol(s)");
  });

  test("reads descriptors in the target's key order", () => {
    const log = [];
    const symbol = Symbol("s");
    const inner = new Proxy({ a: 1, [symbol]: 2 }, {
      ownKeys: () => [symbol, "a"],
      getOwnPropertyDescriptor(t, k) {
        log.push(String(k));
        return Reflect.getOwnPropertyDescriptor(t, k);
      },
    });
    const outer = new Proxy(inner, { ownKeys: () => ["a", symbol] });

    expect(Reflect.ownKeys(outer)).toEqual(["a", symbol]);
    expect(log.join(" ")).toBe("Symbol(s) a");
  });

  test("a throwing isExtensible stops the check before the target's keys are read", () => {
    const log = [];
    const inner = new Proxy({}, {
      ownKeys(t) {
        log.push("ownKeys");
        return Reflect.ownKeys(t);
      },
      isExtensible() {
        log.push("isExtensible");
        throw new SyntaxError("stop");
      },
    });
    const outer = new Proxy(inner, { ownKeys: () => [] });

    expect(() => Reflect.ownKeys(outer)).toThrow(SyntaxError);
    expect(log.join(" ")).toBe("isExtensible");
  });
});

// The invariant check holds the trap's keys and the target's keys in native
// state while the target's traps run; a collection there must not free them.
describe.runIf(typeof Goccia !== "undefined" && typeof Goccia.gc === "function")(
  "Proxy ownKeys invariant check under collection", () => {
    const churn = () => {
      Goccia.gc();
      let total = 0;
      for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
        const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
        total += scratch.a + scratch.b[0];
      }
      return total;
    };
    const freshKeys = (...keys) => keys.map((k) => k.split("").join(""));

    test("keeps the trap's keys and the target's keys alive", () => {
      const symbol = Symbol("s");
      const target = Object.preventExtensions({ alpha: 1, beta: 2, [symbol]: 3 });
      const inner = new Proxy(target, {
        ownKeys: (t) => Reflect.ownKeys(t),
        getOwnPropertyDescriptor(t, k) {
          churn();
          return Reflect.getOwnPropertyDescriptor(t, k);
        },
      });

      const outer = new Proxy(inner, {
        ownKeys: () => freshKeys("alpha", "beta").concat([symbol]),
      });
      expect(Reflect.ownKeys(outer)).toEqual(["alpha", "beta", symbol]);

      const missing = new Proxy(inner, {
        ownKeys: () => freshKeys("alpha").concat([symbol]),
      });
      expect(() => Reflect.ownKeys(missing)).toThrow(TypeError);
    });
  });
