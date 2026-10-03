/*---
description: A Proxy reached through the prototype chain answers [[Get]], [[Set]] and [[HasProperty]] with its own traps
features: [Proxy, Reflect, prototype-chain]
---*/

// ES2026 §10.1.8.1 OrdinaryGet, §10.1.9.2 OrdinarySetWithOwnDescriptor and
// §10.1.7.1 OrdinaryHasProperty call the parent's own internal method when the
// receiver has no own property; for a Proxy parent that is §10.5.8 [[Get]],
// §10.5.9 [[Set]] and §10.5.7 [[HasProperty]].

const receivers = [
  ["Object.create", (proto) => Object.create(proto)],
  ["object literal", (proto) => ({ __proto__: proto })],
  ["class instance", (proto) => {
    class Plain {}
    return Object.setPrototypeOf(new Plain(), proto);
  }],
  ["array", (proto) => Object.setPrototypeOf([], proto)],
  ["arrow function", (proto) => Object.setPrototypeOf(() => 0, proto)],
  ["two levels down", (proto) => Object.create(Object.create(proto))],
];

describe.each(receivers)("Proxy in the prototype chain of %s", (_, inherit) => {
  test("a read calls the get trap with the receiver", () => {
    let seen;
    const proxy = new Proxy({ x: "target" }, {
      get(target, key, receiver) {
        seen = receiver;
        return "trap:" + String(key);
      },
    });
    const child = inherit(proxy);
    expect(child.x).toBe("trap:x");
    // The receiver has no toString of its own to print, so compare identity.
    expect(seen === child).toBe(true);
    expect(child["y"]).toBe("trap:y");
    expect(Reflect.get(child, "z")).toBe("trap:z");
  });

  test("a read does not consult the getOwnPropertyDescriptor trap", () => {
    const log = [];
    const proxy = new Proxy({ x: 1 }, {
      getOwnPropertyDescriptor(target, key) {
        log.push(String(key));
        return Reflect.getOwnPropertyDescriptor(target, key);
      },
    });
    const child = inherit(proxy);
    expect(child.x).toBe(1);
    expect(log).toEqual([]);
  });

  test("a symbol-keyed read calls the get trap", () => {
    const key = Symbol("key");
    const proxy = new Proxy({}, { get: (target, k) => k === key ? "symbol trap" : undefined });
    expect(inherit(proxy)[key]).toBe("symbol trap");
  });

  test("an assignment calls the set trap with the receiver and creates no own property", () => {
    const log = [];
    let child;
    const proxy = new Proxy({}, {
      set(target, key, value, receiver) {
        log.push([String(key), value, receiver === child]);
        return true;
      },
    });
    child = inherit(proxy);
    child.x = 1;
    child["y"] = 2;
    expect(log).toEqual([["x", 1, true], ["y", 2, true]]);
    expect(Object.hasOwn(child, "x")).toBe(false);
    expect(Object.hasOwn(child, "y")).toBe(false);
  });

  test("a symbol-keyed assignment calls the set trap", () => {
    const key = Symbol("key");
    const log = [];
    const child = inherit(new Proxy({}, {
      set(target, k, value) {
        log.push(value);
        return true;
      },
    }));
    child[key] = 3;
    expect(log).toEqual([3]);
    expect(Object.hasOwn(child, key)).toBe(false);
  });

  test("a set trap returning false makes the assignment throw", () => {
    const child = inherit(new Proxy({}, { set: () => false }));
    expect(() => { child.x = 1; }).toThrow(TypeError);
    expect(Reflect.set(child, "x", 1)).toBe(false);
    expect(Object.hasOwn(child, "x")).toBe(false);
  });

  test("a getter-only accessor behind a Proxy makes the assignment throw", () => {
    const target = Object.create({ get x() { return "from getter"; } });
    const child = inherit(new Proxy(target, {}));
    expect(() => { child.x = 1; }).toThrow(TypeError);
    expect(Object.hasOwn(child, "x")).toBe(false);
  });

  test("a read-only property behind a Proxy makes the assignment throw", () => {
    const target = Object.create(Object.defineProperty({}, "x", { value: 1, writable: false }));
    const child = inherit(new Proxy(target, {}));
    expect(() => { child.x = 2; }).toThrow(TypeError);
    expect(Object.hasOwn(child, "x")).toBe(false);
  });

  test("a setter behind a Proxy runs with the receiver as this", () => {
    let seen;
    const target = Object.create({ set x(value) { seen = [this, value]; } });
    const child = inherit(new Proxy(target, {}));
    child.x = 5;
    expect(seen[0]).toBe(child);
    expect(seen[1]).toBe(5);
    expect(Object.hasOwn(child, "x")).toBe(false);
  });

  test("without a set trap the assignment creates an own property on the receiver", () => {
    const target = {};
    const child = inherit(new Proxy(target, {}));
    child.x = 1;
    expect(Object.hasOwn(child, "x")).toBe(true);
    expect(Object.hasOwn(target, "x")).toBe(false);
  });

  test("a compound assignment calls get and then set", () => {
    const log = [];
    const child = inherit(new Proxy({ n: 1 }, {
      get(target, key, receiver) {
        log.push("get " + String(key));
        return Reflect.get(target, key, receiver);
      },
      set(target, key, value) {
        log.push("set " + String(key) + " " + value);
        return true;
      },
    }));
    child.n += 1;
    expect(log).toEqual(["get n", "set n 2"]);
  });

  test("an in check calls the has trap", () => {
    const log = [];
    const child = inherit(new Proxy({}, {
      has(target, key) {
        log.push(String(key));
        return true;
      },
    }));
    expect("anything" in child).toBe(true);
    expect(Reflect.has(child, "other")).toBe(true);
    expect(log).toEqual(["anything", "other"]);
  });
});
