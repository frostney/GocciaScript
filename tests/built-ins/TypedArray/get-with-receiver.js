/*---
description: >
  A typed array's [[Get]] answers a canonical numeric string from its own
  elements whatever the receiver, so Reflect.get, a Proxy without a get trap
  and an exotic object inheriting from a typed array all read the element
features: [TypedArray, Reflect, Proxy]
---*/

describe("TypedArray [[Get]] with a receiver", () => {
  test("Reflect.get reads an element with the typed array or any other receiver", () => {
    const ta = new Uint8Array([7, 8]);

    expect(Reflect.get(ta, "0")).toBe(7);
    expect(Reflect.get(ta, 1)).toBe(8);
    expect(Reflect.get(ta, "0", {})).toBe(7);
    expect(Reflect.get(ta, "1", new Uint8Array([1, 2]))).toBe(8);
    expect(Reflect.get(new Float64Array([1.5]), "0")).toBe(1.5);
    expect(Reflect.get(new BigInt64Array([5n]), "0", {})).toBe(5n);
  });

  test("Reflect.get answers a canonical numeric string that is not a valid index with undefined", () => {
    const prototype = { "2": "inherited", "-1": "inherited", "1.5": "inherited", "-0": "inherited" };
    const ta = Object.setPrototypeOf(new Uint8Array([7, 8]), prototype);
    const receiver = { "2": "receiver" };

    for (const key of ["2", "-1", "1.5", "-0"]) {
      expect(Reflect.get(ta, key)).toBeUndefined();
      expect(Reflect.get(ta, key, receiver)).toBeUndefined();
    }
  });

  test("Reflect.get calls an inherited getter with the receiver it is given", () => {
    const prototype = Object.create(Uint8Array.prototype, {
      self: { get() { return this; } },
    });
    const ta = Object.setPrototypeOf(new Uint8Array([7, 8, 9]), prototype);
    const other = new Uint8Array(5);
    const receiver = {};

    expect(Reflect.get(ta, "self", receiver)).toBe(receiver);
    expect(Reflect.get(ta, "length")).toBe(3);
    expect(Reflect.get(ta, "length", other)).toBe(5);
    expect(Reflect.get(ta, "byteLength", new Float64Array(2))).toBe(16);
    expect(() => Reflect.get(ta, "length", receiver)).toThrow(TypeError);
  });

  test("a Proxy without a get trap forwards an element read to the typed array", () => {
    const ta = new Uint8Array([7, 8]);
    const proxy = new Proxy(ta, {});

    expect(proxy[0]).toBe(7);
    expect(proxy["1"]).toBe(8);
    expect(proxy[2]).toBeUndefined();
    // The built-in length getter receives the Proxy, which has no [[TypedArrayName]].
    expect(() => proxy.length).toThrow(TypeError);
    expect(new Proxy(new Float32Array([0.5]), {})[0]).toBe(0.5);
  });

  test("a get trap that forwards with Reflect.get reads the element", () => {
    const proxy = new Proxy(new Int16Array([-3]), {
      get: (target, key, receiver) => Reflect.get(target, key, receiver),
    });

    expect(proxy[0]).toBe(-3);
  });

  test("an array, a class instance and a Map whose prototype is a typed array read its elements", () => {
    class Plain {}
    const ta = new Uint8Array([7, 8]);
    const inheritors = [
      Object.setPrototypeOf([], ta),
      Object.setPrototypeOf(new Plain(), ta),
      Object.setPrototypeOf(new Map(), ta),
      Object.create(ta),
    ];

    for (const inheritor of inheritors) {
      expect(inheritor[0]).toBe(7);
      expect(inheritor["1"]).toBe(8);
    }
  });

  test("an inherited read of a canonical numeric string that is not a valid index stops at the typed array", () => {
    const ta = Object.setPrototypeOf(new Uint8Array([7]), { "-1": "inherited", "5": "inherited" });
    const inheritors = [Object.setPrototypeOf([], ta), Object.create(ta), new Proxy(ta, {})];

    for (const inheritor of inheritors) {
      expect(inheritor["-1"]).toBeUndefined();
      expect(inheritor[5]).toBeUndefined();
    }
  });

  test("an element read through a Proxy in the prototype chain reaches the typed array", () => {
    const ta = new Uint8Array([7]);
    const child = Object.setPrototypeOf([], new Proxy(ta, {}));

    expect(child[0]).toBe(7);
  });
});
