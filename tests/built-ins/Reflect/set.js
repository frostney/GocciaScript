/*---
description: Reflect.set
features: [Reflect]
---*/

describe("Reflect.set", () => {
  test("sets a property and returns true", () => {
    const obj = {};
    const result = Reflect.set(obj, "x", 42);
    expect(result).toBe(true);
    expect(obj.x).toBe(42);
  });

  test("updates an existing property", () => {
    const obj = { x: 1 };
    Reflect.set(obj, "x", 99);
    expect(obj.x).toBe(99);
  });

  test("works with named properties on arrays", () => {
    const arr = [1, 2, 3];
    Reflect.set(arr, "custom", "value");
    expect(arr.custom).toBe("value");
  });

  test("works with symbol keys", () => {
    const sym = Symbol("key");
    const obj = {};
    Reflect.set(obj, sym, "symbol-value");
    expect(obj[sym]).toBe("symbol-value");
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.set(42, "x", 1)).toThrow(TypeError);
    expect(() => Reflect.set("str", "x", 1)).toThrow(TypeError);
    expect(() => Reflect.set(null, "x", 1)).toThrow(TypeError);
  });

  test("receiver is passed as this to setter", () => {
    let setterThis = null;
    const target = {};
    Object.defineProperty(target, "value", {
      set(v) { setterThis = this; },
      configurable: true,
    });
    const receiver = { id: "receiver-obj" };
    Reflect.set(target, "value", 42, receiver);
    expect(setterThis).toBe(receiver);
  });

  test("setter receives value with correct receiver this", () => {
    let captured = {};
    const target = {};
    Object.defineProperty(target, "x", {
      set(v) {
        captured.thisId = this.id;
        captured.value = v;
      },
      configurable: true,
    });
    const receiver = { id: "recv" };
    Reflect.set(target, "x", 99, receiver);
    expect(captured.thisId).toBe("recv");
    expect(captured.value).toBe(99);
  });

  test("data property write lands on receiver, not target", () => {
    const target = { x: 1 };
    const receiver = {};
    const result = Reflect.set(target, "x", 42, receiver);
    expect(result).toBe(true);
    // Value should be written to receiver, not target
    expect(receiver.x).toBe(42);
    // Target should be unchanged
    expect(target.x).toBe(1);
  });

  test("inherited setter invoked with receiver as this", () => {
    let setterThis = null;
    const proto = {};
    Object.defineProperty(proto, "prop", {
      set(v) { setterThis = this; },
      configurable: true,
    });
    const target = Object.create(proto);
    const receiver = { id: "the-receiver" };
    Reflect.set(target, "prop", "val", receiver);
    expect(setterThis).toBe(receiver);
  });

  test("returns false for non-writable data property", () => {
    const target = {};
    Object.defineProperty(target, "x", {
      value: 10,
      writable: false,
      configurable: true,
    });
    const result = Reflect.set(target, "x", 99);
    expect(result).toBe(false);
  });

  test("returns false when receiver has accessor for data property key", () => {
    const target = { x: 1 };
    const receiver = {};
    Object.defineProperty(receiver, "x", {
      get() { return 0; },
      configurable: true,
    });
    // Target has data property, but receiver has an accessor for same key
    const result = Reflect.set(target, "x", 42, receiver);
    expect(result).toBe(false);
  });

  test("returns false when receiver has non-writable own property", () => {
    const target = { x: 1 };
    const receiver = {};
    Object.defineProperty(receiver, "x", {
      value: 0,
      writable: false,
      configurable: true,
    });
    const result = Reflect.set(target, "x", 42, receiver);
    expect(result).toBe(false);
  });

  test("creates new property on receiver when absent", () => {
    const proto = { x: 10 };
    const target = Object.create(proto);
    const receiver = {};
    const result = Reflect.set(target, "x", 42, receiver);
    expect(result).toBe(true);
    expect(receiver.x).toBe(42);
    // Proto should be unchanged
    expect(proto.x).toBe(10);
  });

  test("receiver defaults to target when omitted", () => {
    const obj = {};
    Reflect.set(obj, "x", 42);
    expect(obj.x).toBe(42);
  });

  test("receiver with symbol-keyed setter", () => {
    let setterThis = null;
    const sym = Symbol("val");
    const target = {};
    Object.defineProperty(target, sym, {
      set(v) { setterThis = this; },
      configurable: true,
    });
    const receiver = { tag: "sym-receiver" };
    Reflect.set(target, sym, 99, receiver);
    expect(setterThis).toBe(receiver);
  });

  test("returns false for non-extensible receiver when creating property", () => {
    const target = {};
    const receiver = {};
    Object.preventExtensions(receiver);
    const result = Reflect.set(target, "newProp", 42, receiver);
    expect(result).toBe(false);
  });
});
