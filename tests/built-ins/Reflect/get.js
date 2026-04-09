/*---
description: Reflect.get
features: [Reflect]
---*/

describe("Reflect.get", () => {
  test("gets a property value", () => {
    const obj = { x: 42 };
    expect(Reflect.get(obj, "x")).toBe(42);
  });

  test("returns undefined for missing property", () => {
    const obj = { x: 1 };
    expect(Reflect.get(obj, "y")).toBe(undefined);
  });

  test("traverses prototype chain", () => {
    const proto = { greeting: "hello" };
    const obj = Object.create(proto);
    expect(Reflect.get(obj, "greeting")).toBe("hello");
  });

  test("works with arrays", () => {
    const arr = [10, 20, 30];
    expect(Reflect.get(arr, "length")).toBe(3);
  });

  test("works with symbol keys", () => {
    const sym = Symbol("key");
    const obj = {};
    obj[sym] = "symbol-value";
    expect(Reflect.get(obj, sym)).toBe("symbol-value");
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.get(42, "x")).toThrow(TypeError);
    expect(() => Reflect.get("str", "x")).toThrow(TypeError);
    expect(() => Reflect.get(null, "x")).toThrow(TypeError);
    expect(() => Reflect.get(undefined, "x")).toThrow(TypeError);
  });

  test("receiver defaults to target for data properties", () => {
    const obj = { x: 10 };
    // When receiver is omitted, should behave identically
    expect(Reflect.get(obj, "x")).toBe(10);
    // Explicitly passing target as receiver
    expect(Reflect.get(obj, "x", obj)).toBe(10);
  });

  test("receiver is passed as this to getter", () => {
    const target = {};
    Object.defineProperty(target, "value", {
      get() { return this.x; },
      configurable: true,
    });
    const receiver = { x: 42 };
    // Without receiver: this === target, target.x is undefined
    expect(Reflect.get(target, "value")).toBe(undefined);
    // With receiver: this === receiver, receiver.x is 42
    expect(Reflect.get(target, "value", receiver)).toBe(42);
  });

  test("receiver is threaded through prototype chain getters", () => {
    const proto = {};
    Object.defineProperty(proto, "name", {
      get() { return this.label; },
      configurable: true,
    });
    const target = Object.create(proto);
    const receiver = { label: "from-receiver" };
    expect(Reflect.get(target, "name", receiver)).toBe("from-receiver");
  });

  test("receiver with symbol-keyed getter", () => {
    const sym = Symbol("val");
    const target = {};
    Object.defineProperty(target, sym, {
      get() { return this.data; },
      configurable: true,
    });
    const receiver = { data: "symbol-receiver" };
    expect(Reflect.get(target, sym, receiver)).toBe("symbol-receiver");
  });

  test("receiver does not affect data properties", () => {
    const target = { x: 100 };
    const receiver = { x: 999 };
    // Data properties return the target's value regardless of receiver
    expect(Reflect.get(target, "x", receiver)).toBe(100);
  });
});
