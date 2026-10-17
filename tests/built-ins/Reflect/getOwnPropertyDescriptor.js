/*---
description: Reflect.getOwnPropertyDescriptor
features: [Reflect]
---*/

describe("Reflect.getOwnPropertyDescriptor", () => {
  test("returns descriptor for data property", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: false,
      configurable: true,
    });
    const desc = Reflect.getOwnPropertyDescriptor(obj, "x");
    expect(desc.value).toBe(42);
    expect(desc.writable).toBe(true);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("returns data descriptor keys in FromPropertyDescriptor order", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, "x"))).toEqual([
      "value",
      "writable",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns descriptor for accessor property", () => {
    const obj = {};
    const getter = () => 10;
    Object.defineProperty(obj, "y", {
      get: getter,
      enumerable: true,
      configurable: true,
    });
    const desc = Reflect.getOwnPropertyDescriptor(obj, "y");
    expect(desc.get).toBe(getter);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("returns accessor descriptor keys in FromPropertyDescriptor order", () => {
    const obj = {};
    const getter = () => 10;
    const setter = () => {};
    Object.defineProperty(obj, "y", {
      get: getter,
      set: setter,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, "y"))).toEqual([
      "get",
      "set",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns symbol data descriptor keys in FromPropertyDescriptor order", () => {
    const sym = Symbol("x");
    const obj = {};
    Object.defineProperty(obj, sym, {
      value: 1,
      writable: true,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, sym))).toEqual([
      "value",
      "writable",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns undefined for non-existent property", () => {
    const obj = { x: 1 };
    expect(Reflect.getOwnPropertyDescriptor(obj, "z")).toBe(undefined);
  });

  test("does not traverse prototype chain", () => {
    const proto = { inherited: true };
    const obj = Object.create(proto);
    expect(Reflect.getOwnPropertyDescriptor(obj, "inherited")).toBe(undefined);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.getOwnPropertyDescriptor(42, "x")).toThrow(TypeError);
    expect(() => Reflect.getOwnPropertyDescriptor("str", "x")).toThrow(TypeError);
  });
});

// ES2026 §28.1.6 step 4 and §6.2.6.4 FromPropertyDescriptor: every field is
// made with CreateDataPropertyOrThrow, so it is an own data property and no
// setter inherited from Object.prototype runs.
describe("Reflect.getOwnPropertyDescriptor with setters on Object.prototype", () => {
  const FIELDS = ["value", "writable", "get", "set", "enumerable", "configurable"];

  // Installs a setter for every descriptor field name on Object.prototype,
  // calls fn, and removes the setters again before returning fn's result.
  const withPrototypeSetters = (onSet, fn) => {
    FIELDS.forEach((name) => {
      // A null-prototype descriptor: an ordinary one would inherit the
      // accessors already installed and be rejected by ToPropertyDescriptor.
      const descriptor = Object.create(null);
      descriptor.set = (v) => { onSet(name, v); };
      descriptor.configurable = true;
      Object.defineProperty(Object.prototype, name, descriptor);
    });
    try {
      return fn();
    } finally {
      FIELDS.forEach((name) => { delete Object.prototype[name]; });
    }
  };

  const ownDataFields = (desc) =>
    Object.getOwnPropertyNames(desc).filter((name) =>
      Object.hasOwn(Object.getOwnPropertyDescriptor(desc, name), "value"));

  test("a string-keyed data property", () => {
    const calls = [];
    const obj = { x: 1 };
    const desc = withPrototypeSetters((name) => calls.push(name),
      () => Reflect.getOwnPropertyDescriptor(obj, "x"));

    expect(calls).toEqual([]);
    expect(ownDataFields(desc)).toEqual(["value", "writable", "enumerable", "configurable"]);
    expect(desc.value).toBe(1);
    expect(desc.writable).toBe(true);
  });

  test("a symbol-keyed data property", () => {
    const calls = [];
    const key = Symbol("x");
    const obj = { [key]: 2 };
    const desc = withPrototypeSetters((name) => calls.push(name),
      () => Reflect.getOwnPropertyDescriptor(obj, key));

    expect(calls).toEqual([]);
    expect(ownDataFields(desc)).toEqual(["value", "writable", "enumerable", "configurable"]);
    expect(desc.value).toBe(2);
  });

  test("a string-keyed accessor property", () => {
    const calls = [];
    const getter = () => 3;
    const obj = {};
    Object.defineProperty(obj, "x", { get: getter, enumerable: true, configurable: true });
    const desc = withPrototypeSetters((name) => calls.push(name),
      () => Reflect.getOwnPropertyDescriptor(obj, "x"));

    expect(calls).toEqual([]);
    expect(ownDataFields(desc)).toEqual(["get", "set", "enumerable", "configurable"]);
    expect(desc.get).toBe(getter);
    expect(desc.set).toBe(undefined);
  });

  test("a symbol-keyed accessor property", () => {
    const calls = [];
    const key = Symbol("x");
    const setter = (v) => {};
    const obj = {};
    Object.defineProperty(obj, key, { set: setter, configurable: true });
    const desc = withPrototypeSetters((name) => calls.push(name),
      () => Reflect.getOwnPropertyDescriptor(obj, key));

    expect(calls).toEqual([]);
    expect(ownDataFields(desc)).toEqual(["get", "set", "enumerable", "configurable"]);
    expect(desc.set).toBe(setter);
    expect(desc.enumerable).toBe(false);
  });

  test("a setter that deletes the property being described does not run", () => {
    const obj = { x: 4 };
    const desc = withPrototypeSetters(() => { delete obj.x; },
      () => Reflect.getOwnPropertyDescriptor(obj, "x"));

    expect(Object.hasOwn(obj, "x")).toBe(true);
    expect(desc.value).toBe(4);
    expect(desc.writable).toBe(true);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("matches Object.getOwnPropertyDescriptor", () => {
    const obj = { x: 5 };
    const [fromReflect, fromObject] = withPrototypeSetters(() => {}, () => [
      Reflect.getOwnPropertyDescriptor(obj, "x"),
      Object.getOwnPropertyDescriptor(obj, "x"),
    ]);

    expect(Object.getOwnPropertyNames(fromReflect)).toEqual(Object.getOwnPropertyNames(fromObject));
  });
});
