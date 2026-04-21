/*---
description: >
  Object.defineProperty non-configurable redefinition rules per ES2026 §10.1.6.3
features: [Object.defineProperty, Object.getOwnPropertyDescriptor]
---*/

// Issue 1: Non-configurable property redefinition — allowed cases

test("non-configurable writable data: value change allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 1, writable: true, configurable: false });
  Object.defineProperty(obj, "x", { value: 2 });
  expect(obj.x).toBe(2);
});

test("non-configurable writable data: writable false transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 1, writable: true, configurable: false });
  Object.defineProperty(obj, "x", { writable: false });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.writable).toBe(false);
  expect(desc.value).toBe(1);
});

test("non-configurable data: identical redefinition is no-op", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 42,
    writable: false,
    enumerable: true,
    configurable: false,
  });
  // Redefine with same descriptor — should succeed
  Object.defineProperty(obj, "x", {
    value: 42,
    writable: false,
    enumerable: true,
    configurable: false,
  });
  expect(obj.x).toBe(42);
});

test("non-configurable non-writable data: value change rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { value: 2 });
  }).toThrow(TypeError);
  expect(obj.x).toBe(1);
});

test("non-configurable: writable false to true rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { writable: true });
  }).toThrow(TypeError);
});

test("non-configurable: enumerable change rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    enumerable: true,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { enumerable: false });
  }).toThrow(TypeError);
});

test("non-configurable: configurable true rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { configurable: true });
  }).toThrow(TypeError);
});

test("non-configurable: data to accessor transition rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", {
      get: () => 2,
    });
  }).toThrow(TypeError);
});

test("non-configurable accessor: getter change rejected", () => {
  const getter1 = () => 1;
  const getter2 = () => 2;
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: getter1,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { get: getter2 });
  }).toThrow(TypeError);
});

test("non-configurable accessor: setter change rejected", () => {
  const setter1 = (v) => {};
  const setter2 = (v) => {};
  const obj = {};
  Object.defineProperty(obj, "x", {
    set: setter1,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { set: setter2 });
  }).toThrow(TypeError);
});

test("non-configurable accessor: identical redefinition is no-op", () => {
  const getter = () => 42;
  const setter = (v) => {};
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: getter,
    set: setter,
    enumerable: true,
    configurable: false,
  });
  // Same getter and setter — should succeed
  Object.defineProperty(obj, "x", {
    get: getter,
    set: setter,
    enumerable: true,
    configurable: false,
  });
  expect(obj.x).toBe(42);
});

// Issue 3: Data-to-accessor descriptor transition (configurable)

test("configurable: data to accessor transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    configurable: true,
  });
  Object.defineProperty(obj, "x", {
    get: () => 2,
    configurable: true,
  });
  expect(obj.x).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(typeof desc.get).toBe("function");
  expect(desc.value).toBeUndefined();
});

test("configurable: accessor to data transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: () => 1,
    configurable: true,
  });
  Object.defineProperty(obj, "x", {
    value: 2,
    writable: true,
    configurable: true,
  });
  expect(obj.x).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.writable).toBe(true);
  expect(desc.get).toBeUndefined();
});

// Issue 4: Descriptor flag defaults — absent fields preserve existing values

test("redefine preserves value when only enumerable changes", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: "hello",
    writable: true,
    enumerable: true,
    configurable: true,
  });
  Object.defineProperty(obj, "x", { enumerable: false });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.value).toBe("hello");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});

test("redefine preserves flags when only value changes", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  Object.defineProperty(obj, "x", { value: 2 });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.value).toBe(2);
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(true);
  expect(desc.configurable).toBe(true);
});

test("redefine on non-configurable writable preserves flags", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    enumerable: true,
    configurable: false,
  });
  Object.defineProperty(obj, "x", { value: 2 });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.value).toBe(2);
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(true);
  expect(desc.configurable).toBe(false);
});

// Reflect.defineProperty boolean variant

test("Reflect.defineProperty returns false for non-configurable violations", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(Reflect.defineProperty(obj, "x", { value: 2 })).toBe(false);
  expect(obj.x).toBe(1);
});

test("Reflect.defineProperty returns true for allowed non-configurable updates", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    configurable: false,
  });
  expect(Reflect.defineProperty(obj, "x", { value: 2 })).toBe(true);
  expect(obj.x).toBe(2);
});
