/*---
description: Object.defineProperties
features: [Object.defineProperties]
---*/

test("Object.defineProperties with single property definition", () => {
  const obj = {};
  Object.defineProperties(obj, {
    foo: { value: 42, writable: true, enumerable: true, configurable: true },
  });
  expect(obj.foo).toBe(42);

  const descriptors = Object.getOwnPropertyDescriptor(obj, "foo");
  expect(descriptors.value).toBe(42);
  expect(descriptors.writable).toBe(true);
  expect(descriptors.enumerable).toBe(true);
  expect(descriptors.configurable).toBe(true);
});

test("Object.defineProperties with multiple property definitions", () => {
  const obj = {};

  Object.defineProperties(obj, {
    prop1: {
      value: "first",
      writable: true,
      enumerable: true,
      configurable: true,
    },
    prop2: {
      value: "second",
      writable: false,
      enumerable: true,
      configurable: false,
    },
    prop3: {
      get() {
        return "computed";
      },
      enumerable: false,
      configurable: true,
    },
    prop4: {
      get() {
        return "computed";
      },
      set(value) {
        this.prop4 = value;
      },
      enumerable: true,
      configurable: true,
    },
  });

  expect(obj.prop1).toBe("first");
  expect(obj.prop2).toBe("second");
  expect(obj.prop3).toBe("computed");
  expect(obj.prop4).toBe("computed");

  const descriptors = {
    prop1: Object.getOwnPropertyDescriptor(obj, "prop1"),
    prop2: Object.getOwnPropertyDescriptor(obj, "prop2"),
    prop3: Object.getOwnPropertyDescriptor(obj, "prop3"),
    prop4: Object.getOwnPropertyDescriptor(obj, "prop4"),
  };

  expect(descriptors.prop1.writable).toBe(true);
  expect(descriptors.prop2.writable).toBe(false);
  expect(descriptors.prop3.get).toBeInstanceOf(Function);
  expect(descriptors.prop4.get).toBeInstanceOf(Function);
  expect(descriptors.prop4.set).toBeInstanceOf(Function);

  expect(descriptors.prop1.enumerable).toBe(true);
  expect(descriptors.prop2.enumerable).toBe(true);
  expect(descriptors.prop3.enumerable).toBe(false);
  expect(descriptors.prop4.enumerable).toBe(true);

  expect(descriptors.prop1.configurable).toBe(true);
  expect(descriptors.prop2.configurable).toBe(false);
  expect(descriptors.prop3.configurable).toBe(true);
  expect(descriptors.prop4.configurable).toBe(true);
});

test("Object.defineProperties with empty object", () => {
  const obj = {};
  Object.defineProperties(obj, {});
  expect(obj).toEqual({});
});
