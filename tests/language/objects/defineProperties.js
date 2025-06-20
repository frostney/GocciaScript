test("multiple property definitions", () => {
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
  });

  expect(obj.prop1).toBe("first");
  expect(obj.prop2).toBe("second");
  expect(obj.prop3).toBe("computed");

  const descriptors = {
    prop1: Object.getOwnPropertyDescriptor(obj, "prop1"),
    prop2: Object.getOwnPropertyDescriptor(obj, "prop2"),
    prop3: Object.getOwnPropertyDescriptor(obj, "prop3"),
  };

  expect(descriptors.prop1.writable).toBe(true);
  expect(descriptors.prop2.writable).toBe(false);
  expect(descriptors.prop3.get).toBeInstanceOf(Function);

  expect(descriptors.prop1.enumerable).toBe(true);
  expect(descriptors.prop2.enumerable).toBe(true);
  expect(descriptors.prop3.enumerable).toBe(false);
});
