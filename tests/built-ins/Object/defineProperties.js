test("Object.defineProperties with single property", () => {
  const obj = {};
  let barValue = 42;
  Object.defineProperties(obj, {
    foo: { value: 42, writable: true, enumerable: true, configurable: true },
    bar: {
      get: () => barValue,
      set: (value) => {
        barValue = value;
      },
      enumerable: true,
      configurable: true,
    },
  });
  expect(obj.foo).toBe(42);
  obj.bar = 43;
  expect(obj.bar).toBe(43);
});

test("Object.defineProperties with multiple properties", () => {
  const obj = {};
  let barValue = 42;
  Object.defineProperties(obj, {
    foo: { value: 42, writable: true, enumerable: true, configurable: true },
    bar: {
      get: () => barValue,
      set: (value) => {
        barValue = value;
      },
      enumerable: true,
      configurable: true,
    },
  });
  expect(obj.foo).toBe(42);
  obj.bar = 43;
  expect(obj.bar).toBe(43);
});
