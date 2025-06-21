test("Object.defineProperty with value", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    value: 42,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(obj.foo).toBe(42);
});

test("Object.defineProperty with getter and setter", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    get: () => 42,
    set: (value) => {
      obj.foo = value;
    },
  });
  expect(obj.foo).toBe(42);
  obj.foo = 43;
  expect(obj.foo).toBe(43);
});

test("Object.defineProperty with getter only", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    get: () => 42,
  });
  expect(obj.foo).toBe(42);
});

test("Object.defineProperty with setter only", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    set: (value) => {
      obj.foo = value;
    },
  });
  obj.foo = 42;
  expect(obj.foo).toBe(42);
});
