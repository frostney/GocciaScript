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
  let value = 42;
  Object.defineProperty(obj, "foo", {
    get: () => value,
    set: (newValue) => {
      value = newValue;
    },
  });
  expect(obj.foo).toBe(42);
  obj.foo = 43;
  expect(obj.foo).toBe(43);
});

test("Object.defineProperty with getter and setter leveraging object scope", () => {
  const obj = { _value: 42 };
  Object.defineProperty(obj, "foo", {
    get: () => this._value,
    set: (newValue) => {
      this._value = newValue;
    },
  });
  expect(obj.foo).toBe(42);
  obj.foo = 43;
  expect(obj.foo).toBe(43);
  expect(obj._value).toBe(43);
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
  let value = 42;
  Object.defineProperty(obj, "foo", {
    set: (newValue) => {
      value = newValue;
    },
  });
  obj.foo = 43;
  expect(obj.foo).toBeUndefined(); // Property with only setter returns undefined when accessed
  expect(value).toBe(43);
});
