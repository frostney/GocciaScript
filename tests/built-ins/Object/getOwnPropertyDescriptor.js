test("Object.getOwnPropertyDescriptor with value", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    value: 42,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(Object.getOwnPropertyDescriptor(obj, "foo")).toEqual({
    value: 42,
    writable: true,
    enumerable: true,
    configurable: true,
  });
});

test("Object.getOwnPropertyDescriptor with getter and setter", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    get: () => 42,
    set: (value) => {
      obj.foo = value;
    },
  });
  expect(Object.getOwnPropertyDescriptor(obj, "foo")).toEqual({
    get: () => 42,
    set: (value) => {
      obj.foo = value;
    },
    enumerable: true,
    configurable: true,
  });
});

test("Object.getOwnPropertyDescriptor with getter only", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    get: () => 42,
  });
  expect(Object.getOwnPropertyDescriptor(obj, "foo")).toEqual({
    get: () => 42,
    enumerable: true,
    configurable: true,
  });
});

test("Object.getOwnPropertyDescriptor with setter only", () => {
  const obj = {};
  Object.defineProperty(obj, "foo", {
    set: (value) => {
      obj.foo = value;
    },
  });
  expect(Object.getOwnPropertyDescriptor(obj, "foo")).toEqual({
    set: (value) => {
      obj.foo = value;
    },
    enumerable: true,
    configurable: true,
  });
});
