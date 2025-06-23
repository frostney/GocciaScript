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
  let value = 42;
  const getter = () => value;
  const setter = (newValue) => {
    value = newValue;
  };
  Object.defineProperty(obj, "foo", {
    get: getter,
    set: setter,
  });
  const desc = Object.getOwnPropertyDescriptor(obj, "foo");
  expect(desc.get).toBe(getter);
  expect(desc.set).toBe(setter);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.getOwnPropertyDescriptor with getter only", () => {
  const obj = {};
  const getter = () => 42;
  Object.defineProperty(obj, "foo", {
    get: getter,
  });
  const desc = Object.getOwnPropertyDescriptor(obj, "foo");
  expect(desc.get).toBe(getter);
  expect(desc.set).toBeUndefined();
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.getOwnPropertyDescriptor with setter only", () => {
  const obj = {};
  let value = 42;
  const setter = (newValue) => {
    value = newValue;
  };
  Object.defineProperty(obj, "foo", {
    set: setter,
  });
  const desc = Object.getOwnPropertyDescriptor(obj, "foo");
  expect(desc.get).toBeUndefined();
  expect(desc.set).toBe(setter);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});
