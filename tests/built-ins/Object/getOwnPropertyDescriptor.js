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

test("Object.getOwnPropertyDescriptor with number returns undefined", () => {
  expect(Object.getOwnPropertyDescriptor(42, "foo")).toBeUndefined();
});

test("Object.getOwnPropertyDescriptor with string returns character descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor("str", "0");
  expect(desc).toBeDefined();
  expect(desc.value).toBe("s");
  expect(desc.enumerable).toBe(true);
  expect(desc.writable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.getOwnPropertyDescriptor with string returns length descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor("str", "length");
  expect(desc).toBeDefined();
  expect(desc.value).toBe(3);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.getOwnPropertyDescriptor with non-canonical string index returns undefined", () => {
  // "01" and "-0" are not canonical string indices per ES2026 §10.4.3
  expect(Object.getOwnPropertyDescriptor("str", "01")).toBeUndefined();
  expect(Object.getOwnPropertyDescriptor("str", "-0")).toBeUndefined();
});

test("Object.getOwnPropertyDescriptor throws for null", () => {
  expect(() => Object.getOwnPropertyDescriptor(null, "foo")).toThrow(TypeError);
});

test("Object.getOwnPropertyDescriptor throws for undefined", () => {
  expect(() => Object.getOwnPropertyDescriptor(undefined, "foo")).toThrow(TypeError);
});
