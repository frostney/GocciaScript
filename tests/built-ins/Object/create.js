test("object create", () => {
  const obj = Object.create({ name: "John" });
  expect(obj.name).toBe("John");
});

test("Object.create has the correct function length", () => {
  expect(Object.create.length).toBe(2);
});

test("object create with null prototype", () => {
  const obj = Object.create(null);
  expect(obj.name).toBe(undefined);
});

test("object create with prototype chain", () => {
  const obj = Object.create(null);
  const obj2 = Object.create(obj);
  expect(obj2.name).toBe(undefined);
});

test("Object.create with property descriptors", () => {
  const obj = Object.create({}, {
    x: { value: 42, writable: true, enumerable: true, configurable: true }
  });
  expect(obj.x).toBe(42);
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(true);
  expect(desc.configurable).toBe(true);
});

test("Object.create property descriptor defaults to false", () => {
  const obj = Object.create({}, {
    y: { value: "hello" }
  });
  expect(obj.y).toBe("hello");
  const desc = Object.getOwnPropertyDescriptor(obj, "y");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.create with accessor descriptors", () => {
  const obj = Object.create({}, {
    z: {
      get: () => 99,
      configurable: true
    }
  });
  expect(obj.z).toBe(99);
});

test("Object.create with multiple properties", () => {
  const obj = Object.create(null, {
    a: { value: 1, enumerable: true },
    b: { value: 2, enumerable: true },
    c: { value: 3, enumerable: false }
  });
  expect(obj.a).toBe(1);
  expect(obj.b).toBe(2);
  expect(obj.c).toBe(3);
  const keys = Object.keys(obj);
  expect(keys.includes("a")).toBe(true);
  expect(keys.includes("b")).toBe(true);
  expect(keys.includes("c")).toBe(false);
});

test("Object.create ignores undefined Properties argument", () => {
  const obj = Object.create({}, undefined);
  expect(typeof obj).toBe("object");
});
