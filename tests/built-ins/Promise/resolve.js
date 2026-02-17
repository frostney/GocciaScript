/*---
description: Promise.resolve creates resolved promises correctly
features: [Promise.resolve]
---*/

test("Promise.resolve with primitive value", () => {
  return Promise.resolve(42).then((v) => {
    expect(v).toBe(42);
  });
});

test("Promise.resolve with string", () => {
  return Promise.resolve("hello").then((v) => {
    expect(v).toBe("hello");
  });
});

test("Promise.resolve with undefined", () => {
  return Promise.resolve(undefined).then((v) => {
    expect(v).toBeUndefined();
  });
});

test("Promise.resolve with null", () => {
  return Promise.resolve(null).then((v) => {
    expect(v).toBeNull();
  });
});

test("Promise.resolve with object", () => {
  const obj = { a: 1 };
  return Promise.resolve(obj).then((v) => {
    expect(v).toBe(obj);
  });
});

test("Promise.resolve with boolean", () => {
  return Promise.resolve(true).then((v) => {
    expect(v).toBe(true);
  });
});

test("Promise.resolve with zero", () => {
  return Promise.resolve(0).then((v) => {
    expect(v).toBe(0);
  });
});

test("Promise.resolve with a Promise returns the same Promise", () => {
  const p = Promise.resolve(42);
  const p2 = Promise.resolve(p);
  expect(p2).toBe(p);
});

test("Promise.resolve with no argument resolves to undefined", () => {
  return Promise.resolve().then((v) => {
    expect(v).toBeUndefined();
  });
});

test("Promise.resolve with nested resolved promise returns inner", () => {
  const inner = Promise.resolve(42);
  const outer = Promise.resolve(inner);
  expect(outer).toBe(inner);
  return outer.then((v) => {
    expect(v).toBe(42);
  });
});

test("Promise.resolve with array", () => {
  const arr = [1, 2, 3];
  return Promise.resolve(arr).then((v) => {
    expect(v).toBe(arr);
    expect(v).toEqual([1, 2, 3]);
  });
});
