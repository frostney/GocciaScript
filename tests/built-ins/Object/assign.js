/*---
features: [Object.assign]
---*/

test("Object.assign creates a shallow copy of the object", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const obj2 = Object.assign({}, obj);
  expect(obj2).toEqual({ a: 1, b: 2, c: 3 });
  expect(obj2).not.toBe(obj);
  expect(obj2.a).toBe(obj.a);
  expect(obj2.b).toBe(obj.b);
  expect(obj2.c).toBe(obj.c);
});

test("Object.assign merges two objects shallowly", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const obj2 = { d: 4, e: 5, f: 6 };
  const obj3 = Object.assign({}, obj, obj2);
  expect(obj3).toEqual({ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 });
  expect(obj3).not.toBe(obj);
  expect(obj3).not.toBe(obj2);
});

test("Object.assign merges multiple objects shallowly", () => {
  const obj = { a: 1 };
  const obj2 = { b: 2 };
  const obj3 = { c: 3 };
  const obj4 = { d: 4 };
  const obj5 = Object.assign({}, obj, obj2, obj3, obj4);
  expect(obj5).toEqual({ a: 1, b: 2, c: 3, d: 4 });
  expect(obj5).not.toBe(obj);
  expect(obj5).not.toBe(obj2);
  expect(obj5).not.toBe(obj3);
  expect(obj5).not.toBe(obj4);
});

test("Object.assign overwrites properties", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const obj2 = { b: 20, d: 4 };
  const obj3 = Object.assign({}, obj, obj2);
  expect(obj3).toEqual({ a: 1, b: 20, c: 3, d: 4 });
  expect(obj3).not.toBe(obj);
  expect(obj3).not.toBe(obj2);
});

test("Object.assign mutates the first argument", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(obj.a).toBe(1);
  expect(obj.b).toBe(2);
  expect(obj.c).toBe(3);
  const obj2 = { b: 20, d: 4 };
  const obj3 = Object.assign(obj, obj2);
  expect(obj3).toEqual({ a: 1, b: 20, c: 3, d: 4 });
  expect(obj3).toBe(obj);
  expect(obj3).not.toBe(obj2);
  expect(obj.a).toBe(1);
  expect(obj.b).toBe(20);
  expect(obj.c).toBe(3);
  expect(obj.d).toBe(4);
});
