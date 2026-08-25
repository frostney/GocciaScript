/*---
description: Constant property reads from a local object use fused local+prop decode
features: [Object, property-access, Object.defineProperty]
---*/

test("own data property on a local object", () => {
  const obj = { x: 1, y: 2 };
  expect(obj.x).toBe(1);
  expect(obj.y).toBe(2);
  obj.x = 3;
  expect(obj.x).toBe(3);
});

test("missing own property on a local object is undefined", () => {
  const obj = { x: 1 };
  expect(obj.missing).toBeUndefined();
});

test("inherited data property on a local object", () => {
  const proto = { x: 7 };
  const obj = Object.create(proto);
  expect(obj.x).toBe(7);
});

test("own accessor on a local object is invoked", () => {
  let calls = 0;
  const obj = {
    get x() {
      calls += 1;
      return 42;
    },
  };
  expect(obj.x).toBe(42);
  expect(obj.x).toBe(42);
  expect(calls).toBe(2);
});

test("inherited accessor on a local object uses the local receiver", () => {
  const proto = {
    get x() {
      return this.y;
    },
  };
  const obj = Object.create(proto);
  obj.y = 9;
  expect(obj.x).toBe(9);
});

test("nullish local base throws TypeError", () => {
  const und = undefined;
  const nul = null;
  let undefinedError;
  let nullError;

  try {
    und.x;
  } catch (error) {
    undefinedError = error;
  }
  try {
    nul.x;
  } catch (error) {
    nullError = error;
  }

  expect(undefinedError instanceof TypeError).toBe(true);
  expect(undefinedError.message).toBe(
    "Cannot read properties of undefined (reading 'x')",
  );
  expect(nullError instanceof TypeError).toBe(true);
  expect(nullError.message).toBe(
    "Cannot read properties of null (reading 'x')",
  );
});

test("parameter local property reads observe redefinition", () => {
  const readX = (o) => o.x;
  const obj = { x: 1 };
  expect(readX(obj)).toBe(1);
  Object.defineProperty(obj, "x", {
    get() {
      return 99;
    },
    configurable: true,
  });
  expect(readX(obj)).toBe(99);
});
