/*---
description: Repeated property reads through one call site reflect redefinition, deletion, and receiver changes
features: [Object.defineProperty, property-descriptors]
---*/

test("repeated reads through one site observe value reassignment", () => {
  const obj = { x: 1 };
  const readX = (o) => o.x;

  expect(readX(obj)).toBe(1);
  expect(readX(obj)).toBe(1);
  obj.x = 2;
  expect(readX(obj)).toBe(2);
});

test("repeated reads through one site observe data-to-accessor redefinition", () => {
  const obj = { x: 1 };
  const readX = (o) => o.x;
  let getterCalls = 0;

  expect(readX(obj)).toBe(1);
  expect(readX(obj)).toBe(1);
  Object.defineProperty(obj, "x", {
    get() {
      getterCalls += 1;
      return 42;
    },
    configurable: true,
  });
  expect(readX(obj)).toBe(42);
  expect(readX(obj)).toBe(42);
  expect(getterCalls).toBe(2);
});

test("repeated reads through one site observe deletion and re-addition", () => {
  const obj = { x: "first" };
  const readX = (o) => o.x;

  expect(readX(obj)).toBe("first");
  delete obj.x;
  expect(readX(obj)).toBeUndefined();
  obj.x = "second";
  expect(readX(obj)).toBe("second");
});

test("one site reading many receivers returns each receiver's own value", () => {
  const readX = (o) => o.x;
  const objects = Array.from({ length: 64 }, (_, i) => ({ x: i }));
  objects.forEach((o, i) => {
    expect(readX(o)).toBe(i);
  });
});

test("one site reading many receivers still observes a later accessor receiver", () => {
  const readX = (o) => o.x;
  Array.from({ length: 64 }, (_, i) => ({ x: i })).forEach((o, i) => {
    expect(readX(o)).toBe(i);
  });
  const withGetter = {};
  Object.defineProperty(withGetter, "x", {
    get() {
      return "from getter";
    },
  });
  expect(readX(withGetter)).toBe("from getter");
});

test("one site reading class instances observes per-instance field values", () => {
  class Point {
    x;
    constructor(x) {
      this.x = x;
    }
  }
  const readX = (o) => o.x;
  const a = new Point(1);
  const b = new Point(2);

  expect(readX(a)).toBe(1);
  expect(readX(b)).toBe(2);
  a.x = 10;
  expect(readX(a)).toBe(10);
  expect(readX(b)).toBe(2);
});

test("own data read through one site does not mask a later prototype getter", () => {
  const proto = {};
  const readX = (o) => o.x;
  const own = Object.create(proto);
  own.x = "own";

  expect(readX(own)).toBe("own");
  delete own.x;
  Object.defineProperty(proto, "x", {
    get() {
      return "proto getter";
    },
  });
  expect(readX(own)).toBe("proto getter");
});
