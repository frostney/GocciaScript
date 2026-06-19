/*---
description: Destructuring with defaults
features: [destructuring]
---*/

test("array destructuring with defaults not triggered", () => {
  let [a = 1, b = 2, c = 3] = [4, 5, 6];
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
});

test("array destructuring with defaults triggered", () => {
  let [a = 1, b = 2, c = 3] = [];
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("array destructuring with null and undefined", () => {
  let [a = 1, b = 2] = [null, undefined];
  expect(a).toBe(null);
  expect(b).toBe(2);
});

test("object destructuring with defaults not triggered", () => {
  let { a = 1, b = 2, c = 3 } = { a: 4, b: 5 };
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(3);
});

test("object destructuring with defaults triggered", () => {
  let { a = 1, b = 2, c = 3 } = {};
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("object destructuring numeric keys use canonical property names", () => {
  let { 0x10: hex, 1e2: exp, 1.0: one } = {
    16: "hex",
    100: "exp",
    1: "one"
  };

  expect(hex).toBe("hex");
  expect(exp).toBe("exp");
  expect(one).toBe("one");
});

test("array destructuring with defaults and rest", () => {
  let [a = 1, b = 2, c = 3, ...rest] = [4, 5, 6, 7, 8];
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
  expect(rest).toEqual([7, 8]);
});

test("object destructuring with defaults and rest", () => {
  let { a = 1, b = 2, c = 3, ...rest } = { a: 4, b: 5, c: 6, d: 7, e: 8 };
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
  expect(rest).toEqual({ d: 7, e: 8 });
});

test("destructuring defaults infer names for anonymous arrows and classes", () => {
  let { missing = () => {}, cls = class {} } = {};
  let [arrow = () => {}, otherCls = class {}] = [];

  expect(missing.name).toBe("missing");
  expect(cls.name).toBe("cls");
  expect(arrow.name).toBe("arrow");
  expect(otherCls.name).toBe("otherCls");
});

test("assignment destructuring preserves duplicate source keys for default names", () => {
  let first;
  let second;

  ({ x: first = () => {}, x: second = class {} } = {});

  expect(first.name).toBe("first");
  expect(second.name).toBe("second");
});

test("for-of assignment destructuring defaults infer names", () => {
  let objectArrow;
  let objectClass;
  let arrayArrow;
  let arrayClass;

  for ({ missing: objectArrow = () => {}, cls: objectClass = class {} } of [{}]) {}
  for ([arrayArrow = () => {}, arrayClass = class {}] of [[]]) {}

  expect(objectArrow.name).toBe("objectArrow");
  expect(objectClass.name).toBe("objectClass");
  expect(arrayArrow.name).toBe("arrayArrow");
  expect(arrayClass.name).toBe("arrayClass");
});
