/*---
description: Delete operator removes properties from objects
features: [delete]
---*/

test("delete removes own property", () => {
  const obj = { a: 1, b: 2, c: 3 };
  delete obj.b;
  expect(obj.b).toBeUndefined();
  expect(Object.keys(obj)).toEqual(["a", "c"]);
});

test("delete with bracket notation", () => {
  const obj = { x: 10, y: 20 };
  delete obj["x"];
  expect(obj.x).toBeUndefined();
  expect(Object.keys(obj)).toEqual(["y"]);
});

test("delete non-existent property does not throw", () => {
  const obj = { a: 1 };
  delete obj.nonexistent;
  expect(Object.keys(obj)).toEqual(["a"]);
});

test("delete with computed property name", () => {
  const key = "dynamic";
  const obj = { dynamic: 42, other: 99 };
  delete obj[key];
  expect(obj.dynamic).toBeUndefined();
  expect(obj.other).toBe(99);
});

test("delete array element creates sparse array", () => {
  const arr = [1, 2, 3, 4, 5];
  delete arr[2];
  expect(arr.length).toBe(5);
  expect(arr[2]).toBeUndefined();
  expect(arr[0]).toBe(1);
  expect(arr[4]).toBe(5);
});

test("property is gone after delete", () => {
  const obj = { name: "test", value: 42 };
  expect("value" in obj).toBe(true);
  delete obj.value;
  expect("value" in obj).toBe(false);
});

test("delete with symbol key removes symbol-keyed property (ES2026 §7.1.19)", () => {
  const sym = Symbol("foo");
  const obj = { [sym]: "x", regular: 1 };
  expect(obj[sym]).toBe("x");
  delete obj[sym];
  expect(obj[sym]).toBeUndefined();
  expect(obj.regular).toBe(1);
});

test("delete with symbol key on array removes symbol-keyed property", () => {
  const sym = Symbol("tag");
  const arr = [1, 2, 3];
  arr[sym] = "marker";
  expect(arr[sym]).toBe("marker");
  delete arr[sym];
  expect(arr[sym]).toBeUndefined();
  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
});

test("delete null[string] throws TypeError", () => {
  expect(() => { delete null["x"]; }).toThrow(TypeError);
});

test("delete undefined[string] throws TypeError", () => {
  expect(() => { delete undefined["x"]; }).toThrow(TypeError);
});

test("delete null[symbol] throws TypeError", () => {
  const sym = Symbol();
  expect(() => { delete null[sym]; }).toThrow(TypeError);
});

test("delete undefined[symbol] throws TypeError", () => {
  const sym = Symbol();
  expect(() => { delete undefined[sym]; }).toThrow(TypeError);
});

test("delete null.prop throws TypeError", () => {
  expect(() => { delete null.x; }).toThrow(TypeError);
});

test("delete undefined.prop throws TypeError", () => {
  expect(() => { delete undefined.x; }).toThrow(TypeError);
});

test("delete null[key] does not invoke key toString before throwing", () => {
  let called = false;
  const key = { toString() { called = true; return "x"; } };
  expect(() => { delete null[key]; }).toThrow(TypeError);
  expect(called).toBe(false);
});

test("delete null?.prop returns true (optional chaining short-circuit)", () => {
  expect(delete null?.x).toBe(true);
  expect(delete undefined?.x).toBe(true);
});

test("delete null?.[computed] returns true without evaluating key", () => {
  let called = false;
  const key = { toString() { called = true; return "x"; } };
  expect(delete null?.[key]).toBe(true);
  expect(called).toBe(false);
});

test("delete non-configurable property with computed key throws TypeError", () => {
  const obj = Object.freeze({ x: 1 });
  const key = "x";
  expect(() => { delete obj[key]; }).toThrow(TypeError);
});

test("delete non-configurable property with dynamic key throws TypeError", () => {
  const obj = {};
  Object.defineProperty(obj, "fixed", { value: 1, configurable: false });
  const key = "fixed";
  expect(() => { delete obj[key]; }).toThrow(TypeError);
});

test("delete configurable property with computed key succeeds", () => {
  const obj = { removable: 42 };
  const key = "removable";
  expect(delete obj[key]).toBe(true);
  expect(obj.removable).toBeUndefined();
});

test("delete call expression evaluates operand and returns true", () => {
  let called = false;
  const fn = () => { called = true; };

  expect(delete fn()).toBe(true);
  expect(called).toBe(true);
});

test("delete array length fails because length is non-configurable", () => {
  const arr = [1, 2, 3];

  expect(() => { delete arr.length; }).toThrow(TypeError);
  expect(arr.length).toBe(3);
});
