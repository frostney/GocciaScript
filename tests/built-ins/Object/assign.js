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

test("Object.assign returns the target object", () => {
  const target = {};
  const result = Object.assign(target, { a: 1 });
  expect(result).toBe(target);
});

test("Object.assign with no sources returns target unchanged", () => {
  const target = { a: 1 };
  const result = Object.assign(target);
  expect(result).toBe(target);
  expect(result.a).toBe(1);
});

test("Object.assign throws for null target", () => {
  expect(() => Object.assign(null, {})).toThrow(TypeError);
});

test("Object.assign throws for undefined target", () => {
  expect(() => Object.assign(undefined, {})).toThrow(TypeError);
});

test("Object.assign with null or undefined source skips them", () => {
  const target = { a: 1 };
  const result = Object.assign(target, null, undefined, { b: 2 });
  expect(result.a).toBe(1);
  expect(result.b).toBe(2);
});

test("Object.assign has correct name and length", () => {
  expect(Object.assign.name).toBe("assign");
  expect(Object.assign.length).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(Object.assign, "name");
  expect(desc.configurable).toBe(true);
  expect(desc.enumerable).toBe(false);
});

// ES2026 §20.1.2.1 step 3.a.iii.2.b: each property is copied with
// Set(to, nextKey, propValue, true), so a target's own accessor receives the
// value through its setter, and a missing setter is a TypeError.
describe("Object.assign onto a target with an own accessor", () => {
  class Plain {}

  const targets = [
    ["a plain object", () => ({})],
    ["a class instance", () => new Plain()],
    ["an array", () => [1, 2, 3]],
    ["a Uint8Array", () => new Uint8Array(2)],
    ["a Map", () => new Map()],
    ["a String object", () => new String("ab")],
  ];

  describe.each(targets)("%s", (label, make) => {
    test("calls the setter and keeps the accessor", () => {
      const target = make();
      const calls = [];
      Object.defineProperty(target, "x", {
        set(value) {
          calls.push([this === target, value]);
        },
        configurable: true,
      });

      const result = Object.assign(target, { x: 1 });

      expect(result).toBe(target);
      expect(calls).toEqual([[true, 1]]);
      expect("set" in Object.getOwnPropertyDescriptor(target, "x")).toBe(true);
    });

    test("throws TypeError when the accessor has no setter", () => {
      const target = make();
      Object.defineProperty(target, "x", {
        get: () => "from getter",
        configurable: true,
      });

      expect(() => Object.assign(target, { x: 1 })).toThrow(TypeError);
      expect(target.x).toBe("from getter");
      expect("get" in Object.getOwnPropertyDescriptor(target, "x")).toBe(true);
    });
  });

  test("copies the properties before a getter-only accessor and stops there", () => {
    const target = new Plain();
    Object.defineProperty(target, "b", { get: () => "kept", configurable: true });

    expect(() => Object.assign(target, { a: 1, b: 2, c: 3 })).toThrow(TypeError);
    expect(target.a).toBe(1);
    expect(target.b).toBe("kept");
    expect(Object.hasOwn(target, "c")).toBe(false);
  });
});
