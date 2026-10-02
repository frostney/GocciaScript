/*---
description: One property read or store site gives the right result for every kind of receiver that reaches it
features: [class, Proxy, Map, Symbol]
---*/

test("a read site warmed on plain objects reads arrays, functions, strings and exotic objects", () => {
  const readLength = (o) => o.length;
  const plain = Array.from({ length: 64 }, (_, i) => ({ length: i }));

  plain.forEach((o, i) => {
    expect(readLength(o)).toBe(i);
  });
  expect(readLength([1, 2, 3])).toBe(3);
  expect(readLength((a, b) => a + b)).toBe(2);
  expect(readLength("four")).toBe(4);
  expect(readLength(new Uint8Array(5))).toBe(5);
  expect(readLength(new Proxy({}, { get: () => 6 }))).toBe(6);
  expect(readLength(Object.create({ length: 7 }))).toBe(7);
  expect(readLength(7)).toBeUndefined();
  expect(readLength(true)).toBeUndefined();
  expect(readLength(Symbol("s"))).toBeUndefined();
  expect(() => readLength(null)).toThrow(TypeError);
  expect(() => readLength(undefined)).toThrow(TypeError);
  expect(readLength(plain[5])).toBe(5);
});

test("a read site warmed on class instances reads the same name from the class, a Map and an array", () => {
  class Box {
    size;
    static size = "static";
    constructor(size) {
      this.size = size;
    }
  }
  const readSize = (o) => o.size;
  const boxes = Array.from({ length: 64 }, (_, i) => new Box(i));

  boxes.forEach((b, i) => {
    expect(readSize(b)).toBe(i);
  });
  expect(readSize(Box)).toBe("static");
  expect(readSize(new Map([[1, 2]]))).toBe(1);
  expect(readSize(new Set([1, 2, 3]))).toBe(3);
  expect(readSize([])).toBeUndefined();
  expect(readSize(boxes[8])).toBe(8);
});

test("a store site warmed on plain objects stores to arrays, functions, class values and a Proxy", () => {
  const setTag = (o, v) => {
    o.tag = v;
    return o;
  };
  const plain = Array.from({ length: 64 }, () => ({ tag: 0 }));

  plain.forEach((o, i) => {
    expect(setTag(o, i).tag).toBe(i);
  });
  expect(setTag([], "array").tag).toBe("array");
  expect(setTag(() => 1, "function").tag).toBe("function");
  class Tagged {}
  expect(setTag(Tagged, "class").tag).toBe("class");
  expect(Object.keys(Tagged)).toEqual(["tag"]);
  const seen = [];
  const proxy = new Proxy(
    {},
    {
      set(target, key, value) {
        seen.push([key, value]);
        target[key] = value;
        return true;
      },
    },
  );
  setTag(proxy, "proxy");
  expect(seen).toEqual([["tag", "proxy"]]);
  expect(setTag(plain[2], "again").tag).toBe("again");
});

test("a method assigned to a class through a warmed store site can use super", () => {
  class Base {
    static describe() {
      return "base";
    }
  }
  class Derived extends Base {}
  const install = (target, fn) => {
    target.describe = fn;
  };
  const holder = {
    describe() {
      return "derived over " + super.describe();
    },
  };
  Object.setPrototypeOf(holder, Base);

  Array.from({ length: 64 }).forEach(() => {
    install({}, () => "plain");
  });
  install(Derived, holder.describe);
  expect(Derived.describe()).toBe("derived over base");
});
