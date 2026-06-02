/*---
description: for-in loops enumerate object property names
features: [compat-for-in-loop]
---*/

test("enumerates enumerable own string keys", () => {
  const obj = { a: 1, b: 2 };
  const keys = [];
  for (const key in obj) keys.push(key);
  expect(keys).toEqual(["a", "b"]);
});

test("assigns to an existing identifier target", () => {
  let key;
  const keys = [];
  for (key in { a: 1, b: 2 }) {
    keys.push(key);
  }
  expect(keys).toEqual(["a", "b"]);
  expect(key).toBe("b");
});

test("assigns to an existing member target", () => {
  const state = { key: "" };
  const keys = [];
  for (state.key in { a: 1, b: 2 }) {
    keys.push(state.key);
  }
  expect(keys).toEqual(["a", "b"]);
  expect(state.key).toBe("b");
});

test("assigns to an existing private member target", () => {
  class Collector {
    #key = "";

    collect() {
      const keys = [];
      for (this.#key in { a: 1, b: 2 }) {
        keys.push(this.#key);
      }
      return [keys, this.#key];
    }
  }

  const result = new Collector().collect();
  expect(result[0]).toEqual(["a", "b"]);
  expect(result[1]).toBe("b");
});

test("skips non-enumerable own properties", () => {
  const obj = { visible: 1 };
  Object.defineProperty(obj, "hidden", {
    value: 2,
    enumerable: false,
  });

  const keys = [];
  for (const key in obj) keys.push(key);
  expect(keys).toEqual(["visible"]);
});

test("enumerates inherited enumerable string keys", () => {
  const proto = { inherited: 1 };
  const obj = Object.create(proto);
  obj.own = 2;

  const keys = [];
  for (const key in obj) keys.push(key);
  expect(keys).toEqual(["own", "inherited"]);
});

test("non-enumerable own properties shadow inherited enumerable keys", () => {
  const proto = { shadowed: 1, inherited: 2 };
  const obj = Object.create(proto);
  Object.defineProperty(obj, "shadowed", {
    value: 3,
    enumerable: false,
  });

  const keys = [];
  for (const key in obj) keys.push(key);
  expect(keys).toEqual(["inherited"]);
});

test("null and undefined right-hand sides are empty loops", () => {
  const keys = [];
  for (const key in null) keys.push(key);
  for (const key in undefined) keys.push(key);
  expect(keys).toEqual([]);
});

test("string primitives enumerate character indices", () => {
  const keys = [];
  for (const key in "abc") keys.push(key);
  expect(keys).toEqual(["0", "1", "2"]);
});

test("deleted properties are not yielded when reached", () => {
  const obj = { a: 1, b: 2 };
  const keys = [];
  for (const key in obj) {
    keys.push(key);
    delete obj.b;
  }
  expect(keys).toEqual(["a"]);
});

test("properties made non-enumerable before visit are skipped", () => {
  const obj = { a: 1, b: 2 };
  const keys = [];
  for (const key in obj) {
    keys.push(key);
    Object.defineProperty(obj, "b", { enumerable: false });
  }
  expect(keys).toEqual(["a"]);
});

test("deleted own shadow does not expose inherited property", () => {
  const proto = { shadowed: 1 };
  const obj = Object.create(proto);
  obj.a = 2;
  obj.shadowed = 3;

  const keys = [];
  for (const key in obj) {
    keys.push(key);
    delete obj.shadowed;
  }

  expect(keys).toEqual(["a"]);
});
