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

test("array-index property names are ordered before other string keys", () => {
  const obj = { p2: 0, p4: 0, p1: 0 };
  obj[2] = 0;
  obj[0] = 0;
  obj[1] = 0;

  const keys = [];
  for (const key in obj) keys.push(key);

  expect(keys).toEqual(["0", "1", "2", "p2", "p4", "p1"]);
});

test("lexical loop head names are in TDZ while evaluating the source", () => {
  let key = "outer";
  expect(() => {
    for (let key in { [key]: 1 }) {
    }
  }).toThrow(ReferenceError);
  expect(key).toBe("outer");
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

test("enumerable own key shadowing a same-named inherited key is yielded once", () => {
  const proto = { shared: "proto", onlyProto: 1 };
  const obj = Object.create(proto);
  obj.shared = "own";
  obj.onlyOwn = 2;

  const keys = [];
  for (const key in obj) keys.push(key);

  // "shared" appears once, in its own (nearer) position; never re-yielded
  // from the prototype.
  expect(keys).toEqual(["shared", "onlyOwn", "onlyProto"]);
});

test("dedups same-named keys across a deep prototype chain at scale", () => {
  // Build a 12-level chain where every level redeclares the same 40 keys,
  // plus one level-unique key. Each shared key must be yielded exactly once
  // (from the nearest level); per-level order is preserved.
  const LEVELS = 12;
  const SHARED = 40;
  const sharedKeys = Array.from({ length: SHARED }, (_, i) => "k" + i);

  const leaf = Array.from({ length: LEVELS }).reduce((proto, _unused, level) => {
    const obj = Object.create(proto);
    for (const key of sharedKeys) obj[key] = level;
    obj["unique" + level] = level;
    return obj;
  }, null);

  const seen = [];
  const counts = {};
  for (const key in leaf) {
    seen.push(key);
    counts[key] = (counts[key] || 0) + 1;
  }

  // Every key yielded exactly once.
  for (const key of seen) expect(counts[key]).toBe(1);

  // The nearest (leaf) level owns the shared keys, in their declared order,
  // before any inherited level-unique keys.
  expect(seen.slice(0, SHARED)).toEqual(sharedKeys);

  // All shared keys plus one unique key per level, no duplicates.
  expect(seen.length).toBe(SHARED + LEVELS);

  // Shared keys resolve to the leaf level's value.
  expect(leaf.k0).toBe(LEVELS - 1);
});
