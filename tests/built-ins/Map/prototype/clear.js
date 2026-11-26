/*---
description: Map.prototype.clear removes all entries from the Map
features: [Map.prototype.clear]
---*/

test("clear removes all entries", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.size).toBe(3);
  map.clear();
  expect(map.size).toBe(0);
  expect(map.has("a")).toBe(false);
  expect(map.has("b")).toBe(false);
  expect(map.has("c")).toBe(false);
});

test("clear on empty Map", () => {
  const map = new Map();
  map.clear();
  expect(map.size).toBe(0);
});

test("set after clear", () => {
  const map = new Map([["a", 1]]);
  map.clear();
  map.set("b", 2);
  expect(map.size).toBe(1);
  expect(map.has("a")).toBe(false);
  expect(map.has("b")).toBe(true);
});

test("throws TypeError when called on non-Map", () => {
  const clear = Map.prototype.clear;
  expect(() => clear.call(Map.prototype)).toThrow(TypeError);
  expect(() => clear.call({})).toThrow(TypeError);
  expect(() => clear.call(new Set())).toThrow(TypeError);
});

test("an iterator part-way through sees the entries added after a clear", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  const it = map.keys();
  expect(it.next().value).toBe("a");
  map.clear();
  map.set("x", 9);
  map.set("y", 8);
  expect([...it]).toEqual(["x", "y"]);
});

test("forEach goes on with the entries added after a clear in its callback", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  const seen = [];
  let cleared = false;
  map.forEach((value, key) => {
    seen.push(key);
    if (!cleared) {
      cleared = true;
      map.clear();
      map.set("z", 0);
    }
  });
  expect(seen).toEqual(["a", "z"]);
});

test("a Set iterator part-way through sees the values added after a clear", () => {
  const set = new Set([1, 2, 3]);
  const it = set.values();
  expect(it.next().value).toBe(1);
  set.clear();
  set.add(7);
  expect([...it]).toEqual([7]);
});
