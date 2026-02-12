/*---
description: Object.fromEntries
features: [Object.fromEntries]
---*/

describe("Object.fromEntries", () => {
  test("basic key-value pairs", () => {
    const entries = [["a", 1], ["b", 2], ["c", 3]];
    const obj = Object.fromEntries(entries);
    expect(obj.a).toBe(1);
    expect(obj.b).toBe(2);
    expect(obj.c).toBe(3);
  });

  test("roundtrip with Object.entries", () => {
    const original = { x: 10, y: 20 };
    const roundtripped = Object.fromEntries(Object.entries(original));
    expect(roundtripped.x).toBe(10);
    expect(roundtripped.y).toBe(20);
  });

  test("empty entries array produces empty object", () => {
    const obj = Object.fromEntries([]);
    expect(Object.keys(obj).length).toBe(0);
  });

  test("duplicate keys use last value", () => {
    const entries = [["a", 1], ["a", 2]];
    const obj = Object.fromEntries(entries);
    expect(obj.a).toBe(2);
  });
});
