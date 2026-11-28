/*---
description: >
  Reading an array's holes and serializing its primitive elements makes no
  property key for each index. Keys live as long as the engine and count
  against the memory limit, so one key per index would be refused here.
features: [Goccia.gc]
---*/

const sparse = (length) => {
  const a = [];
  a.length = 4e9; // past the dense limit: the array keeps no elements
  a.length = length;
  return a;
};

describe("index keys under the memory limit", () => {
  test("join reads a sparse array's holes without a key each", () => {
    expect(sparse(1e7).join("")).toBe("");
  });

  test("JSON.stringify writes a sparse array's holes without a key each", () => {
    const text = JSON.stringify(sparse(1e6));
    expect(text.length).toBe(5e6 + 1);
    expect(text.startsWith("[null,null,")).toBe(true);
  });

  test("JSON.stringify writes primitive elements without a key each", () => {
    const text = JSON.stringify(Array.from({ length: 400000 }, (_, i) => i));
    expect(JSON.parse(text).length).toBe(400000);
  });
});
