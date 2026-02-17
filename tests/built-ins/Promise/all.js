/*---
description: Promise.all waits for all promises to resolve
features: [Promise.all]
---*/

test("Promise.all with empty array", () => {
  return Promise.all([]).then((v) => {
    expect(v).toEqual([]);
  });
});

test("Promise.all with all resolved", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.resolve(2),
    Promise.resolve(3)
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all preserves order", () => {
  return Promise.all([
    Promise.resolve("a"),
    Promise.resolve("b"),
    Promise.resolve("c")
  ]).then((v) => {
    expect(v).toEqual(["a", "b", "c"]);
  });
});

test("Promise.all with non-promise values", () => {
  return Promise.all([1, 2, 3]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all rejects on first rejection", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.reject("err"),
    Promise.resolve(3)
  ]).catch((e) => {
    expect(e).toBe("err");
  });
});

test("Promise.all with mixed promises and values", () => {
  return Promise.all([
    1,
    Promise.resolve(2),
    3
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});
