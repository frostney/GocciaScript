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

test("Promise.all with single element", () => {
  return Promise.all([Promise.resolve(42)]).then((v) => {
    expect(v).toEqual([42]);
  });
});

test("Promise.all with multiple rejections only catches first", () => {
  return Promise.all([
    Promise.reject("first"),
    Promise.reject("second"),
    Promise.reject("third")
  ]).catch((e) => {
    expect(e).toBe("first");
  });
});

test("Promise.all with string iterates characters", () => {
  return Promise.all("hello").then((result) => {
    expect(result).toEqual(["h", "e", "l", "l", "o"]);
  });
});

test("Promise.all with Set iterates values", () => {
  return Promise.all(new Set([1, 2, 3])).then((result) => {
    expect(result).toEqual([1, 2, 3]);
  });
});

test("Promise.all with Set of promises", () => {
  const s = new Set([Promise.resolve("a"), Promise.resolve("b")]);
  return Promise.all(s).then((result) => {
    expect(result).toEqual(["a", "b"]);
  });
});

test("Promise.all with Map iterates entries", () => {
  const m = new Map([["x", 1], ["y", 2]]);
  return Promise.all(m).then((result) => {
    expect(result).toEqual([["x", 1], ["y", 2]]);
  });
});
