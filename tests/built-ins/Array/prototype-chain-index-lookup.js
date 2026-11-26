/*---
description: Array methods on array-likes find indices on the prototype chain, and skip indices nothing defines
features: [Array]
---*/

test("an index defined only on Object.prototype is found through an array-like", () => {
  Object.defineProperty(Object.prototype, "7", { get() { return "g"; }, configurable: true });
  try {
    expect(Array.prototype.indexOf.call({ length: 10 }, "g")).toBe(7);
    expect(Array.prototype.includes.call({ length: 10 }, "g")).toBe(true);
  } finally {
    delete Object.prototype[7];
  }
  expect(Array.prototype.includes.call({ length: 10 }, "g")).toBe(false);
});

test("an index on a plain prototype object is found", () => {
  const proto = { 3: "p" };
  const arrayLike = Object.create(proto);
  arrayLike.length = 5;
  expect(Array.prototype.indexOf.call(arrayLike, "p")).toBe(3);
  expect(Array.prototype.join.call(arrayLike, "-")).toBe("---p-");
});

test("an array-like with a long length and few indices is walked", () => {
  const arrayLike = { length: 200000, 199999: "end" };
  expect(Array.prototype.indexOf.call(arrayLike, "end")).toBe(199999);
  expect(Array.prototype.lastIndexOf.call(arrayLike, "missing")).toBe(-1);
});
