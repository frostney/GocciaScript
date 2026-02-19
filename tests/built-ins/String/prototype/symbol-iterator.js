/*---
description: String.prototype[Symbol.iterator] iterates over characters
features: [String, Symbol.iterator, Iterator]
---*/

test("iterates over characters", () => {
  const iter = "abc"[Symbol.iterator]();
  expect(iter.next().value).toBe("a");
  expect(iter.next().value).toBe("b");
  expect(iter.next().value).toBe("c");
  expect(iter.next().done).toBe(true);
});

test("empty string iterator", () => {
  const iter = ""[Symbol.iterator]();
  expect(iter.next().done).toBe(true);
});

test("spread on string produces characters", () => {
  expect([..."hello"]).toEqual(["h", "e", "l", "l", "o"]);
});

test("destructuring a string", () => {
  const [a, b, c] = "xyz";
  expect(a).toBe("x");
  expect(b).toBe("y");
  expect(c).toBe("z");
});

test("rest pattern with string destructuring", () => {
  const [first, ...rest] = "abcd";
  expect(first).toBe("a");
  expect(rest).toEqual(["b", "c", "d"]);
});
