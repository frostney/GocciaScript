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

test("iterator combines explicitly encoded surrogate pairs", () => {
  const pair = String.fromCharCode(0xD83D, 0xDE00);
  const iter = pair[Symbol.iterator]();
  expect(iter.next()).toEqual({ value: "😀", done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("spread on string produces characters", () => {
  expect([..."hello"]).toEqual(["h", "e", "l", "l", "o"]);
});

test("spread on string preserves UTF-8 characters", () => {
  expect([..."é中"]).toEqual(["é", "中"]);
  expect([..."😀"]).toEqual(["😀"]);
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

test("String iterator prototype inherits from Iterator.prototype", () => {
  const iteratorPrototype = Object.getPrototypeOf(
    Object.getPrototypeOf([][Symbol.iterator]())
  );
  const stringIteratorPrototype = Object.getPrototypeOf(""[Symbol.iterator]());
  const tagDescriptor = Object.getOwnPropertyDescriptor(
    stringIteratorPrototype,
    Symbol.toStringTag
  );

  expect(Object.getPrototypeOf(stringIteratorPrototype)).toBe(iteratorPrototype);
  expect(tagDescriptor.value).toBe("String Iterator");
  expect(tagDescriptor.writable).toBe(false);
  expect(tagDescriptor.enumerable).toBe(false);
  expect(tagDescriptor.configurable).toBe(true);
});
