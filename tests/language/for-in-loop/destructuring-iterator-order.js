/*---
description: |
  An array binding pattern in a for-in head iterates the key string one
  element at a time, running each default during its own step.
  ES2026 §8.6.3 IteratorBindingInitialization.
features: [compat-for-in-loop, destructuring, iterators]
---*/

test("a for-in head runs each default before stepping the next character", () => {
  const log = [];
  const original = String.prototype[Symbol.iterator];
  const patch = {
    [Symbol.iterator]() {
      const text = String(this);
      let index = 0;
      return {
        next() {
          log.push("next");
          if (index < text.length) {
            // The first character reads as undefined so its default runs.
            const value = index === 0 ? undefined : text[index];
            index++;
            return { value, done: false };
          }
          return { value: undefined, done: true };
        },
        return() {
          log.push("return");
          return {};
        },
      };
    },
  };
  String.prototype[Symbol.iterator] = patch[Symbol.iterator];
  const seen = [];
  try {
    for (const [a = (log.push("default"), "d"), b] in { xyz: 1 }) {
      seen.push(a, b);
    }
  } finally {
    String.prototype[Symbol.iterator] = original;
  }
  expect(seen).toEqual(["d", "y"]);
  expect(log).toEqual(["next", "default", "next", "return"]);
});
