/*---
description: Iterator protocol basics - next(), done, value, and Symbol.iterator self-return
features: [Iterator, Symbol.iterator]
---*/

describe("Iterator protocol", () => {
  test("next() returns {value, done} objects", () => {
    const iter = [1].values();
    const result = iter.next();
    expect(result.value).toBe(1);
    expect(result.done).toBe(false);
  });

  test("done iterator returns {value: undefined, done: true}", () => {
    const iter = [].values();
    const result = iter.next();
    expect(result.value).toBe(undefined);
    expect(result.done).toBe(true);
  });

  test("calling next() after done continues to return done", () => {
    const iter = [1].values();
    iter.next();
    const r1 = iter.next();
    const r2 = iter.next();
    expect(r1.done).toBe(true);
    expect(r2.done).toBe(true);
  });

  test("iterator is itself iterable via [Symbol.iterator]()", () => {
    const iter = [1, 2, 3].values();
    expect(iter[Symbol.iterator]()).toBe(iter);
  });

  test("spread consumes iterator from current position", () => {
    const iter = [1, 2, 3].values();
    iter.next();
    expect([...iter]).toEqual([2, 3]);
  });
});
