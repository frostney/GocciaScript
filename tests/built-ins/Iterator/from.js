/*---
description: Iterator.from() wraps iterables and iterator-like objects
features: [Iterator, Iterator.from]
---*/

describe("Iterator.from()", () => {
  test("returns an existing iterator as-is", () => {
    const iter = [1, 2, 3].values();
    expect(Iterator.from(iter)).toBe(iter);
  });

  test("wraps a Set iterable", () => {
    const set = new Set([1, 2, 3]);
    const iter = Iterator.from(set);
    expect(iter.next().value).toBe(1);
    expect(iter.next().value).toBe(2);
    expect(iter.next().value).toBe(3);
    expect(iter.next().done).toBe(true);
  });

  test("wraps a Map iterable", () => {
    const map = new Map([["a", 1]]);
    const iter = Iterator.from(map);
    const entry = iter.next().value;
    expect(entry[0]).toBe("a");
    expect(entry[1]).toBe(1);
    expect(iter.next().done).toBe(true);
  });

  test("wraps a string iterable", () => {
    const iter = Iterator.from("abc");
    expect(iter.next().value).toBe("a");
    expect(iter.next().value).toBe("b");
    expect(iter.next().value).toBe("c");
    expect(iter.next().done).toBe(true);
  });

  test("wraps a user-defined iterable", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 2) {
              return { value: i * 10, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    const iter = Iterator.from(iterable);
    expect(iter.next().value).toBe(10);
    expect(iter.next().value).toBe(20);
    expect(iter.next().done).toBe(true);
  });

  test("wraps an iterator-like object with next()", () => {
    let count = 0;
    const iteratorLike = {
      next() {
        count = count + 1;
        if (count <= 3) {
          return { value: count, done: false };
        }
        return { value: undefined, done: true };
      }
    };
    const iter = Iterator.from(iteratorLike);
    expect(iter.next().value).toBe(1);
    expect(iter.next().value).toBe(2);
    expect(iter.next().value).toBe(3);
    expect(iter.next().done).toBe(true);
  });

  test("wrapped iterator has helper methods", () => {
    const set = new Set([1, 2, 3, 4, 5]);
    const result = Iterator.from(set).filter((x) => x > 2).toArray();
    expect(result).toEqual([3, 4, 5]);
  });

  test("Iterator.from on array (iterable)", () => {
    const iter = Iterator.from([10, 20, 30]);
    expect(iter.toArray()).toEqual([10, 20, 30]);
  });

  test("Iterator.prototype is exposed", () => {
    expect(Iterator.prototype).toBeDefined();
  });

  test("throws TypeError for null", () => {
    expect(() => Iterator.from(null)).toThrow(TypeError);
  });

  test("throws TypeError for undefined", () => {
    expect(() => Iterator.from(undefined)).toThrow(TypeError);
  });
});
