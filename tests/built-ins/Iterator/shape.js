/*---
description: Iterator constructor and helper object shape
features: [Iterator, iterator-helpers]
---*/

describe("Iterator shape", () => {
  test("Iterator is a function but not directly callable or constructible", () => {
    expect(typeof Iterator).toBe("function");
    expect(Object.getPrototypeOf(Iterator)).toBe(Function.prototype);
    expect(() => Iterator()).toThrow(TypeError);
    expect(() => new Iterator()).toThrow(TypeError);
  });

  test("helper return values are instanceof Iterator", () => {
    const mapped = [1, 2, 3].values().map((value) => value);
    expect(mapped instanceof Iterator).toBe(true);
  });

  test("helpers accept plain iterator protocol objects", () => {
    let count = 0;
    const iter = {
      next() {
        count = count + 1;
        if (count <= 3) {
          return { value: count, done: false };
        }
        return { value: undefined, done: true };
      },
    };

    const result = Iterator.prototype.map.call(iter, (value) => value * 2).toArray();
    expect(result).toEqual([2, 4, 6]);
  });

});
