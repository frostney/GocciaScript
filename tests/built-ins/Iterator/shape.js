/*---
description: Iterator constructor and prototype object shape
features: [Iterator, iterator-helpers]
---*/

describe("Iterator shape", () => {
  test("Iterator is a function but not directly callable or constructible", () => {
    expect(typeof Iterator).toBe("function");
    expect(Object.getPrototypeOf(Iterator)).toBe(Function.prototype);
    expect(() => Iterator()).toThrow(TypeError);
    expect(() => new Iterator()).toThrow(TypeError);
  });

  test("Iterator.prototype.constructor is an accessor returning Iterator", () => {
    const desc = Object.getOwnPropertyDescriptor(Iterator.prototype, "constructor");
    expect(typeof desc.get).toBe("function");
    expect(typeof desc.set).toBe("function");
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(desc.value).toBe(undefined);
    expect(desc.writable).toBe(undefined);
    expect(Iterator.prototype.constructor).toBe(Iterator);
    expect(desc.get.call()).toBe(Iterator);
  });

  test("Iterator.prototype Symbol.toStringTag is an accessor", () => {
    const desc = Object.getOwnPropertyDescriptor(Iterator.prototype, Symbol.toStringTag);
    expect(typeof desc.get).toBe("function");
    expect(typeof desc.set).toBe("function");
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(desc.value).toBe(undefined);
    expect(desc.writable).toBe(undefined);
    expect(Iterator.prototype[Symbol.toStringTag]).toBe("Iterator");
    expect(Object.prototype.toString.call(Iterator.prototype)).toBe("[object Iterator]");
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
