/*---
description: Iterator.prototype.some helper behavior
features: [Iterator.prototype.some]
---*/

describe("Iterator.prototype.some()", () => {
  test("returns true if any element matches", () => {
    expect([1, 2, 3].values().some((x) => x === 2)).toBe(true);
  });

  test("returns false if no elements match", () => {
    expect([1, 2, 3].values().some((x) => x === 5)).toBe(false);
  });

  test("some closes the source iterator after a truthy match", () => {
    let closed = 0;
    const source = Iterator.from({
      count: 0,
      next() {
        this.count = this.count + 1;
        if (this.count <= 3) {
          return { value: this.count, done: false };
        }
        return { value: undefined, done: true };
      },
      return() {
        closed = closed + 1;
        return { value: undefined, done: true };
      },
    });

    expect(source.some((value) => value === 2)).toBe(true);
    expect(closed).toBe(1);
  });

  test("some throws TypeError when return() does not return an object", () => {
    const source = Iterator.from({
      next() {
        return { value: 1, done: false };
      },
      return() {
        return 0;
      },
    });

    expect(() => source.some(() => true)).toThrow(TypeError);
  });

  test("some can use nested iterator predicates", () => {
    const data = [[1, 3, 5], [2, 4, 6], [7, 9, 11]];
    const result = Iterator.from(data[Symbol.iterator]())
      .some((group) =>
        Iterator.from(group[Symbol.iterator]())
          .every((x) => x % 2 === 0),
      );

    expect(result).toBe(true);
  });
});
