/*---
description: Iterator.prototype.find helper behavior
features: [Iterator.prototype.find]
---*/

describe("Iterator.prototype.find()", () => {
  test("returns the first matching element", () => {
    expect([1, 2, 3, 4].values().find((x) => x > 2)).toBe(3);
  });

  test("returns undefined if no element matches", () => {
    expect([1, 2, 3].values().find((x) => x > 10)).toBe(undefined);
  });

  test("find closes the source iterator after finding a match", () => {
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

    expect(source.find((value) => value === 2)).toBe(2);
    expect(closed).toBe(1);
  });

  test("find short-circuits after filtering", () => {
    const result = [1, 2, 3, 4, 5].values()
      .filter((x) => x > 2)
      .find((x) => x === 4);

    expect(result).toBe(4);
  });

  test("find can use nested iterator computations", () => {
    const data = [[1, 1, 1], [2, 3, 4], [10, 20, 30]];
    const result = Iterator.from(data[Symbol.iterator]())
      .find((group) =>
        Iterator.from(group[Symbol.iterator]())
          .reduce((a, b) => a + b, 0) > 15,
      );

    expect(result).toEqual([10, 20, 30]);
  });
});
