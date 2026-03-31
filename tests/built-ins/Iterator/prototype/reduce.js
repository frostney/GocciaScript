/*---
description: Iterator.prototype.reduce helper behavior
features: [Iterator.prototype.reduce]
---*/

describe("Iterator.prototype.reduce()", () => {
  test("reduces with an initial value", () => {
    expect([1, 2, 3].values().reduce((acc, x) => acc + x, 0)).toBe(6);
  });

  test("reduces without an initial value using the first element", () => {
    expect([1, 2, 3].values().reduce((acc, x) => acc + x)).toBe(6);
  });

  test("reduce callback receives the iteration counter", () => {
    const counters = [];

    [10, 20, 30].values().reduce((acc, value, index) => {
      counters.push(index);
      return acc + value;
    }, 0);

    expect(counters).toEqual([0, 1, 2]);
  });

  test("reduce without an initial value starts the counter at 1", () => {
    const counters = [];

    [10, 20, 30].values().reduce((acc, value, index) => {
      counters.push(index);
      return acc + value;
    });

    expect(counters).toEqual([1, 2]);
  });

  test("reduce on an empty iterator without an initial value throws TypeError", () => {
    expect(() => [].values().reduce((acc, x) => acc + x)).toThrow(TypeError);
  });

  test("reduce closes the source iterator when the callback throws", () => {
    let closed = 0;
    const source = Iterator.from({
      count: 0,
      next() {
        this.count = this.count + 1;
        if (this.count <= 2) {
          return { value: this.count, done: false };
        }
        return { value: undefined, done: true };
      },
      return() {
        closed = closed + 1;
        return { value: undefined, done: true };
      },
    });

    expect(() => source.reduce(() => {
      throw new Error("boom");
    }, 0)).toThrow(Error);
    expect(closed).toBe(1);
  });

  test("reduce can consume nested iterators", () => {
    const result = Iterator.from([1, 2, 3][Symbol.iterator]())
      .reduce((acc, x) => {
        const inner = Iterator.from([x, x * 2, x * 3][Symbol.iterator]())
          .filter((v) => v > 2)
          .toArray();
        return [...acc, ...inner];
      }, []);

    expect(result).toEqual([3, 4, 6, 3, 6, 9]);
  });
});
