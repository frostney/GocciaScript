/*---
description: Iterator.prototype.forEach helper behavior
features: [Iterator.prototype.forEach]
---*/

describe("Iterator.prototype.forEach()", () => {
  test("calls the callback for each value", () => {
    const collected = [];
    [1, 2, 3].values().forEach((x) => collected.push(x));
    expect(collected).toEqual([1, 2, 3]);
  });

  test("forEach returns undefined", () => {
    expect([1].values().forEach((x) => x)).toBe(undefined);
  });

  test("forEach callback receives the iterator index", () => {
    const indices = [];
    [10, 20, 30].values().forEach((value, index) => {
      indices.push(index);
    });
    expect(indices).toEqual([0, 1, 2]);
  });

  test("forEach closes the source iterator when the callback throws", () => {
    let closed = 0;
    const source = Iterator.from({
      next() {
        return { value: 1, done: false };
      },
      return() {
        closed = closed + 1;
        return { value: undefined, done: true };
      },
    });

    expect(() => source.forEach(() => {
      throw new Error("boom");
    })).toThrow(Error);
    expect(closed).toBe(1);
  });
});
