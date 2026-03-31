/*---
description: Iterator.prototype.every helper behavior
features: [Iterator.prototype.every]
---*/

describe("Iterator.prototype.every()", () => {
  test("returns true if all elements match", () => {
    expect([2, 4, 6].values().every((x) => x % 2 === 0)).toBe(true);
  });

  test("returns false if an element fails the predicate", () => {
    expect([2, 3, 6].values().every((x) => x % 2 === 0)).toBe(false);
  });

  test("every closes the source iterator after the first falsy result", () => {
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

    expect(source.every((value) => value < 2)).toBe(false);
    expect(closed).toBe(1);
  });
});
