/*---
description: Iterator.prototype.toArray helper behavior
features: [Iterator.prototype.toArray]
---*/

describe("Iterator.prototype.toArray()", () => {
  test("converts an iterator to an array", () => {
    expect([1, 2, 3].values().toArray()).toEqual([1, 2, 3]);
  });

  test("toArray on an empty iterator returns an empty array", () => {
    expect([].values().toArray()).toEqual([]);
  });

  test("toArray preserves chained lazy helper composition", () => {
    const result = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].values()
      .filter((x) => x % 2 === 0)
      .map((x) => x * 3)
      .take(3)
      .toArray();

    expect(result).toEqual([6, 12, 18]);
  });
});
