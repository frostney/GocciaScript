describe("Array.prototype.findLastIndex", () => {
  test("returns the index of the last matching element", () => {
    expect([1, 2, 3, 4, 5].findLastIndex((x) => x > 3)).toBe(4);
  });

  test("returns last index when multiple match", () => {
    expect([1, 2, 3, 2, 1].findLastIndex((x) => x === 2)).toBe(3);
  });

  test("returns -1 if no element matches", () => {
    expect([1, 2, 3].findLastIndex((x) => x > 10)).toBe(-1);
  });

  test("callback receives element, index, and array", () => {
    const arr = [10, 20, 30];
    const indices = [];
    arr.findLastIndex((val, idx) => {
      indices.push(idx);
      return false;
    });
    expect(indices).toEqual([2, 1, 0]);
  });

  test("empty array returns -1", () => {
    expect([].findLastIndex(() => true)).toBe(-1);
  });
});
