describe("Array.prototype.findLast", () => {
  test("returns the last element matching the predicate", () => {
    expect([1, 2, 3, 4, 5].findLast((x) => x > 3)).toBe(5);
    expect([1, 2, 3, 4, 5].findLast((x) => x > 2)).toBe(5);
  });

  test("returns the last match, not the first", () => {
    expect([1, 2, 3, 2, 1].findLast((x) => x === 2)).toBe(2);
  });

  test("returns undefined if no element matches", () => {
    expect([1, 2, 3].findLast((x) => x > 10)).toBe(undefined);
  });

  test("callback receives element, index, and array", () => {
    const arr = [10, 20, 30];
    const collected = [];
    arr.findLast((val, idx, a) => {
      collected.push({ val, idx, len: a.length });
      return false;
    });
    expect(collected.length).toBe(3);
    expect(collected[0].val).toBe(30);
    expect(collected[0].idx).toBe(2);
  });

  test("empty array returns undefined", () => {
    expect([].findLast(() => true)).toBe(undefined);
  });

  test("stops iteration after last match found", () => {
    let callCount = 0;
    [1, 2, 3, 4, 5].findLast((x) => {
      callCount = callCount + 1;
      return x === 4;
    });
    expect(callCount).toBe(2);
  });
});
