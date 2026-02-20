describe('Array.prototype.findIndex', () => {
  test('returns the index of the first matching element', () => {
    const arr = [1, 2, 3, 4, 5];
    expect(arr.findIndex((x) => x > 3)).toBe(3);
  });

  test('returns -1 if no element matches', () => {
    const arr = [1, 2, 3];
    expect(arr.findIndex((x) => x > 10)).toBe(-1);
  });

  test('returns index of first match when multiple match', () => {
    const arr = [1, 2, 3, 2, 1];
    expect(arr.findIndex((x) => x === 2)).toBe(1);
  });

  test('callback receives element, index, and array', () => {
    const arr = [10, 20, 30];
    const collected = [];
    arr.findIndex((val, idx, a) => {
      collected.push({ val, idx, len: a.length });
      return false;
    });
    expect(collected.length).toBe(3);
    expect(collected[1].val).toBe(20);
    expect(collected[1].idx).toBe(1);
  });

  test('stops iteration after first match', () => {
    let callCount = 0;
    [1, 2, 3, 4, 5].findIndex((x) => {
      callCount = callCount + 1;
      return x === 3;
    });
    expect(callCount).toBe(3);
  });

  test('empty array returns -1', () => {
    expect([].findIndex(() => true)).toBe(-1);
  });
});
