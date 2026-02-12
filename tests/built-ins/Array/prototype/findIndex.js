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
});
