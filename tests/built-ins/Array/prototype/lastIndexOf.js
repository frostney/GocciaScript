describe('Array.prototype.lastIndexOf', () => {
  test('returns the last index of a matching element', () => {
    expect([1, 2, 3, 2, 1].lastIndexOf(2)).toBe(3);
  });

  test('returns -1 if element not found', () => {
    expect([1, 2, 3].lastIndexOf(10)).toBe(-1);
  });

  test('searches backwards from fromIndex', () => {
    expect([1, 2, 3, 2, 1].lastIndexOf(2, 2)).toBe(1);
  });
});
