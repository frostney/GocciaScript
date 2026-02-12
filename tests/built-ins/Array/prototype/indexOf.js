describe('Array.prototype.indexOf', () => {
  test('returns the index of the first matching element', () => {
    expect([1, 2, 3, 4, 5].indexOf(3)).toBe(2);
  });

  test('returns -1 if element not found', () => {
    expect([1, 2, 3].indexOf(10)).toBe(-1);
  });

  test('searches from fromIndex', () => {
    expect([1, 2, 3, 2, 1].indexOf(2, 2)).toBe(3);
  });

  test('uses strict equality', () => {
    expect([1, 2, 3].indexOf('2')).toBe(-1);
  });
});
