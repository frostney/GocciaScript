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

  test('negative fromIndex counts from end', () => {
    expect([1, 2, 3, 2, 1].lastIndexOf(2, -2)).toBe(3);
    expect([1, 2, 3, 2, 1].lastIndexOf(2, -4)).toBe(1);
  });

  test('NaN is not found (uses strict equality)', () => {
    expect([NaN, 1, 2].lastIndexOf(NaN)).toBe(-1);
  });

  test('uses strict equality', () => {
    expect([1, 2, 3].lastIndexOf('2')).toBe(-1);
  });

  test('empty array returns -1', () => {
    expect([].lastIndexOf(1)).toBe(-1);
  });

  test('negative fromIndex beyond length returns -1', () => {
    expect([1, 2, 3].lastIndexOf(1, -100)).toBe(-1);
  });
});
