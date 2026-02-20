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

  test('returns first occurrence index', () => {
    expect([1, 2, 3, 2, 1].indexOf(2)).toBe(1);
  });

  test('negative fromIndex counts from end', () => {
    const arr = [1, 2, 3, 4, 5];
    expect(arr.indexOf(3, -3)).toBe(2);
    expect(arr.indexOf(1, -2)).toBe(-1);
  });

  test('negative fromIndex beyond length searches from 0', () => {
    expect([1, 2, 3].indexOf(1, -100)).toBe(0);
  });

  test('NaN is not found by indexOf (uses strict equality)', () => {
    expect([NaN, 1, 2].indexOf(NaN)).toBe(-1);
  });

  test('distinguishes null and undefined', () => {
    expect([null, undefined].indexOf(null)).toBe(0);
    expect([null, undefined].indexOf(undefined)).toBe(1);
  });

  test('fromIndex at or beyond length returns -1', () => {
    expect([1, 2, 3].indexOf(1, 3)).toBe(-1);
    expect([1, 2, 3].indexOf(1, 100)).toBe(-1);
  });

  test('empty array returns -1', () => {
    expect([].indexOf(undefined)).toBe(-1);
  });
});
