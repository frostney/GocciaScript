describe('Relational Operator Type Coercion', () => {
  test('string number compared to number', () => {
    expect("10" > 2).toBe(true);
    expect("1" < 2).toBe(true);
  });

  test('boolean compared to number', () => {
    expect(true > 0).toBe(true);
    expect(false < 1).toBe(true);
  });

  test('null compared to number', () => {
    expect(null < 1).toBe(true);
    expect(null >= 0).toBe(true);
  });

  test('undefined in comparison always false', () => {
    expect(undefined > 0).toBe(false);
    expect(undefined < 0).toBe(false);
    expect(undefined >= 0).toBe(false);
    expect(undefined <= 0).toBe(false);
  });

  test('NaN comparisons are always false', () => {
    expect(NaN > 0).toBe(false);
    expect(NaN < 0).toBe(false);
    expect(NaN >= 0).toBe(false);
    expect(NaN <= 0).toBe(false);
  });

  test('string-to-string comparison is lexicographic', () => {
    expect("abc" < "abd").toBe(true);
    expect("z" > "a").toBe(true);
  });

  test('boolean to string comparison coerces to number', () => {
    expect(true > "0").toBe(true);
  });
});
