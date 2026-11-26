describe('String.prototype.padStart', () => {
  test('pads string to target length with spaces by default', () => {
    expect('abc'.padStart(6)).toBe('   abc');
  });

  test('pads string with custom pad string', () => {
    expect('abc'.padStart(6, '0')).toBe('000abc');
  });

  test('returns original string if already at target length', () => {
    expect('abc'.padStart(3)).toBe('abc');
  });

  test('returns original string if longer than target length', () => {
    expect('abcdef'.padStart(3)).toBe('abcdef');
  });

  test('truncates pad string to fit exactly', () => {
    expect('abc'.padStart(8, '12')).toBe('12121abc');
  });

  test('handles empty pad string', () => {
    expect('abc'.padStart(6, '')).toBe('abc');
  });
});

test("padStart does not coerce the filler when there is nothing to pad (22.1.3.17.1)", () => {
  let calls = 0;
  const fill = { toString() { calls++; return "x"; } };
  expect("abc".padStart(2, fill)).toBe("abc");
  expect("abc".padEnd(3, fill)).toBe("abc");
  expect(calls).toBe(0);
  expect("abc".padStart(5, fill)).toBe("xxabc");
  expect(calls).toBe(1);
});
