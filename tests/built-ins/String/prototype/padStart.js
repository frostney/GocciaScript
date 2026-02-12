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
