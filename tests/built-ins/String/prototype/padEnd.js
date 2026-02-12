describe('String.prototype.padEnd', () => {
  test('pads string to target length with spaces by default', () => {
    expect('abc'.padEnd(6)).toBe('abc   ');
  });

  test('pads string with custom pad string', () => {
    expect('abc'.padEnd(6, '0')).toBe('abc000');
  });

  test('returns original string if already at target length', () => {
    expect('abc'.padEnd(3)).toBe('abc');
  });

  test('returns original string if longer than target length', () => {
    expect('abcdef'.padEnd(3)).toBe('abcdef');
  });

  test('truncates pad string to fit exactly', () => {
    expect('abc'.padEnd(8, '12')).toBe('abc12121');
  });
});
