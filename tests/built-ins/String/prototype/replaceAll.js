describe('String.prototype.replaceAll', () => {
  test('replaces all occurrences of a substring', () => {
    expect('aabbcc'.replaceAll('b', 'x')).toBe('aaxxcc');
  });

  test('replaces all occurrences when multiple exist', () => {
    expect('hello world hello'.replaceAll('hello', 'hi')).toBe('hi world hi');
  });

  test('returns original string when search not found', () => {
    expect('hello'.replaceAll('xyz', 'abc')).toBe('hello');
  });

  test('replaces with empty string', () => {
    expect('aabbcc'.replaceAll('b', '')).toBe('aacc');
  });
});
