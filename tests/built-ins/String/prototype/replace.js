describe('String.prototype.replace', () => {
  test('replaces only the first occurrence', () => {
    expect('aabbcc'.replace('b', 'x')).toBe('aaxbcc');
  });

  test('replaces first occurrence in longer string', () => {
    expect('hello world hello'.replace('hello', 'hi')).toBe('hi world hello');
  });

  test('is case-sensitive', () => {
    expect('Hello hello'.replace('hello', 'world')).toBe('Hello world');
  });

  test('returns original when search not found', () => {
    expect('hello'.replace('xyz', 'abc')).toBe('hello');
  });

  test('replaces with empty string', () => {
    expect('hello world'.replace('world', '')).toBe('hello ');
  });

  test('replaces empty string at start', () => {
    expect('hello'.replace('', 'x')).toBe('xhello');
  });

  test('replace with callback function', () => {
    const result = 'hello world'.replace('world', (match) => match.toUpperCase());
    expect(result).toBe('hello WORLD');
  });
});
