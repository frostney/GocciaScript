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
});
