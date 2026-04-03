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

  test('replace supports regex arguments', () => {
    expect('abcabc'.replace(/bc/, 'X')).toBe('aXabc');
    expect('abc'.replace(/(b)(c)/, (match, b, c, index, input) => {
      expect(match).toBe('bc');
      expect(b).toBe('b');
      expect(c).toBe('c');
      expect(index).toBe(1);
      expect(input).toBe('abc');
      return c + b;
    })).toBe('acb');
  });

  test('replace replaces all matches for global regex arguments', () => {
    expect('abcabc'.replace(/bc/g, 'X')).toBe('aXaX');
  });

  test('replace expands regex replacement tokens', () => {
    expect('abc'.replace(/b/, '[$&]')).toBe('a[b]c');
    expect('abc'.replace(/(b)/, '<$1>')).toBe('a<b>c');
    expect('abc'.replace(/b/, '$$')).toBe('a$c');
    expect('abc'.replace(/b/, '$`')).toBe('aac');
    expect('abc'.replace(/b/, "$'")).toBe('acc');
    expect('b'.replace(/(a)?b/, 'x$1y')).toBe('xy');
  });

  test('replace dispatches through Symbol.replace', () => {
    const searchValue = {
      [Symbol.replace](input, replacement) {
        expect(input).toBe('abc');
        expect(replacement).toBe('x');
        return 'custom replace';
      },
    };

    expect('abc'.replace(searchValue, 'x')).toBe('custom replace');
  });
});
