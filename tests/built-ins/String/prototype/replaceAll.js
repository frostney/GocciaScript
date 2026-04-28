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

  test('replaceAll with callback function', () => {
    const result = 'hello hello'.replaceAll('hello', (match) => match.toUpperCase());
    expect(result).toBe('HELLO HELLO');
  });

  test('no occurrences returns original', () => {
    const str = 'hello';
    expect(str.replaceAll('xyz', 'abc')).toBe('hello');
  });

  test('replaceAll supports global regex arguments', () => {
    expect('abcabc'.replaceAll(/bc/g, 'X')).toBe('aXaX');
    expect('abcabc'.replaceAll(/(b)(c)/g, (match, b, c) => c + b)).toBe('acbacb');
  });

  test('replaceAll rejects non-global regex arguments', () => {
    expect(() => {
      'abcabc'.replaceAll(/bc/, 'X');
    }).toThrow(TypeError);
  });

  test('replaceAll expands regex replacement tokens', () => {
    expect('abcabc'.replaceAll(/(b)(c)/g, '<$2$1>')).toBe('a<cb>a<cb>');
    expect('aba'.replaceAll(/a/g, '$$')).toBe('$b$');
  });

  test('replaceAll expands named regex replacement tokens', () => {
    const re = new RegExp('(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})', 'g');
    expect('2026-04-07'.replaceAll(re, '$<day>/$<month>/$<year>')).toBe(
      '07/04/2026'
    );
  });

  test('replaceAll expands string replacement tokens', () => {
    expect('aXbXc'.replaceAll('X', '$$')).toBe('a$b$c');
    expect('aXbXc'.replaceAll('X', '$&')).toBe('aXbXc');
    expect('aXbXc'.replaceAll('X', '$`')).toBe('aabaXbc');
    expect('aXbXc'.replaceAll('X', "$'")).toBe('abXcbcc');
    expect('aXbXc'.replaceAll('X', '$1')).toBe('a$1b$1c');
  });

  test('replaceAll handles empty string search values', () => {
    expect('abc'.replaceAll('', '-')).toBe('-a-b-c-');
    expect('é'.replaceAll('', '-')).toBe('-é-');
  });

  test('replaceAll calls function replacer for empty string search values', () => {
    const offsets = [];
    const result = 'ab'.replaceAll('', (match, offset, input) => {
      offsets.push(offset);
      expect(match).toBe('');
      expect(input).toBe('ab');
      return String(offset);
    });

    expect(result).toBe('0a1b2');
    expect(offsets.length).toBe(3);
  });

  test('replaceAll dispatches through Symbol.replace', () => {
    const searchValue = {
      [Symbol.replace](input, replacement) {
        expect(input).toBe('abc');
        expect(replacement).toBe('x');
        return 'custom replaceAll';
      },
    };

    expect('abc'.replaceAll(searchValue, 'x')).toBe('custom replaceAll');
  });
});
