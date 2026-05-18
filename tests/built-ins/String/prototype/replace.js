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

  test('replace string search uses UTF-16 offsets', () => {
    const emoji = String.fromCodePoint(0x1f600);

    expect((emoji + 'a').replace('a', '$`')).toBe(emoji + emoji);
    expect((emoji + 'a').replace('a', (match, offset) => String(offset))).toBe(emoji + '2');
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
    expect('foo'.replace(/(f)/, '$2')).toBe('$2oo');
    expect('foo'.replace(/(f)/, '$12')).toBe('f2oo');
  });

  test('replace expands string replacement tokens', () => {
    expect('abc'.replace('b', '[$&]')).toBe('a[b]c');
    expect('abc'.replace('b', '$$')).toBe('a$c');
    expect('abc'.replace('b', '$`')).toBe('aac');
    expect('abc'.replace('b', "$'")).toBe('acc');
    expect('abc'.replace('b', '$1')).toBe('a$1c');
  });

  test('replace calls function replacer for empty string search values', () => {
    expect('abc'.replace('', (match, offset, input) => {
      expect(match).toBe('');
      expect(offset).toBe(0);
      expect(input).toBe('abc');
      return '-';
    })).toBe('-abc');
  });

  test('replace preserves original text around zero-width global matches', () => {
    expect('ab'.replace(/(?:)/g, '-')).toBe('-a-b-');
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

  test('Symbol.replace fires before ToString on the receiver (ES2026 §22.1.3.19 step 2.c-d)', () => {
    // The replacer must run before any ToString on `this`. If the receiver's
    // toString fires first, `poisoned` becomes true and the assertion fails.
    let poisoned = false;
    let replacerFired = false;
    const poison = { toString() { poisoned = true; return ''; } };
    const searchValue = {
      [Symbol.replace]() { replacerFired = true; return 'ok'; },
    };
    ''.replace.call(poison, searchValue, 'x');
    expect(replacerFired).toBe(true);
    expect(poisoned).toBe(false);
  });

  test('Symbol.replace receives O directly, not a stringified copy', () => {
    // Spec step 2.d.i: Call(replacer, searchValue, « O, replaceValue »).
    const receiver = { tag: 'unique-receiver' };
    let receivedThis;
    const searchValue = {
      [Symbol.replace](o) { receivedThis = o; return 'ok'; },
    };
    ''.replace.call(receiver, searchValue, 'x');
    expect(receivedThis).toBe(receiver);
  });

  test('throws TypeError when called on null or undefined', () => {
    expect(() => ''.replace.call(null, /x/, 'y')).toThrow(TypeError);
    expect(() => ''.replace.call(undefined, /x/, 'y')).toThrow(TypeError);
  });
});
