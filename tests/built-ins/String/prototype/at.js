describe('String.prototype.at', () => {
  test('returns character at positive index', () => {
    expect('hello'.at(0)).toBe('h');
    expect('hello'.at(4)).toBe('o');
  });

  test('returns character at negative index', () => {
    expect('hello'.at(-1)).toBe('o');
    expect('hello'.at(-2)).toBe('l');
  });

  test('returns undefined for out of range index', () => {
    expect('hello'.at(10)).toBe(undefined);
    expect('hello'.at(-10)).toBe(undefined);
  });
});
