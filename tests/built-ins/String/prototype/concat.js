describe('String.prototype.concat', () => {
  test('concatenates two strings', () => {
    expect('hello'.concat(' world')).toBe('hello world');
  });

  test('concatenates multiple strings', () => {
    expect('a'.concat('b', 'c', 'd')).toBe('abcd');
  });

  test('concatenates with empty string', () => {
    expect('hello'.concat('')).toBe('hello');
  });

  test('concat with no arguments returns same string', () => {
    expect('hello'.concat()).toBe('hello');
  });

  test('concat converts arguments to strings', () => {
    expect(''.concat(1, true, null)).toBe('1truenull');
  });

  test('empty string concat', () => {
    expect(''.concat('a', 'b')).toBe('ab');
  });
});
