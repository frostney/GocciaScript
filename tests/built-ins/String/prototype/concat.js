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
});
