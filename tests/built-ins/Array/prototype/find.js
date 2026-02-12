describe('Array.prototype.find', () => {
  test('returns the first element that satisfies the predicate', () => {
    const arr = [1, 2, 3, 4, 5];
    expect(arr.find((x) => x > 3)).toBe(4);
  });

  test('returns undefined if no element satisfies the predicate', () => {
    const arr = [1, 2, 3];
    expect(arr.find((x) => x > 10)).toBe(undefined);
  });

  test('passes element, index, and array to callback', () => {
    const arr = [10, 20, 30];
    let foundIndex = -1;
    arr.find((elem, idx) => {
      if (elem === 20) {
        foundIndex = idx;
        return true;
      }
      return false;
    });
    expect(foundIndex).toBe(1);
  });

  test('works with objects in array', () => {
    const arr = [{ name: 'a' }, { name: 'b' }, { name: 'c' }];
    const result = arr.find((item) => item.name === 'b');
    expect(result.name).toBe('b');
  });
});
