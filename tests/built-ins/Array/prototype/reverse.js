describe('Array.prototype.reverse', () => {
  test('reverses array in place', () => {
    const arr = [1, 2, 3];
    arr.reverse();
    expect(arr[0]).toBe(3);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(1);
  });

  test('returns the reversed array', () => {
    const arr = [1, 2, 3];
    const result = arr.reverse();
    expect(result[0]).toBe(3);
  });

  test('handles single element', () => {
    const arr = [42];
    arr.reverse();
    expect(arr[0]).toBe(42);
  });

  test('handles empty array', () => {
    const arr = [];
    arr.reverse();
    expect(arr.length).toBe(0);
  });
});
