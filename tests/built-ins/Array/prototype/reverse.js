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

  test('reverse mutates in place', () => {
    const arr = [1, 2, 3];
    const result = arr.reverse();
    expect(result).toBe(arr);
    expect(arr).toEqual([3, 2, 1]);
  });

  test('reverse with two elements', () => {
    const arr = [1, 2];
    arr.reverse();
    expect(arr).toEqual([2, 1]);
  });

  test('generic receiver reverses array-like in place', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const result = Array.prototype.reverse.call(obj);
    expect(result[0]).toBe('c');
    expect(result[1]).toBe('b');
    expect(result[2]).toBe('a');
  });

  test('reverse preserves holes in sparse array', () => {
    const arr = [1, , 3];
    arr.reverse();
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(3);
    expect(1 in arr).toBe(false);
    expect(arr[2]).toBe(1);
  });
});
