describe('Array.prototype.concat', () => {
  test('concatenates two arrays', () => {
    const result = [1, 2].concat([3, 4]);
    expect(result.length).toBe(4);
    expect(result[0]).toBe(1);
    expect(result[3]).toBe(4);
  });

  test('concatenates multiple arrays', () => {
    const result = [1].concat([2], [3], [4]);
    expect(result.length).toBe(4);
  });

  test('adds non-array values directly', () => {
    const result = [1, 2].concat(3, 4);
    expect(result.length).toBe(4);
    expect(result[2]).toBe(3);
  });

  test('does not modify original arrays', () => {
    const arr1 = [1, 2];
    const arr2 = [3, 4];
    const result = arr1.concat(arr2);
    expect(arr1.length).toBe(2);
    expect(arr2.length).toBe(2);
    expect(result.length).toBe(4);
  });
});
