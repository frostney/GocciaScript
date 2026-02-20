describe("Array.prototype.with", () => {
  test("returns a copy with one element replaced", () => {
    const arr = [1, 2, 3, 4, 5];
    const result = arr.with(2, 99);
    expect(result).toEqual([1, 2, 99, 4, 5]);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test("does not modify the original array", () => {
    const arr = [1, 2, 3];
    const result = arr.with(0, 10);
    expect(arr[0]).toBe(1);
    expect(result[0]).toBe(10);
    expect(result).not.toBe(arr);
  });

  test("negative index counts from end", () => {
    expect([1, 2, 3].with(-1, 99)).toEqual([1, 2, 99]);
    expect([1, 2, 3].with(-2, 99)).toEqual([1, 99, 3]);
  });

  test("replace first element", () => {
    expect([1, 2, 3].with(0, 99)).toEqual([99, 2, 3]);
  });

  test("replace last element", () => {
    expect([1, 2, 3].with(2, 99)).toEqual([1, 2, 99]);
  });

  test("out of bounds throws RangeError", () => {
    expect(() => [1, 2, 3].with(5, 99)).toThrow(RangeError);
    expect(() => [1, 2, 3].with(-4, 99)).toThrow(RangeError);
  });
});
