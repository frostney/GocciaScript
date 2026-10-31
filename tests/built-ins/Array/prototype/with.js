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

  test("omitted arguments replace index zero with undefined in a copy", () => {
    const source = [1, 2];
    const result = source.with();
    expect(result).toEqual([undefined, 2]);
    expect(result).not.toBe(source);
    expect(Object.hasOwn(result, "0")).toBe(true);
    expect(source).toEqual([1, 2]);
  });

  test("omitted value uses undefined after converting the index", () => {
    expect([1, 2].with(0)).toEqual([undefined, 2]);
    expect([1, 2].with(-1)).toEqual([1, undefined]);
    expect([1, 2].with(undefined)).toEqual([undefined, 2]);
    expect([1, 2].with({ valueOf: () => 1.9 })).toEqual([1, undefined]);
    expect([1, 2].with(NaN)).toEqual([undefined, 2]);
    expect([1, 2].with(-0.5)).toEqual([undefined, 2]);
  });

  test("omitted value still checks bounds and propagates index conversion errors", () => {
    let caught;
    try {
      [].with();
    } catch (error) {
      caught = error;
    }
    expect(caught).toBeInstanceOf(RangeError);
    expect(() => [].with(0)).toThrow(RangeError);
    expect(() => [1, 2].with(2)).toThrow(RangeError);
    expect(() => [1, 2].with(-3)).toThrow(RangeError);
    expect(() => [1, 2].with(Infinity)).toThrow(RangeError);
    expect(() => [1, 2].with(-Infinity)).toThrow(RangeError);
    expect(() => [1, 2].with(Symbol())).toThrow(TypeError);
    const error = new Error("index conversion");
    expect(() => [1, 2].with({ valueOf: () => { throw error; } })).toThrow(error);
  });

  test("omitted arguments work on array-like receivers", () => {
    const source = { 0: "a", 1: "b", length: 2 };
    expect(Array.prototype.with.call(source)).toEqual([undefined, "b"]);
    expect(Array.prototype.with.call(source, -1)).toEqual(["a", undefined]);
    expect(() => Array.prototype.with.call({ length: 0 })).toThrow(RangeError);
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

  test("generic receiver creates copy with replaced index from array-like", () => {
    const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const result = Array.prototype.with.call(arrayLike, 1, 'x');
    expect(result).toEqual(['a', 'x', 'c']);
  });
});
