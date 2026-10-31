/*---
description: Array.prototype.toSpliced
features: [Array.prototype.toSpliced]
---*/

test("Array.prototype.toSpliced with numbers", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.toSpliced(2, 2, 6, 7)).toEqual([1, 2, 6, 7, 5]);
});

test("Array.prototype.toSpliced has the correct method length", () => {
  expect(Array.prototype.toSpliced.length).toBe(2);
});

test("Array.prototype.toSpliced with strings", () => {
  const months = ["Jan", "Mar", "Apr", "May"];

  // Inserting an element at index 1
  const months2 = months.toSpliced(1, 0, "Feb");
  expect(months2).toEqual(["Jan", "Feb", "Mar", "Apr", "May"]);

  // Deleting two elements starting from index 2
  const months3 = months2.toSpliced(2, 2);
  expect(months3).toEqual(["Jan", "Feb", "May"]);

  // Replacing one element at index 1 with two new elements
  const months4 = months3.toSpliced(1, 1, "Feb", "Mar");
  expect(months4).toEqual(["Jan", "Feb", "Mar", "May"]);

  // Original array is not modified
  expect(months).toEqual(["Jan", "Mar", "Apr", "May"]);
});

test("Array.prototype.toSpliced with sparse array", () => {
  const arr = [1, , 3, 4, , 6];
  expect(arr.toSpliced(1, 2)).toEqual([1, 4, undefined, 6]);
});

test("Array.prototype.toSpliced with empty array", () => {
  const arr = [];
  expect(arr.toSpliced(0, 0, 1)).toEqual([1]);
});

test("generic receiver creates spliced copy from array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  const result = Array.prototype.toSpliced.call(arrayLike, 1, 1, 'x');
  expect(result).toEqual(['a', 'x', 'c']);
});

test("toSpliced throws TypeError when newLen would exceed Number.MAX_SAFE_INTEGER", () => {
  const obj = { length: Number.MAX_SAFE_INTEGER };
  expect(() => Array.prototype.toSpliced.call(obj, 0, 0, 1)).toThrow(TypeError);
});

test("toSpliced throws RangeError when newLen exceeds 2**32 - 1", () => {
  const obj = { length: 2 ** 32 };
  expect(() => Array.prototype.toSpliced.call(obj, 0, 0)).toThrow(RangeError);
});

test("toSpliced copies sparse tail after huge skipCount when result is small", () => {
  const obj = {
    length: 2 ** 40,
    [2 ** 40 - 2]: "tail-a",
    [2 ** 40 - 1]: "tail-b",
  };
  const result = Array.prototype.toSpliced.call(obj, 0, 2 ** 40 - 2);

  expect(result.length).toBe(2);
  expect(result[0]).toBe("tail-a");
  expect(result[1]).toBe("tail-b");
});

test("toSpliced picks up high-index source past MaxInt via sparse iteration", () => {
  // Receiver length 2^31 puts the highest valid index at 2^31 - 1 (= MaxInt
  // on 32-bit FPC builds).  The dense Integer-indexed source loop saturates
  // at View.Len = MaxInt - 1 and would silently drop the property at index
  // 2^31 - 1; the sparse-iteration path enumerates it via Get64.
  const obj = { length: 2 ** 31, [2 ** 31 - 1]: 'x' };
  const result = Array.prototype.toSpliced.call(obj, 0, 2 ** 31 - 1);
  expect(result.length).toBe(1);
  expect(result[0]).toBe('x');
});

describe.runIf(typeof Goccia !== "undefined")("toSpliced across a collection", () => {
  // Reuses the memory a wrongly collected object leaves behind.
  const churn = () => Array.from({ length: 2000 }, (_, i) => [i, { i }]);

  test("keeps the result while an element getter collects", () => {
    const source = {
      length: 4,
      0: "a",
      get 1() {
        Goccia.gc();
        churn();
        return "b";
      },
      2: "c",
      get 3() {
        Goccia.gc();
        churn();
        return "d";
      },
    };
    expect(Array.prototype.toSpliced.call(source, 2, 1, "x", "y")).toEqual([
      "a", "b", "x", "y", "d",
    ]);
  });

  test("keeps the result on the sparse path while an element getter collects", () => {
    const source = {
      length: 2 ** 31 + 2,
      0: "a",
      get [2 ** 31 + 1]() {
        Goccia.gc();
        churn();
        return "z";
      },
    };
    const result = Array.prototype.toSpliced.call(source, 1, 2 ** 31 - 2);
    expect(result.length).toBe(4);
    expect(result[0]).toBe("a");
    expect(result[3]).toBe("z");
  });
});

test("toSpliced distinguishes omitted start and skipCount from undefined", () => {
  const source = [1, 2, 3];
  const copy = source.toSpliced();
  expect(copy).toEqual(source);
  expect(copy).not.toBe(source);
  expect(source.toSpliced(undefined)).toEqual([]);
  expect(source.toSpliced(1)).toEqual([1]);
  expect(source.toSpliced(1, undefined)).toEqual(source);
  expect(source).toEqual([1, 2, 3]);
});
