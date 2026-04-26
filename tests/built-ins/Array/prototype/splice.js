/*---
description: Array.prototype.splice (mutating)
features: [Array.prototype.splice]
---*/

describe("Array.prototype.splice", () => {
  test("remove elements", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(1, 2);
    expect(removed).toEqual([2, 3]);
    expect(arr).toEqual([1, 4, 5]);
  });

  test("insert elements", () => {
    const arr = [1, 4, 5];
    const removed = arr.splice(1, 0, 2, 3);
    expect(removed).toEqual([]);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test("replace elements", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(1, 2, "a", "b", "c");
    expect(removed).toEqual([2, 3]);
    expect(arr).toEqual([1, "a", "b", "c", 4, 5]);
  });

  test("negative start index", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(-2, 1);
    expect(removed).toEqual([4]);
    expect(arr).toEqual([1, 2, 3, 5]);
  });

  test("delete to end when no deleteCount", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(2);
    expect(removed).toEqual([3, 4, 5]);
    expect(arr).toEqual([1, 2]);
  });

  test("empty splice returns empty array", () => {
    const arr = [1, 2, 3];
    const removed = arr.splice(1, 0);
    expect(removed).toEqual([]);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("generic receiver removes and inserts in array-like", () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const removed = Array.prototype.splice.call(obj, 1, 1, 'x', 'y');
    expect(removed).toEqual(['b']);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('x');
    expect(obj[2]).toBe('y');
    expect(obj[3]).toBe('c');
    expect(obj.length).toBe(4);
  });

  test("splice returns sparse removed array", () => {
    const arr = [1, , 3];
    const removed = arr.splice(0, 3);
    expect(removed.length).toBe(3);
    expect(0 in removed).toBe(true);
    expect(1 in removed).toBe(false);
    expect(2 in removed).toBe(true);
  });

  test("throws TypeError when new length would exceed Number.MAX_SAFE_INTEGER", () => {
    const obj = { length: Number.MAX_SAFE_INTEGER };
    expect(() => Array.prototype.splice.call(obj, 0, 0, 1)).toThrow(TypeError);
  });

  test("throws RangeError when deleteCount exceeds engine MaxInt on huge receiver", () => {
    // splice(0, 2^40 - 100) on a length-2^40 receiver: NewLen would fit in
    // MaxInt (=100), but the delete count itself overflows the in-place
    // shift loop's Integer counter.  Reject up-front rather than wrapping.
    const obj = { length: 2 ** 40 };
    expect(() => Array.prototype.splice.call(obj, 0, 2 ** 40 - 100))
      .toThrow(RangeError);
  });

  test("shifts high-index source past MaxInt via sparse iteration", () => {
    // Receiver length 2^31 puts the highest valid index at 2^31 - 1 (=
    // MaxInt on 32-bit FPC builds).  splice(0, 1) shifts every index left
    // by 1: the property at 2^31 - 1 must move to 2^31 - 2 and the original
    // slot at 2^31 - 1 must be deleted as part of the trailing-range
    // cleanup.  The dense Integer shift+delete loops saturate at View.Len =
    // MaxInt - 1 and would silently leave both bugs in place; sparse
    // iteration handles them via Get64 / DeleteProperty.
    const obj = { length: 2 ** 31, [2 ** 31 - 1]: 'x' };
    const removed = Array.prototype.splice.call(obj, 0, 1);
    expect(removed.length).toBe(1);
    expect(obj.length).toBe(2 ** 31 - 1);
    expect(obj[2 ** 31 - 2]).toBe('x');
    expect((2 ** 31 - 1) in obj).toBe(false);
  });
});
