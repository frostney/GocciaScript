/*---
description: Array.prototype.push adds elements to end of array
features: [Array.prototype.push]
---*/

describe("Array.prototype.push", () => {
  test("adds element and returns new length", () => {
    const arr = [1, 2];
    const newLength = arr.push(3);
    expect(newLength).toBe(3);
    expect(arr.length).toBe(3);
    expect(arr[2]).toBe(3);
  });

  test("push multiple elements", () => {
    const arr = [1];
    const len = arr.push(2, 3, 4);
    expect(len).toBe(4);
    expect(arr).toEqual([1, 2, 3, 4]);
  });

  test("push with no arguments returns current length", () => {
    const arr = [1, 2, 3];
    expect(arr.push()).toBe(3);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("push on empty array", () => {
    const arr = [];
    expect(arr.push(1)).toBe(1);
    expect(arr).toEqual([1]);
  });

  test("push null and undefined", () => {
    const arr = [];
    arr.push(null, undefined);
    expect(arr).toEqual([null, undefined]);
  });

  test("generic receiver appends to array-like", () => {
    const obj = { 0: 'a', length: 1 };
    const newLen = Array.prototype.push.call(obj, 'b');
    expect(newLen).toBe(2);
    expect(obj[1]).toBe('b');
    expect(obj.length).toBe(2);
  });

  test("throws TypeError when new length would exceed Number.MAX_SAFE_INTEGER", () => {
    const obj = { length: Number.MAX_SAFE_INTEGER };
    expect(() => Array.prototype.push.call(obj, 1)).toThrow(TypeError);
  });

  test("throws TypeError when starting length is already past MAX_SAFE_INTEGER and pushing one element", () => {
    const obj = { length: Number.MAX_SAFE_INTEGER + 1 };
    expect(() => Array.prototype.push.call(obj, 1)).toThrow(TypeError);
  });

  test("with no args returns the receiver's full length even when > 2^31", () => {
    const huge = { length: 4294967290 };
    const result = Array.prototype.push.call(huge);
    expect(result).toBe(4294967290);
    expect(huge.length).toBe(4294967290);
  });
});

// ES2026 §23.1.3.23 Array.prototype.push ends with Set(O, "length", len,
// true); on an Array that is ArraySetLength (§10.4.2.4), which accepts any
// length up to 2^32 - 1 and throws RangeError past it.
describe("push on an array longer than 2 ** 31 - 1", () => {
  test("appends the items and sets the length", () => {
    const arr = [];
    arr.length = 2 ** 31;
    expect(arr.push("x", "y")).toBe(2 ** 31 + 2);
    expect(arr.length).toBe(2 ** 31 + 2);
    expect(arr[2 ** 31]).toBe("x");
    expect(arr[2 ** 31 + 1]).toBe("y");
  });

  test("past 2 ** 32 - 1 sets the element, then throws RangeError for the length", () => {
    const arr = [];
    arr.length = 2 ** 32 - 1;
    expect(() => arr.push("x")).toThrow(RangeError);
    expect(arr.length).toBe(2 ** 32 - 1);
    expect(arr[2 ** 32 - 1]).toBe("x");
  });

  test("with a read-only length throws TypeError and adds nothing", () => {
    const arr = [];
    arr.length = 2 ** 31;
    Object.defineProperty(arr, "length", { writable: false });
    expect(() => arr.push(1)).toThrow(TypeError);
    expect(arr.length).toBe(2 ** 31);
    expect(Object.hasOwn(arr, 2 ** 31)).toBe(false);
  });

  test("on an array-like up to 2 ** 53 - 1 sets the length as a number", () => {
    const obj = { length: 2 ** 53 - 3 };
    expect(Array.prototype.push.call(obj, "a", "b")).toBe(2 ** 53 - 1);
    expect(obj.length).toBe(2 ** 53 - 1);
    expect(obj[2 ** 53 - 3]).toBe("a");
    expect(obj[2 ** 53 - 2]).toBe("b");
  });
});
