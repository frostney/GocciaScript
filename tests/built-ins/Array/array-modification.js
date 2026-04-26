/*---
description: Array modification
features: [Array]
---*/

test("modify existing values in array", () => {
  const arr = [1, 2, 3];

  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBe(3);

  arr[0] = 4;
  expect(arr[0]).toBe(4);

  arr[1] = 5;
  expect(arr[1]).toBe(5);

  arr[2] = 6;
  expect(arr[2]).toBe(6);
});

test("modify existing values in sparse array", () => {
  const arr = [1, , 3];

  expect(arr[0]).toBe(1);
  expect(arr[1]).toBeUndefined();
  expect(arr[2]).toBe(3);
  expect(arr.length).toBe(3);

  arr[1] = 10;
  expect(arr[1]).toBe(10);
  expect(arr.length).toBe(3);
});

test("add new values to array", () => {
  const arr = [1, 2, 3];

  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBe(3);

  arr[3] = 4;
  expect(arr[3]).toBe(4);
  expect(arr.length).toBe(4);
});

test("delete values from array into sparse array", () => {
  const arr = [1, 2, 3];

  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBe(3);

  delete arr[1];
  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBeUndefined();
  expect(arr[2]).toBe(3);
});

test("setting length to a smaller value truncates the array", () => {
  const arr = [1, 2, 3];
  arr.length = 0;
  expect(arr.length).toBe(0);
  expect(0 in arr).toBe(false);
  expect(1 in arr).toBe(false);
  expect(2 in arr).toBe(false);
});

test("setting length to a larger value extends the array with holes", () => {
  const arr = [];
  arr.length = 5;
  expect(arr.length).toBe(5);
  expect(0 in arr).toBe(false);
  expect(1 in arr).toBe(false);
  expect(2 in arr).toBe(false);
  expect(3 in arr).toBe(false);
  expect(4 in arr).toBe(false);
});

test("setting length to 2**32 throws RangeError", () => {
  expect(() => {
    [].length = 4294967296;
  }).toThrow(RangeError);
});

test("setting length to 2**32 + 1 throws RangeError", () => {
  expect(() => {
    [].length = 4294967297;
  }).toThrow(RangeError);
});

test("setting length to a negative value throws RangeError", () => {
  expect(() => {
    [].length = -1;
  }).toThrow(RangeError);
});

test("setting length to a non-integer throws RangeError", () => {
  expect(() => {
    [].length = 1.5;
  }).toThrow(RangeError);
});

test("setting length to NaN throws RangeError", () => {
  expect(() => {
    [].length = NaN;
  }).toThrow(RangeError);
});
