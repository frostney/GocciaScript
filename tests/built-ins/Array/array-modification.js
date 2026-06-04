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

test("setting length to 2**32 - 1 is valid and does not allocate holes", () => {
  const arr = [1, 2, 3];
  arr.length = 4294967295;
  expect(arr.length).toBe(4294967295);
  expect(arr[0]).toBe(1);
  expect(arr[2]).toBe(3);
  expect(4294967294 in arr).toBe(false);

  arr.length = 3;
  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[2]).toBe(3);
});

test("setting a high valid array index updates length without dense allocation", () => {
  const arr = [];
  arr[4294967294] = 7;

  expect(arr.length).toBe(4294967295);
  expect(arr[4294967294]).toBe(7);
  expect(Object.prototype.hasOwnProperty.call(arr, "4294967294")).toBe(true);
});

test("defining a high valid array index updates length", () => {
  const arr = [];
  Object.defineProperty(arr, "4294967294", {
    value: "last",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  expect(arr.length).toBe(4294967295);
  expect(arr[4294967294]).toBe("last");
});

test("2**32 - 1 is a plain property name, not an array index", () => {
  const arr = [];
  arr[4294967295] = "plain";

  expect(arr.length).toBe(0);
  expect(arr[4294967295]).toBe("plain");
  expect(Object.prototype.hasOwnProperty.call(arr, "4294967295")).toBe(true);
});

test("non-writable length rejects high indices that would grow the array", () => {
  const arr = [];
  Object.defineProperty(arr, "length", { writable: false });

  expect(() => {
    arr[4294967294] = 7;
  }).toThrow(TypeError);
  expect(arr.length).toBe(0);
  expect(Object.prototype.hasOwnProperty.call(arr, "4294967294")).toBe(false);
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

test("setting length does not walk the prototype chain", () => {
  const traps = [];
  const array = [1, 2, 3];
  Object.setPrototypeOf(array, new Proxy(Array.prototype, {
    getOwnPropertyDescriptor(t, pk) {
      traps.push("gOPD:" + String(pk));
      return Reflect.getOwnPropertyDescriptor(t, pk);
    }
  }));
  array.length = 0;
  expect(traps.length).toBe(0);
  expect(array.length).toBe(0);
});

test("setting length to invalid value throws RangeError without walking prototype chain", () => {
  const traps = [];
  const array = [1, 2, 3];
  Object.setPrototypeOf(array, new Proxy(Array.prototype, {
    getOwnPropertyDescriptor(t, pk) {
      traps.push("gOPD:" + String(pk));
      return Reflect.getOwnPropertyDescriptor(t, pk);
    }
  }));
  expect(() => { array.length = -1; }).toThrow(RangeError);
  expect(traps.length).toBe(0);
  expect(array.length).toBe(3);
});

test("setting length below max array index does not range check in bytecode", () => {
  const array = [0, 1, 2];

  array[4294967294] = 4294967294;
  array.length = 2;

  expect(array[0]).toBe(0);
  expect(array[1]).toBe(1);
  expect(array[2]).toBeUndefined();
  expect(array[4294967294]).toBeUndefined();
});
