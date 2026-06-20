/*---
description: Array.prototype.reduceRight reduces an array from right to left
features: [Array.prototype.reduceRight]
---*/

test("Array.prototype.reduceRight reduces an array from right to left", () => {
  const result = [1, 2, 3, 4].reduceRight((acc, value) => acc - value);
  expect(result).toBe(-2);
});

test("reduceRight with initial value visits every present element from the end", () => {
  const calls = [];
  const arr = [10, 20, 30];
  const callbackHolder = {
    callback(acc, value, index, object) {
      "use strict";
      expect(this).toBe(undefined);
      expect(object).toBe(arr);
      calls.push({ acc, value, index, length: object.length });
      return acc + value;
    },
  };
  const result = arr.reduceRight(callbackHolder.callback, 0);

  expect(result).toBe(60);
  expect(calls.length).toBe(3);
  expect(calls[0].acc).toBe(0);
  expect(calls[0].value).toBe(30);
  expect(calls[0].index).toBe(2);
  expect(calls[1].index).toBe(1);
  expect(calls[2].index).toBe(0);
  expect(calls[2].length).toBe(3);
});

test("reduceRight without initial value seeds from last present element", () => {
  const indices = [];
  const result = [, , 1, , 3].reduceRight((acc, value, index) => {
    indices.push(index);
    return acc - value;
  });

  expect(result).toBe(2);
  expect(indices).toEqual([2]);
});

test("reduceRight on a single present element without initial value does not call callback", () => {
  let called = false;
  const result = [, 42, ,].reduceRight(() => {
    called = true;
    return 0;
  });

  expect(result).toBe(42);
  expect(called).toBe(false);
});

test("reduceRight on empty or all-hole arrays without initial value throws TypeError", () => {
  expect(() => [].reduceRight((acc, value) => acc + value)).toThrow(TypeError);
  expect(() => [, , ,].reduceRight((acc, value) => acc + value)).toThrow(TypeError);
});

test("reduceRight requires a callable callback after coercing the receiver", () => {
  expect(() => Array.prototype.reduceRight.call(null, () => 0)).toThrow(TypeError);
  expect(() => Array.prototype.reduceRight.call(undefined, () => 0)).toThrow(TypeError);
  expect(() => [1, 2].reduceRight()).toThrow(TypeError);
  expect(() => [1, 2].reduceRight(null)).toThrow(TypeError);
});

test("reduceRight is generic over array-like receivers", () => {
  const arrayLike = { 0: "a", 2: "c", length: 3 };
  const result = Array.prototype.reduceRight.call(arrayLike, (acc, value, index) => {
    return acc + index + value;
  }, "");

  expect(result).toBe("2c0a");
});

test("reduceRight sees inherited properties through holes", () => {
  const proto = { 1: "b" };
  const arrayLike = Object.create(proto);
  arrayLike[0] = "a";
  arrayLike[2] = "c";
  arrayLike.length = 3;

  const values = [];
  const result = Array.prototype.reduceRight.call(arrayLike, (acc, value, index) => {
    values.push(index + value);
    return acc + value;
  }, "");

  expect(result).toBe("cba");
  expect(values).toEqual(["2c", "1b", "0a"]);
});

test("reduceRight skips elements deleted before they are visited and ignores appended elements", () => {
  const arr = [1, 2, 3];
  const seen = [];
  const result = arr.reduceRight((acc, value, index, object) => {
    seen.push(index);
    if (index === 2) {
      object.push(4);
      delete object[1];
    }
    return acc + value;
  }, 0);

  expect(result).toBe(4);
  expect(seen).toEqual([2, 0]);
  expect(arr.length).toBe(4);
});

test("reduceRight has correct name and length", () => {
  expect(Array.prototype.reduceRight.name).toBe("reduceRight");
  expect(Array.prototype.reduceRight.length).toBe(1);
});
