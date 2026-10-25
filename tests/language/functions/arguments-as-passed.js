/*---
description: A callee receives its arguments in order however it reads them, including through a proper tail call that passes them on rearranged
features: [function-call]
---*/

test("a tail call that rotates its arguments passes each to the right parameter", () => {
  const rotate = (n, a, b, c) => {
    if (n === 0) {
      return [a, b, c].join("");
    }
    return rotate(n - 1, b, c, a);
  };

  expect(rotate(0, "a", "b", "c")).toBe("abc");
  expect(rotate(1, "a", "b", "c")).toBe("bca");
  expect(rotate(2, "a", "b", "c")).toBe("cab");
  expect(rotate(3000, "a", "b", "c")).toBe("abc");
});

test("a method tail call passes its receiver and arguments on", () => {
  class Counter {
    constructor(base) {
      this.base = base;
    }

    sum(n, total, step) {
      return n === 0 ? this.base + total : this.sum(n - 1, total + step, step);
    }
  }

  expect(new Counter(5).sum(3000, 0, 2)).toBe(6005);
});

test("arguments past the parameters are dropped and missing ones are undefined", () => {
  const pair = (a, b) => [a, b];

  expect(pair(1, 2, 3, 4)).toEqual([1, 2]);
  expect(pair(1)).toEqual([1, undefined]);
  expect(pair()).toEqual([undefined, undefined]);
});

test("default and rest parameters read the arguments as passed", () => {
  const withDefault = (a, b = a + 1) => [a, b];
  const withRest = (first, ...rest) => [first, rest];

  expect(withDefault(1)).toEqual([1, 2]);
  expect(withDefault(1, 5)).toEqual([1, 5]);
  expect(withDefault(1, undefined)).toEqual([1, 2]);
  expect(withRest(1, 2, 3)).toEqual([1, [2, 3]]);
  expect(withRest()).toEqual([undefined, []]);
});

test("a callback a built-in calls receives its arguments in order", () => {
  expect([10, 20, 30].map((value, index) => value + index)).toEqual([10, 21, 32]);
  expect([1, 2, 3].reduce((sum, value, index) => sum + value * index, 0)).toBe(8);
});
