/*---
description: for...of with continue statement
features: [for-of]
---*/

describe("for...of continue", () => {
  test("continue skips current iteration", () => {
    const result = [];
    for (const item of [1, 2, 3, 4, 5]) {
      if (item === 3) {
        continue;
      }
      result.push(item);
    }
    expect(result).toEqual([1, 2, 4, 5]);
  });

  test("continue skips even values", () => {
    const result = [];
    for (const x of [1, 2, 3, 4]) {
      if (x === 2) continue;
      result.push(x);
    }
    expect(result).toEqual([1, 3, 4]);
  });

  test("continue in nested for...of only affects inner loop", () => {
    const result = [];
    for (const outer of ["a", "b"]) {
      for (const inner of [1, 2, 3]) {
        if (inner === 2) {
          continue;
        }
        result.push(outer + inner);
      }
    }
    expect(result).toEqual(["a1", "a3", "b1", "b3"]);
  });

  test("continue with all iterations skipped", () => {
    const result = [];
    for (const x of [1, 2, 3]) {
      continue;
      result.push(x);
    }
    expect(result).toEqual([]);
  });

  test("continue on last iteration", () => {
    const result = [];
    for (const x of [1, 2, 3]) {
      if (x === 3) continue;
      result.push(x);
    }
    expect(result).toEqual([1, 2]);
  });

  test("continue on first iteration", () => {
    const result = [];
    for (const x of [1, 2, 3]) {
      if (x === 1) continue;
      result.push(x);
    }
    expect(result).toEqual([2, 3]);
  });

  test("continue inside switch inside for-of", () => {
    const result = [];
    for (const x of [1, 2, 3, 4]) {
      switch (x) {
        case 2:
          continue;
        case 3:
          continue;
        default:
          result.push(x);
      }
    }
    expect(result).toEqual([1, 4]);
  });

  test("continue with destructuring binding", () => {
    const pairs = [[1, "a"], [2, "b"], [3, "c"]];
    const result = [];
    for (const [n, s] of pairs) {
      if (n === 2) continue;
      result.push(s);
    }
    expect(result).toEqual(["a", "c"]);
  });

  test("continue and break in same loop", () => {
    const result = [];
    for (const x of [1, 2, 3, 4, 5]) {
      if (x === 2) continue;
      if (x === 4) break;
      result.push(x);
    }
    expect(result).toEqual([1, 3]);
  });

  test("continue with custom iterator", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i++;
            return i <= 5 ? { value: i, done: false } : { done: true };
          }
        };
      }
    };

    const result = [];
    for (const x of iterable) {
      if (x % 2 === 0) continue;
      result.push(x);
    }
    expect(result).toEqual([1, 3, 5]);
  });
});
