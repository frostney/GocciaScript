/*---
description: for...of with break statement
features: [for-of]
---*/

describe("for...of break", () => {
  test("break exits the loop early", () => {
    const result = [];
    for (const item of [1, 2, 3, 4, 5]) {
      if (item === 3) {
        break;
      }
      result.push(item);
    }
    expect(result).toEqual([1, 2]);
  });

  test("break in nested for...of", () => {
    const result = [];
    for (const outer of ["a", "b"]) {
      for (const inner of [1, 2, 3]) {
        if (inner === 2) {
          break;
        }
        result.push(outer + inner);
      }
    }
    expect(result).toEqual(["a1", "b1"]);
  });
});
