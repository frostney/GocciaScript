/*---
description: for...of with destructuring patterns
features: [for-of]
---*/

describe("for...of destructuring", () => {
  test("array destructuring in for...of", () => {
    const pairs = [[1, "a"], [2, "b"], [3, "c"]];
    const nums = [];
    const chars = [];
    for (const [num, char] of pairs) {
      nums.push(num);
      chars.push(char);
    }
    expect(nums).toEqual([1, 2, 3]);
    expect(chars).toEqual(["a", "b", "c"]);
  });

  test("object destructuring in for...of", () => {
    const items = [{ name: "Alice", age: 30 }, { name: "Bob", age: 25 }];
    const names = [];
    for (const { name } of items) {
      names.push(name);
    }
    expect(names).toEqual(["Alice", "Bob"]);
  });

  test("nested destructuring in for...of", () => {
    const data = [[1, [2, 3]], [4, [5, 6]]];
    const results = [];
    for (const [a, [b, c]] of data) {
      results.push(a + b + c);
    }
    expect(results).toEqual([6, 15]);
  });
});
