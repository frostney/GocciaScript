/*---
description: Array.of
features: [Array.of]
---*/

describe("Array.of", () => {
  test("creates array from arguments", () => {
    expect(Array.of(1, 2, 3)).toEqual([1, 2, 3]);
  });

  test("single argument creates single-element array", () => {
    expect(Array.of(7)).toEqual([7]);
  });

  test("no arguments creates empty array", () => {
    expect(Array.of()).toEqual([]);
  });

  test("mixed types", () => {
    const arr = Array.of(1, "two", true, null);
    expect(arr.length).toBe(4);
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe("two");
    expect(arr[2]).toBe(true);
    expect(arr[3]).toBe(null);
  });
});
