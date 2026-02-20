/*---
description: Array.prototype.at
features: [Array.prototype.at]
---*/

describe("Array.prototype.at", () => {
  test("positive index", () => {
    const arr = ["a", "b", "c", "d"];
    expect(arr.at(0)).toBe("a");
    expect(arr.at(1)).toBe("b");
    expect(arr.at(3)).toBe("d");
  });

  test("negative index", () => {
    const arr = ["a", "b", "c", "d"];
    expect(arr.at(-1)).toBe("d");
    expect(arr.at(-2)).toBe("c");
    expect(arr.at(-4)).toBe("a");
  });

  test("out of bounds returns undefined", () => {
    const arr = [1, 2, 3];
    expect(arr.at(10)).toBe(undefined);
    expect(arr.at(-10)).toBe(undefined);
  });

  test("at with 0 returns first element", () => {
    expect([10, 20, 30].at(0)).toBe(10);
  });

  test("at on empty array returns undefined", () => {
    expect([].at(0)).toBe(undefined);
    expect([].at(-1)).toBe(undefined);
  });
});
