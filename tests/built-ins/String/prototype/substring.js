/*---
description: String.prototype.substring works correctly
features: [String.prototype.substring]
---*/

describe("String.prototype.substring", () => {
  test("extracts with start and end", () => {
    expect("hello world".substring(0, 5)).toBe("hello");
    expect("hello world".substring(6)).toBe("world");
    expect("hello world".substring(0)).toBe("hello world");
  });

  test("swaps start and end when start > end", () => {
    expect("hello".substring(3, 1)).toBe("el");
  });

  test("negative values treated as 0", () => {
    expect("hello".substring(-3)).toBe("hello");
    expect("hello".substring(-3, 3)).toBe("hel");
  });

  test("values beyond length clamped", () => {
    expect("hello".substring(0, 100)).toBe("hello");
    expect("hello".substring(100)).toBe("");
  });

  test("empty string", () => {
    expect("".substring(0)).toBe("");
    expect("".substring(0, 0)).toBe("");
  });
});
