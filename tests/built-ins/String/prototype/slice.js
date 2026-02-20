/*---
description: String.prototype.slice works correctly
features: [String.prototype.slice]
---*/

describe("String.prototype.slice", () => {
  test("extracts with start and end", () => {
    expect("hello world".slice(0, 5)).toBe("hello");
    expect("hello world".slice(6)).toBe("world");
  });

  test("negative start counts from end", () => {
    expect("hello world".slice(-5)).toBe("world");
  });

  test("negative start and end", () => {
    expect("hello world".slice(-5, -1)).toBe("worl");
  });

  test("start >= end returns empty string", () => {
    expect("hello".slice(3, 1)).toBe("");
    expect("hello".slice(5, 5)).toBe("");
  });

  test("no arguments returns full string", () => {
    expect("hello".slice()).toBe("hello");
  });

  test("start beyond length returns empty string", () => {
    expect("hello".slice(100)).toBe("");
  });

  test("negative start beyond length starts from 0", () => {
    expect("hello".slice(-100)).toBe("hello");
  });

  test("end beyond length clamps to length", () => {
    expect("hello".slice(0, 100)).toBe("hello");
  });

  test("empty string", () => {
    expect("".slice(0)).toBe("");
    expect("".slice(0, 5)).toBe("");
  });
});
