/*---
description: ASI for variable declarations
features: [automatic-semicolon-insertion]
---*/

describe("ASI variable declarations", () => {
  test("const declaration without semicolon", () => {
    const x = 42
    expect(x).toBe(42);
  });

  test("let declaration without semicolon", () => {
    let x = 10
    expect(x).toBe(10);
  });

  test("multiple declarations without semicolons", () => {
    const a = 1
    const b = 2
    const c = a + b
    expect(c).toBe(3);
  });

  test("let without initializer followed by newline", () => {
    let x
    expect(x).toBeUndefined();
  });

  test("destructuring declaration without semicolon", () => {
    const [a, b] = [1, 2]
    expect(a).toBe(1);
    expect(b).toBe(2);
  });

  test("object destructuring without semicolon", () => {
    const { x, y } = { x: 10, y: 20 }
    expect(x).toBe(10);
    expect(y).toBe(20);
  });

  test("multi-variable declaration without semicolon", () => {
    const a = 1, b = 2
    expect(a).toBe(1);
    expect(b).toBe(2);
  });

  test("declaration before closing brace inserts semicolon", () => {
    const fn = () => { const x = 99; return x }
    expect(fn()).toBe(99);
  });

  test("declaration at end of file", () => {
    const x = "eof"
    expect(x).toBe("eof")
  });
});
