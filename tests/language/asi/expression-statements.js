/*---
description: ASI for expression statements
features: [automatic-semicolon-insertion]
---*/

describe("ASI expression statements", () => {
  test("function call without semicolon", () => {
    let x = 0;
    const inc = () => { x = x + 1 }
    inc()
    inc()
    expect(x).toBe(2);
  });

  test("assignment without semicolon", () => {
    let x = 0
    x = 10
    expect(x).toBe(10);
  });

  test("method call without semicolon", () => {
    const arr = [3, 1, 2]
    const sorted = arr.slice().sort((a, b) => a - b)
    expect(sorted[0]).toBe(1)
    expect(sorted[2]).toBe(3)
  });

  test("expression before closing brace", () => {
    let x = 0;
    const fn = () => { x = 42 }
    fn();
    expect(x).toBe(42);
  });
});
