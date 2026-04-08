/*---
description: ASI for return statements including restricted production
features: [automatic-semicolon-insertion]
---*/

describe("ASI return statement", () => {
  test("return value without semicolon", () => {
    const fn = () => { return 42 }
    expect(fn()).toBe(42);
  });

  test("return expression without semicolon", () => {
    const fn = (a, b) => { return a + b }
    expect(fn(3, 4)).toBe(7);
  });

  test("return without value and without semicolon", () => {
    let called = false
    const fn = () => { called = true; return }
    const result = fn()
    expect(called).toBe(true);
    expect(result).toBeUndefined();
  });

  test("restricted production: newline after return inserts semicolon", () => {
    // ES2026 section 12.10.1: return [no LineTerminator here] Expression
    // A newline after 'return' causes ASI to insert a semicolon,
    // so the function returns undefined.
    const fn = () => {
      return
      42
    }
    expect(fn()).toBeUndefined();
  });

  test("restricted production: return + newline + object returns undefined", () => {
    // Classic ASI pitfall: the object literal is NOT returned
    const fn = () => {
      return
      { value: 42 }
    }
    expect(fn()).toBeUndefined();
  });

  test("return on same line works normally", () => {
    const fn = () => { return { value: 42 } }
    expect(fn().value).toBe(42);
  });

  test("return before closing brace", () => {
    const fn = () => { return 100 }
    expect(fn()).toBe(100);
  });
});
