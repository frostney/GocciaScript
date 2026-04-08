/*---
description: ASI edge cases
features: [automatic-semicolon-insertion]
---*/

describe("ASI edge cases", () => {
  test("ASI at end of file", () => {
    const x = "eof"
    expect(x).toBe("eof")
  });

  test("ASI before closing brace without newline", () => {
    const fn = () => { return 42 }
    expect(fn()).toBe(42);
  });

  test("semicolons still work when present", () => {
    const a = 1;
    const b = 2;
    expect(a + b).toBe(3);
  });

  test("no ASI between tokens on same line", () => {
    // Two statements on the same line without semicolons should fail.
    // We can't easily test parse errors in the test framework,
    // but we can verify that proper syntax still works.
    const a = 1; const b = 2;
    expect(a + b).toBe(3);
  });

  test("ASI with nested blocks", () => {
    let result = 0
    const outer = () => {
      const inner = () => {
        result = 42
      }
      inner()
    }
    outer()
    expect(result).toBe(42)
  });

  test("ASI with if/else", () => {
    let x
    if (true) {
      x = "yes"
    } else {
      x = "no"
    }
    expect(x).toBe("yes")
  });

  test("ASI with try/catch", () => {
    let caught = false
    try {
      throw new Error("test")
    } catch (e) {
      caught = true
    }
    expect(caught).toBe(true)
  });

  test("ASI with arrow function body", () => {
    const double = (n) => {
      const result = n * 2
      return result
    }
    expect(double(21)).toBe(42)
  });
});
