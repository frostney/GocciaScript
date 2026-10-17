/*---
description: ASI for return statements including restricted production
features: [automatic-semicolon-insertion, compat-label]
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

  test("restricted production: return + newline + labeled block returns undefined", () => {
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

  // ES2026 §12.10.1: the restricted production looks for a LineTerminator
  // between `return` and the next token. A line break inside a template
  // literal or a string's LineContinuation is part of that token.
  test("multi-line template on the return line is returned", () => {
    const fn = () => {
      return `a
b`;
    }
    expect(fn()).toBe("a\nb");
  });

  test("multi-line template without a semicolon is returned", () => {
    const fn = () => {
      return `a
`
    }
    expect(fn()).toBe("a\n");
  });

  test("template starting with a line break is the start of the expression", () => {
    const c = "a"
    const fn = () => {
      return `
` + c;
    }
    expect(fn()).toBe("\na");
  });

  test("template with a line break after a substitution is returned", () => {
    const fn = () => {
      return `${"a"}
`;
    }
    expect(fn()).toBe("a\n");
  });

  test("string with a line continuation is returned", () => {
    const fn = () => {
      return "a\
b";
    }
    expect(fn()).toBe("ab");
  });

  test("multi-line comment after return still ends the statement", () => {
    const fn = () => {
      return /*
      */ 1;
    }
    expect(fn()).toBeUndefined();
  });
});
