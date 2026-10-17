/*---
description: ASI for throw statements including restricted production
features: [automatic-semicolon-insertion]
---*/

describe("ASI throw statement", () => {
  test("throw without semicolon", () => {
    const fn = () => { throw new Error("test") }
    expect(() => fn()).toThrow(Error);
  });

  test("throw error value without semicolon", () => {
    let caught
    try {
      throw new TypeError("bad type")
    } catch (e) {
      caught = e
    }
    expect(caught.message).toBe("bad type")
  });

  test("throw before closing brace", () => {
    expect(() => { throw new Error("brace") }).toThrow(Error);
  });

  // ES2026 §12.10.1: throw [no LineTerminator here] Expression. A line break
  // inside a template literal or a string's LineContinuation is part of that
  // token, not a LineTerminator between `throw` and the expression.
  test("multi-line template on the throw line is thrown", () => {
    let caught
    try {
      throw `a
b`;
    } catch (e) {
      caught = e
    }
    expect(caught).toBe("a\nb");
  });

  test("multi-line template without a semicolon is thrown", () => {
    let caught
    try {
      throw `a
b`
    } catch (e) {
      caught = e
    }
    expect(caught).toBe("a\nb");
  });

  test("string with a line continuation is thrown", () => {
    let caught
    try {
      throw "a\
b";
    } catch (e) {
      caught = e
    }
    expect(caught).toBe("ab");
  });
});
