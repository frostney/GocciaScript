/*---
description: ASI for yield expressions including the restricted production
features: [automatic-semicolon-insertion, generators]
---*/

describe("ASI yield expression", () => {
  test("yield value without semicolon", () => {
    const obj = {
      *gen() {
        yield 1
        yield 2
      }
    }
    expect([...obj.gen()]).toEqual([1, 2]);
  });

  test("restricted production: newline after yield yields undefined", () => {
    // ES2026 §15.5: yield [no LineTerminator here] AssignmentExpression
    const obj = {
      *gen() {
        yield
        2
      }
    }
    expect([...obj.gen()]).toEqual([undefined]);
  });

  // A line break inside a template literal or a string's LineContinuation is
  // part of that token, not a LineTerminator after `yield`.
  test("multi-line template on the yield line is yielded", () => {
    const obj = {
      *gen() {
        yield `a
b`
      }
    }
    expect([...obj.gen()]).toEqual(["a\nb"]);
  });

  test("string with a line continuation is yielded", () => {
    const obj = {
      *gen() {
        yield "a\
b"
      }
    }
    expect([...obj.gen()]).toEqual(["ab"]);
  });
});
