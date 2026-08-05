/*---
description: A definite assignment assertion "!" must follow the binding name on the same line, so a leading-"!" statement after ASI is not absorbed
features: [automatic-semicolon-insertion, types-as-comments]
---*/

describe("definite assignment assertion line restriction", () => {
  test("leading-! expression statement starts a new statement", () => {
    let ran = false
    let x
    !(() => { ran = true; })()

    expect(ran).toBe(true);
    expect(x).toBeUndefined();
  });

  test("leading-! negation after an uninitialized declaration", () => {
    const truthy = 1
    let flag
    !truthy

    expect(flag).toBeUndefined();
    expect(!truthy).toBe(false);
  });

  test("same-line assertion is still an assertion", () => {
    let value!: number
    value = 5

    expect(value).toBe(5);
  });

  test("assertion and a following leading-! statement", () => {
    let seen = false
    let value!: number
    value = 7
    !(() => { seen = true; })()

    expect(value).toBe(7);
    expect(seen).toBe(true);
  });
});
