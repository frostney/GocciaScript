/*---
description: A postfix non-null assertion "!" must follow its operand on the same line, so a leading-"!" statement after ASI is not absorbed
features: [automatic-semicolon-insertion, types-as-comments]
---*/

describe("non-null assertion line restriction", () => {
  test("leading-! expression statement starts a new statement", () => {
    let ran = false
    const value = 1
    !(() => { ran = true; })()

    expect(value).toBe(1);
    expect(ran).toBe(true);
  });

  test("leading-! after a member access starts a new statement", () => {
    let ran = false
    const holder = { v: "x" }
    const read = holder.v
    !(() => { ran = true; })()

    expect(read).toBe("x");
    expect(ran).toBe(true);
  });

  test("same-line assertion is still erased", () => {
    const holder = { v: "abc" }
    const length = holder.v!.length

    expect(length).toBe(3);
  });

  test("assertion and a following leading-! statement", () => {
    let ran = false
    const holder = { v: "abc" }
    const length = holder.v!.length
    !(() => { ran = true; })()

    expect(length).toBe(3);
    expect(ran).toBe(true);
  });
});
