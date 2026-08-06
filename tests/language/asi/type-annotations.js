/*---
description: A type annotation with no initializer ends at the line break under ASI instead of running into the next statement
features: [automatic-semicolon-insertion, types-as-comments]
---*/

describe("ASI type annotations", () => {
  test("annotation without an initializer does not absorb the next statement", () => {
    let value: number
    value = 5

    expect(value).toBe(5);
  });

  test("the statement after an uninitialized annotation still runs", () => {
    let first: string
    let ran = false
    ran = true

    expect(first).toBeUndefined();
    expect(ran).toBe(true);
  });

  test("annotated declaration with an initializer is unchanged", () => {
    let value: number = 3
    const doubled = value * 2

    expect(doubled).toBe(6);
  });

  test("a union broken after the operator continues on the next line", () => {
    let value: string |
      number = "kept"

    expect(value).toBe("kept");
  });

  test("an annotation broken right after the colon continues", () => {
    let value:
      number = 4

    expect(value).toBe(4);
  });

  test("structured annotation spanning lines", () => {
    let config: {
      retries: number
    } = { retries: 2 }

    expect(config.retries).toBe(2);
  });
});
