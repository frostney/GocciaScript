/*---
description: A type declaration broken across lines is erased whole under ASI instead of ending at the line break
features: [automatic-semicolon-insertion, types-as-comments]
---*/

describe("ASI type declarations", () => {
  test("a union with leading pipes is erased whole", () => {
    type GroupId =
      | "names"
      | "functions"

    const group = "names"

    expect(group).toBe("names");
  });

  test("an exported union with leading pipes is erased whole", () => {
    export type ExportedGroupId =
      | "names"
      | "functions"

    const group = "functions"

    expect(group).toBe("functions");
  });

  test("a type body broken right after the '=' is erased whole", () => {
    type Named =
      MissingReferenceType

    const named = { id: 1 }

    expect(named.id).toBe(1);
  });

  test("an intersection with leading ampersands is erased whole", () => {
    type Combined =
      & { left: number }
      & { right: number }

    const combined = { left: 1, right: 2 }

    expect(combined.left + combined.right).toBe(3);
  });

  test("a function type broken before its arrow is erased whole", () => {
    type Handler = (value: number)
      => string

    const handler = (value) => `${value}`

    expect(handler(7)).toBe("7");
  });

  test("a conditional type broken before its branches is erased whole", () => {
    type Unwrap<T> = T extends Array<infer U>
      ? U
      : T

    const unwrapped = 5

    expect(unwrapped).toBe(5);
  });

  test("the statement after an erased multi-line type still runs", () => {
    let ran = false

    type Ignored =
      | "a"
      | "b"

    ran = true

    expect(ran).toBe(true);
  });
});
