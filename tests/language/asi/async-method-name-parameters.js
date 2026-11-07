/*---
description: A newline before parameters continues an ordinary method named async
features: [automatic-semicolon-insertion]
---*/

describe("ordinary parameters method named async", () => {
  test("a line break before parameters does not end the class element", () => {
    class Named {
      async
      (value) { return value + 1; }
    }
    expect(new Named().async(5)).toBe(6);
  });
});
