/*---
description: A newline before parameters continues an ordinary method named async
features: [automatic-semicolon-insertion]
---*/

describe("ordinary instance method named async", () => {
  test("a line break before parameters does not end the class element", () => {
    class Named {
      async
      () { return 4; }
    }
    expect(new Named().async()).toBe(4);
  });
});
