/*---
description: A newline before parameters continues an ordinary method named async
features: [automatic-semicolon-insertion]
---*/

describe("ordinary static method named async", () => {
  test("a line break before parameters does not end the class element", () => {
    class Named {
      static async
      () { return 5; }
    }
    expect(Named.async()).toBe(5);
  });
});
