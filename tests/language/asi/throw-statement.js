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
});
