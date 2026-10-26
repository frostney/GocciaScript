/*---
description: >
  An error a Date method throws has a stack that keeps every frame of its
  caller, and locates the method's own frame in the Date implementation
  rather than in the calling file.
features: [Date, Error.prototype.stack]
---*/

describe("errors thrown by Date methods", () => {
  test("keep the calling function's frame in the stack", () => {
    // A block body keeps the call out of tail position (ES2026 §15.10.2), so
    // the caller's frame stays on the stack.
    const formatInvalidDate = () => {
      const text = new Date(NaN).toISOString();
      return text;
    };
    let stack = "";
    try {
      formatInvalidDate();
    } catch (error) {
      expect(error instanceof RangeError).toBe(true);
      stack = String(error.stack);
    }
    expect(stack).toContain("formatInvalidDate");
  });

  test("locate the method's frame outside the calling file", () => {
    // toISOString throws in its own body, so its frame is the innermost one.
    // (A method such as getTime throws from a helper it calls, whose frame
    // is innermost instead.)
    let stack = "";
    try {
      new Date(NaN).toISOString();
    } catch (error) {
      expect(error instanceof RangeError).toBe(true);
      stack = String(error.stack);
    }
    const firstFrame = stack.split("\n").find((line) => line.trim().startsWith("at "));
    expect(firstFrame).toContain("toISOString");
    expect(firstFrame).not.toContain("error-stack.js");
  });
});
