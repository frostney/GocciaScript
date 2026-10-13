/*---
description: >
  An error a Date method throws has a stack that keeps every frame of its
  caller, and locates the method's own frame in the Date implementation
  rather than in the calling file.
features: [Date, Error.prototype.stack]
---*/

describe("errors thrown by Date methods", () => {
  test("keep the calling function's frame in the stack", () => {
    const formatInvalidDate = () => new Date(NaN).toISOString();
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
    let stack = "";
    try {
      Date.prototype.getTime.call({});
    } catch (error) {
      expect(error instanceof TypeError).toBe(true);
      stack = String(error.stack);
    }
    const firstFrame = stack.split("\n").find((line) => line.trim().startsWith("at "));
    expect(firstFrame).not.toContain("error-stack.js");
  });
});
