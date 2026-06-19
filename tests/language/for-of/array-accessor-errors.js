/*---
description: for...of forwards array indexed accessor errors before the loop body
features: [for-of]
---*/

describe("for...of array accessor errors", () => {
  test("throws while reading the next array value", () => {
    let array = [];
    const thrown = new Error("array accessor");
    let caught;
    let iterationCount = 0;

    Object.defineProperty(array, "0", {
      get() {
        throw thrown;
      }
    });

    try {
      for (const value of array) {
        iterationCount += 1;
      }
    } catch (error) {
      caught = error;
    }

    expect(caught).toBe(thrown);
    expect(iterationCount).toBe(0);
  });
});
