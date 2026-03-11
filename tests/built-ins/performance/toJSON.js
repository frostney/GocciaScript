/*---
description: performance.toJSON
features: [performance]
---*/

describe("performance.toJSON", () => {
  test("returns a JSON snapshot with timeOrigin", () => {
    expect(performance.toJSON()).toEqual({
      timeOrigin: performance.timeOrigin,
    });
  });

  test("does not include methods in the JSON snapshot", () => {
    const json = performance.toJSON();

    expect(json.now).toBe(undefined);
    expect(json.toJSON).toBe(undefined);
  });
});
