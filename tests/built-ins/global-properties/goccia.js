/*---
description: >
  Goccia is a const global object that exposes engine metadata and utility APIs.
features: [global-properties, Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia global", () => {
  test("Goccia is an object", () => {
    expect(typeof Goccia).toBe("object");
  });

  test("Goccia cannot be reassigned", () => {
    expect(() => {
      Goccia = {};
    }).toThrow(TypeError);
  });
});
