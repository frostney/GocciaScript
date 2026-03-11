/*---
description: JSON[Symbol.toStringTag]
features: [json]
---*/

describe("JSON[Symbol.toStringTag]", () => {
  test("Object.prototype.toString returns [object JSON]", () => {
    expect(Object.prototype.toString.call(JSON)).toBe("[object JSON]");
  });

  test("JSON.toString() returns [object JSON]", () => {
    expect(JSON.toString()).toBe("[object JSON]");
  });

  test("JSON.hasOwnProperty works", () => {
    expect(JSON.hasOwnProperty("parse")).toBe(true);
    expect(JSON.hasOwnProperty("stringify")).toBe(true);
    expect(JSON.hasOwnProperty("missing")).toBe(false);
  });
});
