/*---
description: String.prototype is the empty String exotic object
features: [String.prototype]
---*/

describe("String.prototype object", () => {
  test("has empty string data", () => {
    expect(String.prototype.toString()).toBe("");
    expect(String.prototype.valueOf()).toBe("");
    expect(String.prototype.length).toBe(0);
  });

  test("inherits from Object.prototype with the String brand", () => {
    const ownToString = String.prototype.toString;

    expect(Object.prototype.isPrototypeOf(String.prototype)).toBe(true);
    expect(Object.prototype.toString.call(String.prototype)).toBe("[object String]");

    try {
      delete String.prototype.toString;
      expect(String.prototype.toString()).toBe("[object String]");
    } finally {
      String.prototype.toString = ownToString;
    }
  });
});
