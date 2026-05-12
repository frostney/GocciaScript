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
    const toStringDescriptor = Object.getOwnPropertyDescriptor(String.prototype, "toString");

    expect(Object.prototype.isPrototypeOf(String.prototype)).toBe(true);
    expect(Object.prototype.toString.call(String.prototype)).toBe("[object String]");

    try {
      delete String.prototype.toString;
      expect(String.prototype.toString()).toBe("[object String]");
    } finally {
      Object.defineProperty(String.prototype, "toString", toStringDescriptor);
    }
  });

  test("falls through prototype lookup for missing numeric indices", () => {
    Object.prototype[0] = "inherited zero";
    String.prototype[3] = "inherited three";
    Object.prototype["01"] = "non-canonical index";

    try {
      expect(String.prototype[0]).toBe("inherited zero");
      expect(new String("abc")[3]).toBe("inherited three");
      expect(new String("abc")["01"]).toBe("non-canonical index");
    } finally {
      delete Object.prototype[0];
      delete String.prototype[3];
      delete Object.prototype["01"];
    }
  });
});
