/*---
description: URLSearchParams.prototype.get
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.get", () => {
  test("returns the value for the given name", () => {
    const params = new URLSearchParams("a=1");
    expect(params.get("a")).toBe("1");
  });

  test("returns null for a nonexistent name", () => {
    const params = new URLSearchParams("a=1");
    expect(params.get("b")).toBe(null);
  });

  test("returns the first value when multiple exist", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    expect(params.get("a")).toBe("1");
  });

  test("returns empty string for key with empty value", () => {
    const params = new URLSearchParams("a=");
    expect(params.get("a")).toBe("");
  });

  test("returns decoded value", () => {
    const params = new URLSearchParams("a=hello%20world");
    expect(params.get("a")).toBe("hello world");
  });

  test("returns decoded plus-as-space", () => {
    const params = new URLSearchParams("a=hello+world");
    expect(params.get("a")).toBe("hello world");
  });

  test("returns null on empty params", () => {
    const params = new URLSearchParams();
    expect(params.get("anything")).toBe(null);
  });

  test("key is case-sensitive", () => {
    const params = new URLSearchParams("Key=value");
    expect(params.get("Key")).toBe("value");
    expect(params.get("key")).toBe(null);
  });
});
