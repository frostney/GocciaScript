/*---
description: URLSearchParams.prototype.toString
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.toString", () => {
  test("serializes a single key-value pair", () => {
    const params = new URLSearchParams("a=1");
    expect(params.toString()).toBe("a=1");
  });

  test("serializes multiple pairs with ampersand separator", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    expect(params.toString()).toBe("a=1&b=2&c=3");
  });

  test("returns empty string for empty params", () => {
    const params = new URLSearchParams();
    expect(params.toString()).toBe("");
  });

  test("spaces are encoded as plus signs", () => {
    const params = new URLSearchParams();
    params.set("q", "hello world");
    expect(params.toString()).toBe("q=hello+world");
  });

  test("special characters are percent-encoded", () => {
    const params = new URLSearchParams();
    params.set("key", "a=b&c");
    const str = params.toString();
    expect(str).not.toContain("a=b&c");
    expect(str).toContain("key=");
  });

  test("at sign is percent-encoded", () => {
    const params = new URLSearchParams();
    params.set("email", "user@example.com");
    expect(params.toString()).toContain("user%40example.com");
  });

  test("round-trips through parse and stringify", () => {
    const original = "a=1&b=hello+world&c=3";
    const params = new URLSearchParams(original);
    expect(params.get("b")).toBe("hello world");
    expect(params.toString()).toBe(original);
  });

  test("duplicate keys are both included in output", () => {
    const params = new URLSearchParams("a=1&a=2");
    expect(params.toString()).toBe("a=1&a=2");
  });

  test("leading question mark is not included", () => {
    const params = new URLSearchParams("?a=1");
    expect(params.toString()).toBe("a=1");
  });
});
