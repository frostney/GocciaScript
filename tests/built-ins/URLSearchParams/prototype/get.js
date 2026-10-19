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

  test("percent-decoded bytes are read as UTF-8", () => {
    const params = new URLSearchParams("q=%C3%A9%E2%82%AC%F0%9F%98%80&r=é&s=%FF");
    expect(params.get("q")).toBe("é€\u{1F600}");
    expect(params.get("r")).toBe("é");
    expect(params.get("s")).toBe("�");
  });

  test("a name with a lone surrogate matches the name the constructor parsed", () => {
    const params = new URLSearchParams("\ud800=x");
    expect(params.get("\ud800")).toBe("x");
    expect(params.get("�")).toBe("x");
  });
});
