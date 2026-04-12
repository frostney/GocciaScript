/*---
description: URLSearchParams.prototype.has
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.has", () => {
  test("returns true for an existing key", () => {
    const params = new URLSearchParams("a=1");
    expect(params.has("a")).toBe(true);
  });

  test("returns false for a nonexistent key", () => {
    const params = new URLSearchParams("a=1");
    expect(params.has("b")).toBe(false);
  });

  test("returns false for empty params", () => {
    const params = new URLSearchParams();
    expect(params.has("a")).toBe(false);
  });

  test("is case-sensitive", () => {
    const params = new URLSearchParams("Key=value");
    expect(params.has("Key")).toBe(true);
    expect(params.has("key")).toBe(false);
  });

  test("has with value returns true when name and value match", () => {
    const params = new URLSearchParams("a=1&a=2");
    expect(params.has("a", "1")).toBe(true);
    expect(params.has("a", "2")).toBe(true);
  });

  test("has with value returns false when value does not match", () => {
    const params = new URLSearchParams("a=1");
    expect(params.has("a", "99")).toBe(false);
  });

  test("has with value returns false for nonexistent key", () => {
    const params = new URLSearchParams("a=1");
    expect(params.has("b", "1")).toBe(false);
  });

  test("returns true when key has empty string value and checked with empty string", () => {
    const params = new URLSearchParams("a=");
    expect(params.has("a", "")).toBe(true);
  });

  test("returns true after appending key", () => {
    const params = new URLSearchParams();
    params.append("x", "y");
    expect(params.has("x")).toBe(true);
  });

  test("returns false after deleting key", () => {
    const params = new URLSearchParams("a=1");
    params.delete("a");
    expect(params.has("a")).toBe(false);
  });
});
