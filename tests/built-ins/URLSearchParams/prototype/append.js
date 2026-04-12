/*---
description: URLSearchParams.prototype.append
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.append", () => {
  test("appends a new key-value pair", () => {
    const params = new URLSearchParams();
    params.append("a", "1");
    expect(params.get("a")).toBe("1");
  });

  test("appending to existing key adds another entry", () => {
    const params = new URLSearchParams("a=1");
    params.append("a", "2");
    expect(params.getAll("a")).toEqual(["1", "2"]);
  });

  test("size increases with each append", () => {
    const params = new URLSearchParams();
    params.append("a", "1");
    params.append("b", "2");
    expect(params.size).toBe(2);
  });

  test("appending duplicate key does not remove first", () => {
    const params = new URLSearchParams("a=1");
    params.append("a", "2");
    expect(params.size).toBe(2);
    expect(params.get("a")).toBe("1");
  });

  test("appended value is accessible via getAll", () => {
    const params = new URLSearchParams();
    params.append("color", "red");
    params.append("color", "blue");
    params.append("color", "green");
    expect(params.getAll("color")).toEqual(["red", "blue", "green"]);
  });

  test("append encodes special characters", () => {
    const params = new URLSearchParams();
    params.append("q", "hello world");
    expect(params.get("q")).toBe("hello world");
    expect(params.toString()).toContain("q=hello+world");
  });

  test("append with empty string value", () => {
    const params = new URLSearchParams();
    params.append("key", "");
    expect(params.get("key")).toBe("");
  });
});
