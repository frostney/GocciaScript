/*---
description: URLSearchParams constructor
features: [URLSearchParams]
---*/

describe("URLSearchParams constructor", () => {
  test("no arguments creates empty params", () => {
    const params = new URLSearchParams();
    expect(params.size).toBe(0);
  });

  test("string argument parses key=value pairs", () => {
    const params = new URLSearchParams("a=1&b=2");
    expect(params.get("a")).toBe("1");
    expect(params.get("b")).toBe("2");
    expect(params.size).toBe(2);
  });

  test("string with leading question mark strips it", () => {
    const params = new URLSearchParams("?a=1&b=2");
    expect(params.get("a")).toBe("1");
    expect(params.get("b")).toBe("2");
  });

  test("array of pairs initializes params", () => {
    const params = new URLSearchParams([["a", "1"], ["b", "2"]]);
    expect(params.get("a")).toBe("1");
    expect(params.get("b")).toBe("2");
  });

  test("array allows duplicate keys", () => {
    const params = new URLSearchParams([["a", "1"], ["a", "2"]]);
    expect(params.getAll("a")).toEqual(["1", "2"]);
  });

  test("object initializes params from own enumerable properties", () => {
    const params = new URLSearchParams({ a: "1", b: "2" });
    expect(params.get("a")).toBe("1");
    expect(params.get("b")).toBe("2");
  });

  test("copy from another URLSearchParams", () => {
    const source = new URLSearchParams("x=10&y=20");
    const copy = new URLSearchParams(source);
    expect(copy.get("x")).toBe("10");
    expect(copy.get("y")).toBe("20");
  });

  test("empty string creates empty params", () => {
    const params = new URLSearchParams("");
    expect(params.size).toBe(0);
  });

  test("string with encoded characters is decoded", () => {
    const params = new URLSearchParams("a=hello%20world");
    expect(params.get("a")).toBe("hello world");
  });

  test("plus sign in string is decoded as space", () => {
    const params = new URLSearchParams("a=hello+world");
    expect(params.get("a")).toBe("hello world");
  });

  test("malformed pair with fewer than 2 elements throws", () => {
    expect(() => {
      new URLSearchParams([["only-name"]]);
    }).toThrow(TypeError);
  });

  test("malformed pair with no elements throws", () => {
    expect(() => {
      new URLSearchParams([[]]);
    }).toThrow(TypeError);
  });
});
