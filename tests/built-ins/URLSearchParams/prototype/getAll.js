/*---
description: URLSearchParams.prototype.getAll
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.getAll", () => {
  test("returns array with single value", () => {
    const params = new URLSearchParams("a=1");
    expect(params.getAll("a")).toEqual(["1"]);
  });

  test("returns all values for duplicate keys", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    expect(params.getAll("a")).toEqual(["1", "2", "3"]);
  });

  test("returns empty array for nonexistent key", () => {
    const params = new URLSearchParams("a=1");
    expect(params.getAll("b")).toEqual([]);
  });

  test("returns values in insertion order", () => {
    const params = new URLSearchParams();
    params.append("x", "c");
    params.append("x", "a");
    params.append("x", "b");
    expect(params.getAll("x")).toEqual(["c", "a", "b"]);
  });

  test("returns decoded values", () => {
    const params = new URLSearchParams("a=hello+world&a=foo%20bar");
    expect(params.getAll("a")).toEqual(["hello world", "foo bar"]);
  });

  test("returns empty array on empty params", () => {
    const params = new URLSearchParams();
    expect(params.getAll("a")).toEqual([]);
  });

  test("only returns values for the specified key", () => {
    const params = new URLSearchParams("a=1&b=2&a=3&b=4");
    expect(params.getAll("a")).toEqual(["1", "3"]);
    expect(params.getAll("b")).toEqual(["2", "4"]);
  });
});
