/*---
description: URLSearchParams.prototype.values
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.values", () => {
  test("returns an iterator of all values", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    const values = [];
    for (const value of params.values()) {
      values.push(value);
    }
    expect(values).toEqual(["1", "2", "3"]);
  });

  test("duplicate keys produce multiple values", () => {
    const params = new URLSearchParams("a=1&a=2&b=3");
    const values = [];
    for (const value of params.values()) {
      values.push(value);
    }
    expect(values).toEqual(["1", "2", "3"]);
  });

  test("returns empty iterator for empty params", () => {
    const params = new URLSearchParams();
    const values = [];
    for (const value of params.values()) {
      values.push(value);
    }
    expect(values).toEqual([]);
  });

  test("values are decoded", () => {
    const params = new URLSearchParams("a=hello+world&b=foo%20bar");
    const values = [];
    for (const value of params.values()) {
      values.push(value);
    }
    expect(values).toEqual(["hello world", "foo bar"]);
  });

  test("values iterator can be spread into an array", () => {
    const params = new URLSearchParams("x=10&y=20");
    const values = [...params.values()];
    expect(values).toEqual(["10", "20"]);
  });

  test("values in insertion order", () => {
    const params = new URLSearchParams();
    params.append("a", "first");
    params.append("a", "second");
    const values = [...params.values()];
    expect(values).toEqual(["first", "second"]);
  });
});
