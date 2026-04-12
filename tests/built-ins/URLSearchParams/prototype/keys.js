/*---
description: URLSearchParams.prototype.keys
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.keys", () => {
  test("returns an iterator of all keys", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    const keys = [];
    for (const key of params.keys()) {
      keys.push(key);
    }
    expect(keys).toEqual(["a", "b", "c"]);
  });

  test("duplicate keys appear multiple times", () => {
    const params = new URLSearchParams("a=1&a=2&b=3");
    const keys = [];
    for (const key of params.keys()) {
      keys.push(key);
    }
    expect(keys).toEqual(["a", "a", "b"]);
  });

  test("returns empty iterator for empty params", () => {
    const params = new URLSearchParams();
    const keys = [];
    for (const key of params.keys()) {
      keys.push(key);
    }
    expect(keys).toEqual([]);
  });

  test("iterator is in insertion order", () => {
    const params = new URLSearchParams();
    params.append("z", "1");
    params.append("a", "2");
    params.append("m", "3");
    const keys = [];
    for (const key of params.keys()) {
      keys.push(key);
    }
    expect(keys).toEqual(["z", "a", "m"]);
  });

  test("keys iterator can be spread into an array", () => {
    const params = new URLSearchParams("x=1&y=2");
    const keys = [...params.keys()];
    expect(keys).toEqual(["x", "y"]);
  });
});
