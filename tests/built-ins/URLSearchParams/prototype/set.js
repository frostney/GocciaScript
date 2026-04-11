/*---
description: URLSearchParams.prototype.set
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.set", () => {
  test("sets a new key-value pair", () => {
    const params = new URLSearchParams();
    params.set("a", "1");
    expect(params.get("a")).toBe("1");
  });

  test("updates the first occurrence of an existing key", () => {
    const params = new URLSearchParams("a=old");
    params.set("a", "new");
    expect(params.get("a")).toBe("new");
  });

  test("removes duplicates, leaving only the updated first occurrence", () => {
    const params = new URLSearchParams("a=1&b=x&a=2&a=3");
    params.set("a", "99");
    expect(params.getAll("a")).toEqual(["99"]);
  });

  test("position of the first occurrence is preserved", () => {
    const params = new URLSearchParams("a=1&b=2&a=3");
    params.set("a", "99");
    const keys = [];
    for (const [k] of params.entries()) {
      keys.push(k);
    }
    expect(keys[0]).toBe("a");
    expect(keys[1]).toBe("b");
  });

  test("size is correct after set removes duplicates", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    params.set("a", "x");
    expect(params.size).toBe(1);
  });

  test("set with empty string value", () => {
    const params = new URLSearchParams("a=1");
    params.set("a", "");
    expect(params.get("a")).toBe("");
  });

  test("set encodes value with special characters", () => {
    const params = new URLSearchParams();
    params.set("q", "hello world");
    expect(params.get("q")).toBe("hello world");
    expect(params.toString()).toContain("q=hello+world");
  });
});
