/*---
description: URLSearchParams.prototype.delete
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.delete", () => {
  test("deleting via url.searchParams updates url.search", () => {
    const url = new URL("https://example.com/?a=1&a=2&b=3");
    url.searchParams.delete("a");
    expect(url.search).toBe("?b=3");
  });

  test("deletes all entries with the given name", () => {
    const params = new URLSearchParams("a=1&b=2&a=3");
    params.delete("a");
    expect(params.has("a")).toBe(false);
    expect(params.get("b")).toBe("2");
  });

  test("deleting nonexistent key does nothing", () => {
    const params = new URLSearchParams("a=1");
    params.delete("z");
    expect(params.size).toBe(1);
  });

  test("size decreases after delete", () => {
    const params = new URLSearchParams("a=1&b=2");
    params.delete("a");
    expect(params.size).toBe(1);
  });

  test("delete with value removes only matching entry", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    params.delete("a", "2");
    expect(params.getAll("a")).toEqual(["1", "3"]);
  });

  test("delete with value does nothing if value not found", () => {
    const params = new URLSearchParams("a=1&a=2");
    params.delete("a", "99");
    expect(params.getAll("a")).toEqual(["1", "2"]);
  });

  test("delete with value removes all matching pairs", () => {
    const params = new URLSearchParams("a=1&a=1&b=2");
    params.delete("a", "1");
    expect(params.getAll("a")).toEqual([]);
    expect(params.size).toBe(1);
  });

  test("delete last entry results in empty params", () => {
    const params = new URLSearchParams("a=1");
    params.delete("a");
    expect(params.size).toBe(0);
    expect(params.toString()).toBe("");
  });
});
