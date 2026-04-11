/*---
description: URLSearchParams.prototype.sort
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.sort", () => {
  test("sorting url.searchParams updates url.search", () => {
    const url = new URL("https://example.com/path?b=2&a=1");
    url.searchParams.sort();
    expect(url.search).toBe("?a=1&b=2");
  });

  test("sorts params by name in ascending order", () => {
    const params = new URLSearchParams("c=3&a=1&b=2");
    params.sort();
    const keys = [];
    for (const [k] of params.entries()) {
      keys.push(k);
    }
    expect(keys).toEqual(["a", "b", "c"]);
  });

  test("sort is stable: duplicate keys preserve relative order", () => {
    const params = new URLSearchParams("b=2&a=first&a=second&b=1");
    params.sort();
    expect(params.getAll("a")).toEqual(["first", "second"]);
    expect(params.getAll("b")).toEqual(["2", "1"]);
  });

  test("keys are in sorted order after sort", () => {
    const params = new URLSearchParams("z=last&m=mid&a=first");
    params.sort();
    const entries = [];
    for (const entry of params.entries()) {
      entries.push(entry);
    }
    expect(entries[0][0]).toBe("a");
    expect(entries[1][0]).toBe("m");
    expect(entries[2][0]).toBe("z");
  });

  test("sort does not change values", () => {
    const params = new URLSearchParams("c=30&a=10&b=20");
    params.sort();
    expect(params.get("a")).toBe("10");
    expect(params.get("b")).toBe("20");
    expect(params.get("c")).toBe("30");
  });

  test("sort on already sorted params is a no-op", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    params.sort();
    expect(params.toString()).toBe("a=1&b=2&c=3");
  });

  test("sort on empty params does not throw", () => {
    const params = new URLSearchParams();
    params.sort();
    expect(params.size).toBe(0);
  });

  test("sort on single entry does not throw", () => {
    const params = new URLSearchParams("z=1");
    params.sort();
    expect(params.get("z")).toBe("1");
  });
});
