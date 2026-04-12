/*---
description: URL.prototype.searchParams getter
features: [URL, URLSearchParams]
---*/

describe("URL.prototype.searchParams", () => {
  test("returns a URLSearchParams object", () => {
    const url = new URL("https://example.com/path?a=1&b=2");
    const params = url.searchParams;
    expect(params instanceof URLSearchParams).toBe(true);
  });

  test("searchParams reflects existing query string", () => {
    const url = new URL("https://example.com/path?a=1&b=2");
    expect(url.searchParams.get("a")).toBe("1");
    expect(url.searchParams.get("b")).toBe("2");
  });

  test("empty query gives empty searchParams", () => {
    const url = new URL("https://example.com/path");
    expect(url.searchParams.get("a")).toBe(null);
  });

  test("modifying searchParams updates url.search", () => {
    const url = new URL("https://example.com/path?a=1");
    url.searchParams.set("a", "2");
    expect(url.search).toBe("?a=2");
  });

  test("appending to searchParams updates url.search", () => {
    const url = new URL("https://example.com/path");
    url.searchParams.append("x", "hello");
    expect(url.search).toBe("?x=hello");
  });

  test("deleting from searchParams updates url.search", () => {
    const url = new URL("https://example.com/path?a=1&b=2");
    url.searchParams.delete("a");
    expect(url.search).toBe("?b=2");
  });

  test("setting url.search updates searchParams", () => {
    const url = new URL("https://example.com/path?a=1");
    url.search = "?b=99";
    expect(url.searchParams.get("b")).toBe("99");
    expect(url.searchParams.get("a")).toBe(null);
  });

  test("returns same object on repeated access", () => {
    const url = new URL("https://example.com/path?a=1");
    const p1 = url.searchParams;
    const p2 = url.searchParams;
    expect(p1).toBe(p2);
  });
});
