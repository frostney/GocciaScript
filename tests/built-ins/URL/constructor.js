/*---
description: URL constructor with absolute and relative URLs
features: [URL]
---*/

describe("URL constructor", () => {
  test("basic absolute URL normalizes href", () => {
    const url = new URL("https://example.com");
    expect(url.href).toBe("https://example.com/");
  });

  test("URL with all components", () => {
    const url = new URL("https://user:pass@example.com:8080/path?q=1#hash");
    expect(url.protocol).toBe("https:");
    expect(url.username).toBe("user");
    expect(url.password).toBe("pass");
    expect(url.hostname).toBe("example.com");
    expect(url.port).toBe("8080");
    expect(url.pathname).toBe("/path");
    expect(url.search).toBe("?q=1");
    expect(url.hash).toBe("#hash");
  });

  test("relative URL with string base", () => {
    const url = new URL("/path", "https://example.com");
    expect(url.href).toBe("https://example.com/path");
  });

  test("relative URL with base URL object", () => {
    const base = new URL("https://example.com/foo/baz");
    const url = new URL("../bar", base);
    expect(url.href).toBe("https://example.com/bar");
  });

  test("relative path resolves against base", () => {
    const url = new URL("page.html", "https://example.com/dir/");
    expect(url.href).toBe("https://example.com/dir/page.html");
  });

  test("throws TypeError for invalid URL without base", () => {
    expect(() => new URL("not-a-url")).toThrow(TypeError);
  });

  test("throws TypeError for invalid base", () => {
    expect(() => new URL("/path", "not-valid")).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => new URL()).toThrow(TypeError);
  });

  test("URL preserves trailing slash", () => {
    const url = new URL("https://example.com/path/");
    expect(url.pathname).toBe("/path/");
  });

  test("URL without path gets slash added", () => {
    const url = new URL("https://example.com");
    expect(url.pathname).toBe("/");
  });
});
