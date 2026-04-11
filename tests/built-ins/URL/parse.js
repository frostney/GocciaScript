/*---
description: URL.parse static method
features: [URL]
---*/

describe("URL.parse", () => {
  test("returns a URL object for a valid absolute URL", () => {
    const url = URL.parse("https://example.com/path");
    expect(url).not.toBe(null);
    expect(url.href).toBe("https://example.com/path");
  });

  test("returns null for an invalid URL", () => {
    const result = URL.parse("not-a-url");
    expect(result).toBe(null);
  });

  test("returns null for empty string", () => {
    expect(URL.parse("")).toBe(null);
  });

  test("returns null for relative URL without base", () => {
    expect(URL.parse("/path")).toBe(null);
  });

  test("works with a string base URL", () => {
    const url = URL.parse("/path", "https://example.com");
    expect(url).not.toBe(null);
    expect(url.href).toBe("https://example.com/path");
  });

  test("works with a URL object as base", () => {
    const base = new URL("https://example.com/dir/");
    const url = URL.parse("file.html", base);
    expect(url).not.toBe(null);
    expect(url.href).toBe("https://example.com/dir/file.html");
  });

  test("returns null when base is invalid", () => {
    const result = URL.parse("/path", "not-valid");
    expect(result).toBe(null);
  });

  test("returned object has correct components", () => {
    const url = URL.parse("https://example.com:9000/search?q=hello#section");
    expect(url.hostname).toBe("example.com");
    expect(url.port).toBe("9000");
    expect(url.pathname).toBe("/search");
    expect(url.search).toBe("?q=hello");
    expect(url.hash).toBe("#section");
  });
});
