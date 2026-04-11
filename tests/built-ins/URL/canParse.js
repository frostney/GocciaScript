/*---
description: URL.canParse static method
features: [URL]
---*/

describe("URL.canParse", () => {
  test("returns true for a valid absolute URL", () => {
    expect(URL.canParse("https://example.com")).toBe(true);
  });

  test("returns true for URL with all components", () => {
    expect(URL.canParse("https://user:pass@example.com:8080/path?q=1#hash")).toBe(true);
  });

  test("returns false for an invalid URL without base", () => {
    expect(URL.canParse("not-a-url")).toBe(false);
  });

  test("returns false for empty string", () => {
    expect(URL.canParse("")).toBe(false);
  });

  test("returns true for valid relative URL with base", () => {
    expect(URL.canParse("/path", "https://example.com")).toBe(true);
  });

  test("returns true for relative URL with base URL object", () => {
    const base = new URL("https://example.com/foo/");
    expect(URL.canParse("bar", base)).toBe(true);
  });

  test("returns false for relative URL with invalid base string", () => {
    expect(URL.canParse("/path", "not-valid")).toBe(false);
  });

  test("returns false for relative path without base", () => {
    expect(URL.canParse("/path")).toBe(false);
  });

  test("returns true for http URL", () => {
    expect(URL.canParse("http://example.com/page")).toBe(true);
  });

  test("returns true for file URL", () => {
    expect(URL.canParse("file:///home/user/file.txt")).toBe(true);
  });
});
