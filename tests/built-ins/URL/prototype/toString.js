/*---
description: URL.prototype.toString
features: [URL]
---*/

describe("URL.prototype.toString", () => {
  test("toString returns the same value as href", () => {
    const url = new URL("https://example.com/path?q=1#hash");
    expect(url.toString()).toBe(url.href);
  });

  test("toString for bare origin URL", () => {
    const url = new URL("https://example.com");
    expect(url.toString()).toBe("https://example.com/");
  });

  test("toString after modifying pathname", () => {
    const url = new URL("https://example.com/old");
    url.pathname = "/new";
    expect(url.toString()).toBe(url.href);
  });

  test("toString after modifying search", () => {
    const url = new URL("https://example.com/path");
    url.search = "x=1";
    expect(url.toString()).toBe(url.href);
  });

  test("toString after modifying hash", () => {
    const url = new URL("https://example.com/path");
    url.hash = "section";
    expect(url.toString()).toBe(url.href);
  });

  test("toString reflects full URL with credentials", () => {
    const url = new URL("https://user:pass@example.com/path");
    expect(url.toString()).toBe(url.href);
  });
});
