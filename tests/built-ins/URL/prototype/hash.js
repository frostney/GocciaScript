/*---
description: URL.prototype.hash getter and setter
features: [URL]
---*/

describe("URL.prototype.hash", () => {
  test("getter returns fragment with leading hash", () => {
    const url = new URL("https://example.com/path#section");
    expect(url.hash).toBe("#section");
  });

  test("getter returns empty string when no fragment", () => {
    const url = new URL("https://example.com/path");
    expect(url.hash).toBe("");
  });

  test("getter returns empty string when no fragment but search present", () => {
    const url = new URL("https://example.com/path?q=1");
    expect(url.hash).toBe("");
  });

  test("setter updates the fragment", () => {
    const url = new URL("https://example.com/path");
    url.hash = "#newfrag";
    expect(url.hash).toBe("#newfrag");
  });

  test("setter adds hash prefix if omitted", () => {
    const url = new URL("https://example.com/path");
    url.hash = "section2";
    expect(url.hash).toBe("#section2");
  });

  test("setter with empty string clears fragment", () => {
    const url = new URL("https://example.com/path#section");
    url.hash = "";
    expect(url.hash).toBe("");
  });

  test("href reflects updated hash", () => {
    const url = new URL("https://example.com/path?q=1");
    url.hash = "bottom";
    expect(url.href).toBe("https://example.com/path?q=1#bottom");
  });

  test("clearing hash removes it from href", () => {
    const url = new URL("https://example.com/path#top");
    url.hash = "";
    expect(url.href).toBe("https://example.com/path");
  });
});
