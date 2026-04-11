/*---
description: URL.prototype.href getter and setter
features: [URL]
---*/

describe("URL.prototype.href", () => {
  test("getter returns the full serialized URL", () => {
    const url = new URL("https://example.com/path?q=1#hash");
    expect(url.href).toBe("https://example.com/path?q=1#hash");
  });

  test("getter includes trailing slash for bare origin", () => {
    const url = new URL("https://example.com");
    expect(url.href).toBe("https://example.com/");
  });

  test("setter updates the entire URL", () => {
    const url = new URL("https://example.com/old");
    url.href = "https://other.org/new?x=2#frag";
    expect(url.href).toBe("https://other.org/new?x=2#frag");
    expect(url.hostname).toBe("other.org");
    expect(url.pathname).toBe("/new");
    expect(url.search).toBe("?x=2");
    expect(url.hash).toBe("#frag");
  });

  test("setter with just a different path updates href", () => {
    const url = new URL("https://example.com/old");
    url.href = "https://example.com/new";
    expect(url.href).toBe("https://example.com/new");
  });

  test("setter throws TypeError for invalid URL", () => {
    const url = new URL("https://example.com");
    expect(() => {
      url.href = "not-a-url";
    }).toThrow(TypeError);
  });

  test("href reflects changes made via other setters", () => {
    const url = new URL("https://example.com/path");
    url.pathname = "/updated";
    expect(url.href).toBe("https://example.com/updated");
  });
});
