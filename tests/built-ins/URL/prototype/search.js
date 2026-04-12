/*---
description: URL.prototype.search getter and setter
features: [URL]
---*/

describe("URL.prototype.search", () => {
  test("getter returns query string with leading question mark", () => {
    const url = new URL("https://example.com/path?q=1&r=2");
    expect(url.search).toBe("?q=1&r=2");
  });

  test("getter returns empty string when no query", () => {
    const url = new URL("https://example.com/path");
    expect(url.search).toBe("");
  });

  test("getter does not include the hash fragment", () => {
    const url = new URL("https://example.com/path?q=1#hash");
    expect(url.search).toBe("?q=1");
  });

  test("setter updates the search string", () => {
    const url = new URL("https://example.com/path");
    url.search = "?key=value";
    expect(url.search).toBe("?key=value");
  });

  test("setter with string without leading question mark adds it", () => {
    const url = new URL("https://example.com/path");
    url.search = "key=value";
    expect(url.search).toBe("?key=value");
  });

  test("setter with empty string clears the query", () => {
    const url = new URL("https://example.com/path?q=1");
    url.search = "";
    expect(url.search).toBe("");
  });

  test("href reflects updated search", () => {
    const url = new URL("https://example.com/path?old=1");
    url.search = "new=2";
    expect(url.href).toBe("https://example.com/path?new=2");
  });

  test("clearing search removes question mark from href", () => {
    const url = new URL("https://example.com/path?q=1");
    url.search = "";
    expect(url.href).toBe("https://example.com/path");
  });
});
