/*---
description: URL.prototype.pathname getter and setter
features: [URL]
---*/

describe("URL.prototype.pathname", () => {
  test("getter returns the path", () => {
    const url = new URL("https://example.com/foo/bar");
    expect(url.pathname).toBe("/foo/bar");
  });

  test("getter returns slash for bare origin URL", () => {
    const url = new URL("https://example.com");
    expect(url.pathname).toBe("/");
  });

  test("getter returns path with trailing slash", () => {
    const url = new URL("https://example.com/path/");
    expect(url.pathname).toBe("/path/");
  });

  test("getter does not include search or hash", () => {
    const url = new URL("https://example.com/path?q=1#hash");
    expect(url.pathname).toBe("/path");
  });

  test("setter updates the path", () => {
    const url = new URL("https://example.com/old");
    url.pathname = "/new/path";
    expect(url.pathname).toBe("/new/path");
  });

  test("setter adds leading slash if omitted", () => {
    const url = new URL("https://example.com/old");
    url.pathname = "new/path";
    expect(url.pathname).toBe("/new/path");
  });

  test("href reflects updated pathname", () => {
    const url = new URL("https://example.com/old?q=1");
    url.pathname = "/new";
    expect(url.href).toBe("https://example.com/new?q=1");
  });

  test("setting empty pathname results in slash", () => {
    const url = new URL("https://example.com/path");
    url.pathname = "/";
    expect(url.pathname).toBe("/");
  });
});
