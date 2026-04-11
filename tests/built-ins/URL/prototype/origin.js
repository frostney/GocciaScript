/*---
description: URL.prototype.origin getter
features: [URL]
---*/

describe("URL.prototype.origin", () => {
  test("https URL origin is scheme + host", () => {
    const url = new URL("https://example.com/path");
    expect(url.origin).toBe("https://example.com");
  });

  test("https URL with non-default port includes port", () => {
    const url = new URL("https://example.com:8080/path");
    expect(url.origin).toBe("https://example.com:8080");
  });

  test("https URL on default port 443 omits port", () => {
    const url = new URL("https://example.com:443/path");
    expect(url.origin).toBe("https://example.com");
  });

  test("http URL origin is scheme + host", () => {
    const url = new URL("http://example.com/path");
    expect(url.origin).toBe("http://example.com");
  });

  test("http URL on default port 80 omits port", () => {
    const url = new URL("http://example.com:80/path");
    expect(url.origin).toBe("http://example.com");
  });

  test("http URL with non-default port includes port", () => {
    const url = new URL("http://example.com:8080/path");
    expect(url.origin).toBe("http://example.com:8080");
  });

  test("file URL has opaque origin of null", () => {
    const url = new URL("file:///home/user/file.txt");
    expect(url.origin).toBe("null");
  });

  test("data URL has opaque origin of null", () => {
    const url = new URL("data:text/plain,hello");
    expect(url.origin).toBe("null");
  });
});
