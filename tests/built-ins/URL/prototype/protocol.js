/*---
description: URL.prototype.protocol getter and setter
features: [URL]
---*/

describe("URL.prototype.protocol", () => {
  test("getter returns scheme with colon for https", () => {
    const url = new URL("https://example.com");
    expect(url.protocol).toBe("https:");
  });

  test("getter returns scheme with colon for http", () => {
    const url = new URL("http://example.com");
    expect(url.protocol).toBe("http:");
  });

  test("getter returns scheme with colon for ftp", () => {
    const url = new URL("ftp://example.com/file");
    expect(url.protocol).toBe("ftp:");
  });

  test("setter updates the scheme", () => {
    const url = new URL("https://example.com/path");
    url.protocol = "http:";
    expect(url.protocol).toBe("http:");
    expect(url.href).toBe("http://example.com/path");
  });

  test("setter accepts scheme without colon", () => {
    const url = new URL("https://example.com/path");
    url.protocol = "http";
    expect(url.protocol).toBe("http:");
  });

  test("setter updates origin when protocol changes", () => {
    const url = new URL("https://example.com");
    url.protocol = "http:";
    expect(url.origin).toBe("http://example.com");
  });
});
