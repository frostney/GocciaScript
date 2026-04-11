/*---
description: URL.prototype.hostname getter and setter
features: [URL]
---*/

describe("URL.prototype.hostname", () => {
  test("getter returns hostname without port", () => {
    const url = new URL("https://example.com:8080/path");
    expect(url.hostname).toBe("example.com");
  });

  test("getter returns hostname for URL without port", () => {
    const url = new URL("https://example.com/path");
    expect(url.hostname).toBe("example.com");
  });

  test("getter returns IP address hostname", () => {
    const url = new URL("https://192.168.1.1/path");
    expect(url.hostname).toBe("192.168.1.1");
  });

  test("setter updates only the hostname, not port", () => {
    const url = new URL("https://example.com:8080/path");
    url.hostname = "other.org";
    expect(url.hostname).toBe("other.org");
    expect(url.port).toBe("8080");
  });

  test("setter does not accept port in hostname", () => {
    const url = new URL("https://example.com/path");
    url.hostname = "other.org:9000";
    // port embedded in hostname string is ignored
    expect(url.hostname).toBe("other.org");
  });

  test("href reflects updated hostname", () => {
    const url = new URL("https://example.com/path");
    url.hostname = "newhost.com";
    expect(url.href).toBe("https://newhost.com/path");
  });

  test("href with port reflects updated hostname", () => {
    const url = new URL("https://example.com:3000/path");
    url.hostname = "other.org";
    expect(url.href).toBe("https://other.org:3000/path");
  });
});
