/*---
description: URL.prototype.host getter and setter
features: [URL]
---*/

describe("URL.prototype.host", () => {
  test("getter returns hostname without port when no port", () => {
    const url = new URL("https://example.com/path");
    expect(url.host).toBe("example.com");
  });

  test("getter returns hostname:port when port is present", () => {
    const url = new URL("https://example.com:8080/path");
    expect(url.host).toBe("example.com:8080");
  });

  test("getter omits default https port 443", () => {
    const url = new URL("https://example.com:443/path");
    expect(url.host).toBe("example.com");
  });

  test("getter omits default http port 80", () => {
    const url = new URL("http://example.com:80/path");
    expect(url.host).toBe("example.com");
  });

  test("setter updates hostname", () => {
    const url = new URL("https://example.com/path");
    url.host = "other.org";
    expect(url.hostname).toBe("other.org");
    expect(url.port).toBe("");
  });

  test("setter updates hostname and port together", () => {
    const url = new URL("https://example.com/path");
    url.host = "other.org:9000";
    expect(url.hostname).toBe("other.org");
    expect(url.port).toBe("9000");
  });

  test("href reflects updated host", () => {
    const url = new URL("https://example.com/path");
    url.host = "newhost.com:3000";
    expect(url.href).toBe("https://newhost.com:3000/path");
  });
});
