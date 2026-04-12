/*---
description: URL.prototype.port getter and setter
features: [URL]
---*/

describe("URL.prototype.port", () => {
  test("getter returns port string when port is present", () => {
    const url = new URL("https://example.com:8080/path");
    expect(url.port).toBe("8080");
  });

  test("getter returns empty string when no port specified", () => {
    const url = new URL("https://example.com/path");
    expect(url.port).toBe("");
  });

  test("default https port 443 returns empty string", () => {
    const url = new URL("https://example.com:443/path");
    expect(url.port).toBe("");
  });

  test("default http port 80 returns empty string", () => {
    const url = new URL("http://example.com:80/path");
    expect(url.port).toBe("");
  });

  test("non-default port on http is returned", () => {
    const url = new URL("http://example.com:8080/path");
    expect(url.port).toBe("8080");
  });

  test("setter updates the port", () => {
    const url = new URL("https://example.com/path");
    url.port = "3000";
    expect(url.port).toBe("3000");
  });

  test("setter with empty string clears port", () => {
    const url = new URL("https://example.com:8080/path");
    url.port = "";
    expect(url.port).toBe("");
  });

  test("href reflects updated port", () => {
    const url = new URL("https://example.com/path");
    url.port = "9090";
    expect(url.href).toBe("https://example.com:9090/path");
  });

  test("setting default port clears it from href", () => {
    const url = new URL("https://example.com:8080/path");
    url.port = "443";
    expect(url.port).toBe("");
    expect(url.href).toBe("https://example.com/path");
  });
});
