/*---
description: URL.prototype.toJSON
features: [URL]
---*/

describe("URL.prototype.toJSON", () => {
  test("toJSON returns the same value as href", () => {
    const url = new URL("https://example.com/path?q=1#hash");
    expect(url.toJSON()).toBe(url.href);
  });

  test("toJSON for bare origin URL", () => {
    const url = new URL("https://example.com");
    expect(url.toJSON()).toBe("https://example.com/");
  });

  test("toJSON equals toString", () => {
    const url = new URL("https://example.com/path?q=1");
    expect(url.toJSON()).toBe(url.toString());
  });

  test("toJSON after modifying the URL", () => {
    const url = new URL("https://example.com/old");
    url.pathname = "/new";
    url.search = "updated=true";
    expect(url.toJSON()).toBe(url.href);
  });

  test("JSON.stringify uses toJSON", () => {
    const url = new URL("https://example.com/path");
    const obj = { url };
    const parsed = JSON.parse(JSON.stringify(obj));
    expect(parsed.url).toBe(url.href);
  });
});
