/*---
description: URL.prototype.username getter and setter
features: [URL]
---*/

describe("URL.prototype.username", () => {
  test("getter returns username from URL", () => {
    const url = new URL("https://user@example.com");
    expect(url.username).toBe("user");
  });

  test("getter returns empty string when no username", () => {
    const url = new URL("https://example.com");
    expect(url.username).toBe("");
  });

  test("getter returns username when both username and password are present", () => {
    const url = new URL("https://user:pass@example.com");
    expect(url.username).toBe("user");
  });

  test("setter updates the username", () => {
    const url = new URL("https://example.com");
    url.username = "newuser";
    expect(url.username).toBe("newuser");
  });

  test("setter encodes special characters in username", () => {
    const url = new URL("https://example.com");
    url.username = "user name";
    expect(url.username).toBe("user%20name");
  });

  test("setter with empty string clears username", () => {
    const url = new URL("https://user@example.com");
    url.username = "";
    expect(url.username).toBe("");
  });

  test("href reflects updated username", () => {
    const url = new URL("https://example.com/path");
    url.username = "admin";
    expect(url.href).toBe("https://admin@example.com/path");
  });
});
