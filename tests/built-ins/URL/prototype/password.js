/*---
description: URL.prototype.password getter and setter
features: [URL]
---*/

describe("URL.prototype.password", () => {
  test("getter returns password from URL", () => {
    const url = new URL("https://user:pass@example.com");
    expect(url.password).toBe("pass");
  });

  test("getter returns empty string when no password", () => {
    const url = new URL("https://example.com");
    expect(url.password).toBe("");
  });

  test("getter returns empty string when only username is present", () => {
    const url = new URL("https://user@example.com");
    expect(url.password).toBe("");
  });

  test("setter updates the password", () => {
    const url = new URL("https://user@example.com");
    url.password = "secret";
    expect(url.password).toBe("secret");
  });

  test("setter encodes special characters in password", () => {
    const url = new URL("https://user@example.com");
    url.password = "p@ss word";
    expect(url.password).toBe("p%40ss%20word");
  });

  test("setter with empty string clears password", () => {
    const url = new URL("https://user:pass@example.com");
    url.password = "";
    expect(url.password).toBe("");
  });

  test("href reflects updated password", () => {
    const url = new URL("https://user@example.com/path");
    url.password = "newpass";
    expect(url.href).toBe("https://user:newpass@example.com/path");
  });
});
