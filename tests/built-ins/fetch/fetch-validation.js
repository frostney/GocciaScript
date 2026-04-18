/*---
description: fetch argument validation and method restrictions
features: [fetch]
---*/

describe("fetch", () => {
  test("is a function", () => {
    expect(typeof fetch).toBe("function");
  });

  test("throws TypeError when called with no arguments", () => {
    expect(() => fetch()).toThrow(TypeError);
  });

  test("throws TypeError for POST method", () => {
    expect(() => fetch("http://example.com", { method: "POST" })).toThrow(TypeError);
  });

  test("throws TypeError for PUT method", () => {
    expect(() => fetch("http://example.com", { method: "PUT" })).toThrow(TypeError);
  });

  test("throws TypeError for DELETE method", () => {
    expect(() => fetch("http://example.com", { method: "DELETE" })).toThrow(TypeError);
  });

  test("throws TypeError for PATCH method", () => {
    expect(() => fetch("http://example.com", { method: "PATCH" })).toThrow(TypeError);
  });

  test("returns a thenable for GET", () => {
    const p = fetch("http://0.0.0.0:1/");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
    expect(typeof p.catch).toBe("function");
  });

  test("rejected promise for invalid host", async () => {
    let caught = false;
    await fetch("http://0.0.0.0:1/").catch((e) => {
      caught = true;
      expect(e.name).toBe("TypeError");
      expect(typeof e.message).toBe("string");
    });
    expect(caught).toBe(true);
  });

  test("rejected promise for unsupported scheme", async () => {
    let caught = false;
    await fetch("ftp://example.com").catch((e) => {
      caught = true;
      expect(e.name).toBe("TypeError");
    });
    expect(caught).toBe(true);
  });

  test("accepts URL object as first argument", () => {
    const url = new URL("http://0.0.0.0:1/");
    const p = fetch(url);
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("accepts method option case-insensitively", () => {
    const p = fetch("http://0.0.0.0:1/", { method: "get" });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("accepts head method", () => {
    const p = fetch("http://0.0.0.0:1/", { method: "HEAD" });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("accepts headers as plain object", () => {
    const p = fetch("http://0.0.0.0:1/", {
      headers: { "X-Custom": "value" },
    });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("accepts headers as Headers instance", () => {
    const h = new Headers({ "X-Custom": "value" });
    const p = fetch("http://0.0.0.0:1/", { headers: h });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });
});
