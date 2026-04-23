/*---
description: fetch enforces allowed hosts list
features: [fetch]
---*/

describe("fetch allowed hosts", () => {
  test("throws TypeError for host not in allowed list", () => {
    expect(() => fetch("http://blocked.example.com")).toThrow(TypeError);
  });

  test("error message mentions blocked host", () => {
    let caught = false;
    try {
      fetch("http://blocked.example.com");
    } catch (e) {
      caught = true;
      expect(e.message).toContain("blocked.example.com");
    }
    expect(caught).toBe(true);
  });

  test("throws TypeError for HTTPS host not in allowed list", () => {
    expect(() => fetch("https://not-allowed.test")).toThrow(TypeError);
  });

  test("allowed host proceeds to network request", () => {
    // 0.0.0.0:1 is in the allowed list but unreachable, so the promise
    // rejects with a network error rather than a host-not-allowed TypeError.
    const p = fetch("http://0.0.0.0:1/");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("host matching is case-insensitive", () => {
    // example.com is in the allowed list; EXAMPLE.COM should also work.
    const p = fetch("http://EXAMPLE.COM:1/");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("port does not affect host matching", () => {
    // example.com is allowed regardless of port
    const p = fetch("http://example.com:8080/");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("path does not affect host matching", () => {
    const p = fetch("http://0.0.0.0:1/some/deep/path?q=1");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("userinfo does not affect host matching", () => {
    const p = fetch("http://user:pass@0.0.0.0:1/");
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });
});
