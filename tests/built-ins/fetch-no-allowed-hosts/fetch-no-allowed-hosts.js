/*---
description: fetch throws PermissionDenied when the net capability is not granted
features: [fetch]
---*/

describe("fetch without allowed hosts", () => {
  test("throws PermissionDenied when no allowed hosts are configured", () => {
    expect(() => fetch("http://example.com")).toThrow(PermissionDenied);
  });

  test("error message names the capability and the host", () => {
    let caught = false;
    try {
      fetch("http://example.com");
    } catch (e) {
      caught = true;
      expect(e.message).toBe("net: example.com");
      expect(e.capability).toBe("net");
      expect(e.scope).toBe("example.com");
    }
    expect(caught).toBe(true);
  });

  test("a non-default port is part of the denied scope", () => {
    let message = "";
    try {
      fetch("http://localhost:3000/api");
    } catch (e) {
      message = e.message;
    }
    expect(message).toBe("net: localhost:3000");
  });

  test("throws PermissionDenied with URL object when no hosts allowed", () => {
    const url = new URL("http://example.com/path");
    expect(() => fetch(url)).toThrow(PermissionDenied);
  });

  test("PermissionDenied is an Error and never names the request path", () => {
    let caught;
    try {
      fetch("http://example.com/private?token=abc");
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof PermissionDenied).toBe(true);
    expect(caught instanceof Error).toBe(true);
    expect(caught.name).toBe("PermissionDenied");
    expect(caught.message).not.toContain("private");
    expect(caught.message).not.toContain("token");
  });
});
