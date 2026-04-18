/*---
description: Headers.prototype.get
features: [fetch]
---*/

describe("Headers.prototype.get", () => {
  test("returns value for existing header", () => {
    const h = new Headers({ "Content-Type": "text/plain" });
    expect(h.get("Content-Type")).toBe("text/plain");
  });

  test("is case-insensitive", () => {
    const h = new Headers({ "Content-Type": "text/plain" });
    expect(h.get("content-type")).toBe("text/plain");
    expect(h.get("CONTENT-TYPE")).toBe("text/plain");
  });

  test("returns null for missing header", () => {
    const h = new Headers();
    expect(h.get("x-missing")).toBeNull();
  });
});
