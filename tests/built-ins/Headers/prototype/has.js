/*---
description: Headers.prototype.has
features: [fetch]
---*/

describe("Headers.prototype.has", () => {
  test("returns true for existing header", () => {
    const h = new Headers({ Accept: "text/html" });
    expect(h.has("Accept")).toBe(true);
  });

  test("is case-insensitive", () => {
    const h = new Headers({ Accept: "text/html" });
    expect(h.has("accept")).toBe(true);
    expect(h.has("ACCEPT")).toBe(true);
  });

  test("returns false for missing header", () => {
    const h = new Headers();
    expect(h.has("x-missing")).toBe(false);
  });
});
