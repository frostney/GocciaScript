/*---
description: Headers constructor
features: [fetch]
---*/

describe("Headers constructor", () => {
  test("creates empty Headers with no arguments", () => {
    const h = new Headers();
    expect(h).toBeInstanceOf(Headers);
    expect(h.has("x")).toBe(false);
  });

  test("creates Headers from a plain object", () => {
    const h = new Headers({ "Content-Type": "text/html", Accept: "application/json" });
    expect(h.get("content-type")).toBe("text/html");
    expect(h.get("accept")).toBe("application/json");
  });

  test("creates Headers from another Headers instance", () => {
    const original = new Headers({ "X-Custom": "value" });
    const copy = new Headers(original);
    expect(copy.get("x-custom")).toBe("value");
  });

  test("toString tag is Headers", () => {
    const h = new Headers();
    expect(Object.prototype.toString.call(h)).toBe("[object Headers]");
  });
});
