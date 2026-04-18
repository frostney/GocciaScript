/*---
description: Response constructor and property accessors
features: [fetch]
---*/

describe("Response constructor", () => {
  test("creates a Response instance", () => {
    const r = new Response();
    expect(r).toBeInstanceOf(Response);
  });

  test("has default status 200", () => {
    const r = new Response();
    expect(r.status).toBe(200);
  });

  test("ok is true for default status", () => {
    const r = new Response();
    expect(r.ok).toBe(true);
  });

  test("type is basic", () => {
    const r = new Response();
    expect(r.type).toBe("basic");
  });

  test("bodyUsed is initially false", () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
  });

  test("redirected is initially false", () => {
    const r = new Response();
    expect(r.redirected).toBe(false);
  });

  test("toString tag is Response", () => {
    const r = new Response();
    expect(Object.prototype.toString.call(r)).toBe("[object Response]");
  });
});
