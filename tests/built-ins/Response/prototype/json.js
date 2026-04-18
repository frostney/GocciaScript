/*---
description: Response.prototype.json
features: [fetch]
---*/

describe("Response.prototype.json", () => {
  test("returns a promise", () => {
    const r = new Response();
    const p = r.json();
    expect(p).toHaveProperty("then");
  });

  test("sets bodyUsed to true after consumption", async () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
    // Empty body will fail JSON parse, but bodyUsed should still be set
    try { await r.json(); } catch {}
    expect(r.bodyUsed).toBe(true);
  });

  test("throws TypeError on second consumption", async () => {
    const r = new Response();
    try { await r.json(); } catch {}
    expect(() => r.json()).toThrow(TypeError);
  });
});
