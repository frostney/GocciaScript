/*---
description: Response.prototype.json
features: [fetch]
---*/

describe("Response.prototype.json", () => {
  test("returns a promise", () => {
    const r = new Response();
    const p = r.json();
    expect(p).toHaveProperty("then");
    // An empty body is not valid JSON; the rejection is not what is under test.
    p.catch(() => {});
  });

  test("sets bodyUsed to true after consumption", async () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
    // Empty body will fail JSON parse, but bodyUsed should still be set
    try { await r.json(); } catch {}
    expect(r.bodyUsed).toBe(true);
  });

  test.runIf(typeof Goccia !== "undefined")("releases its native body after consumption", async () => {
    const body = `{"value":1}${" ".repeat(64 * 1024)}`;
    const response = new Response(body);
    const held = Goccia.gc.bytesAllocated;
    expect(await response.json()).toEqual({ value: 1 });
    Goccia.gc();
    expect(response.bodyUsed).toBe(true);
    expect(held - Goccia.gc.bytesAllocated).toBeGreaterThan(body.length / 2);
  });

  test("rejects with TypeError on second consumption", async () => {
    const r = new Response();
    try { await r.json(); } catch {}
    let caught;
    await r.json().catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
