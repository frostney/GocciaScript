/*---
description: Response.prototype.text
features: [fetch]
---*/

describe("Response.prototype.text", () => {
  test("returns a promise", () => {
    const r = new Response();
    const p = r.text();
    expect(p).toHaveProperty("then");
  });

  test("resolves to empty string for empty body", async () => {
    const r = new Response();
    const text = await r.text();
    expect(text).toBe("");
  });

  test("sets bodyUsed to true after consumption", async () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
    await r.text();
    expect(r.bodyUsed).toBe(true);
  });

  test.runIf(typeof Goccia !== "undefined")("releases its native body after consumption", async () => {
    const body = "x".repeat(64 * 1024);
    const response = new Response(body);
    const held = Goccia.gc.bytesAllocated;
    const consume = async () => {
      await response.text();
    };
    await consume();
    Goccia.gc();
    expect(response.bodyUsed).toBe(true);
    expect(held - Goccia.gc.bytesAllocated).toBeGreaterThan(body.length / 2);
  });

  test("rejects with TypeError on second consumption", async () => {
    const r = new Response();
    await r.text();
    let caught;
    await r.text().catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
