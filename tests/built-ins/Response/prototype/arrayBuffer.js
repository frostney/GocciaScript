/*---
description: Response.prototype.arrayBuffer
features: [fetch]
---*/

describe("Response.prototype.arrayBuffer", () => {
  test("returns a promise", () => {
    const r = new Response();
    const p = r.arrayBuffer();
    expect(p).toHaveProperty("then");
  });

  test("resolves to an ArrayBuffer", async () => {
    const r = new Response();
    const buf = await r.arrayBuffer();
    expect(buf).toBeInstanceOf(ArrayBuffer);
  });

  test("resolves to zero-length ArrayBuffer for empty body", async () => {
    const r = new Response();
    const buf = await r.arrayBuffer();
    expect(buf.byteLength).toBe(0);
  });

  test("sets bodyUsed to true after consumption", async () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
    await r.arrayBuffer();
    expect(r.bodyUsed).toBe(true);
  });

  test.runIf(typeof Goccia !== "undefined")("releases its native body after consumption", async () => {
    const body = "x".repeat(64 * 1024);
    const response = new Response(body);
    const held = Goccia.gc.bytesAllocated;
    const buffer = await response.arrayBuffer();
    expect(buffer.byteLength).toBe(body.length);
    Goccia.gc();
    expect(response.bodyUsed).toBe(true);
    expect(Goccia.gc.bytesAllocated).toBeLessThan(held + body.length / 2);
  });

  test("rejects with TypeError on second consumption", async () => {
    const r = new Response();
    await r.arrayBuffer();
    let caught;
    await r.arrayBuffer().catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
