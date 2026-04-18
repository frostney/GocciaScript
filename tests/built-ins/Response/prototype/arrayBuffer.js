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
