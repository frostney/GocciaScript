/*---
description: Blob.prototype.arrayBuffer (File API §3.3.4)
features: [Blob]
---*/

describe("Blob.prototype.arrayBuffer", () => {
  test("resolves to a new ArrayBuffer copy each time", async () => {
    const b = new Blob([new Uint8Array([1, 2, 3])]);
    const a1 = await b.arrayBuffer();
    const a2 = await b.arrayBuffer();
    expect(a1 instanceof ArrayBuffer).toBe(true);
    expect(a1).not.toBe(a2);
    new Uint8Array(a1)[0] = 9;
    expect(Array.from(new Uint8Array(await b.arrayBuffer()))).toEqual([1, 2, 3]);
  });

  test("an empty Blob is an empty buffer", async () => {
    expect((await new Blob().arrayBuffer()).byteLength).toBe(0);
  });

  test("a receiver that is not a Blob rejects with a TypeError instead of throwing", async () => {
    const p = Blob.prototype.arrayBuffer.call({});
    expect(p instanceof Promise).toBe(true);
    let caught;
    await p.catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
