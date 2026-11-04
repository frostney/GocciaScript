/*---
description: Blob.prototype.bytes (File API §3.3.5)
features: [Blob]
---*/

describe("Blob.prototype.bytes", () => {
  test("is an enumerable, writable, configurable method", () => {
    const desc = Object.getOwnPropertyDescriptor(Blob.prototype, "bytes");
    expect(typeof desc.value).toBe("function");
    expect(desc.enumerable).toBe(true);
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.value.length).toBe(0);
  });

  test("resolves to a Uint8Array over a buffer of its own", async () => {
    const u = await new Blob(["AB"]).bytes();
    expect(u instanceof Uint8Array).toBe(true);
    expect(Array.from(u)).toEqual([65, 66]);
    expect(u.byteOffset).toBe(0);
    expect(u.buffer.byteLength).toBe(2);
  });

  test("an empty Blob is an empty array", async () => {
    expect((await new Blob().bytes()).length).toBe(0);
  });

  test("a receiver that is not a Blob rejects with a TypeError instead of throwing", async () => {
    const p = Blob.prototype.bytes.call({});
    expect(p instanceof Promise).toBe(true);
    let caught;
    await p.catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
