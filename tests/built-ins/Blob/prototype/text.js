/*---
description: Blob.prototype.text (File API §3.3.3)
features: [Blob]
---*/

describe("Blob.prototype.text", () => {
  test("is an enumerable, writable, configurable method", () => {
    const desc = Object.getOwnPropertyDescriptor(Blob.prototype, "text");
    expect(typeof desc.value).toBe("function");
    expect(desc.enumerable).toBe(true);
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.value.length).toBe(0);
  });

  test("returns a promise of the UTF-8 decoded bytes", async () => {
    const p = new Blob(["hé"]).text();
    expect(p instanceof Promise).toBe(true);
    expect(await p).toBe("hé");
  });

  test("drops a leading BOM and replaces invalid sequences", async () => {
    expect(await new Blob(["\uFEFFabc"]).text()).toBe("abc");
    expect(await new Blob(["\uFEFF\uFEFFa"]).text()).toBe("\uFEFFa");
    expect(await new Blob([new Uint8Array([0x61, 0xff, 0x62])]).text()).toBe("a�b");
  });

  test("an empty Blob is the empty string", async () => {
    expect(await new Blob().text()).toBe("");
  });

  test("a receiver that is not a Blob rejects with a TypeError instead of throwing", async () => {
    const p = Blob.prototype.text.call({});
    expect(p instanceof Promise).toBe(true);
    let caught;
    await p.catch((e) => {
      caught = e;
    });
    expect(caught instanceof TypeError).toBe(true);
  });
});
