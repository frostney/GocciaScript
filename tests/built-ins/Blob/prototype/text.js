/*---
description: Blob.prototype.text (File API §3.3.3)
features: [Blob]
---*/

describe("Blob.prototype.text", () => {
  test("returns a promise of the UTF-8 decoded bytes", async () => {
    const p = new Blob(["hé"]).text();
    expect(p instanceof Promise).toBe(true);
    expect(await p).toBe("hé");
  });

  test("drops a leading BOM and replaces invalid sequences", async () => {
    expect(await new Blob(["﻿abc"]).text()).toBe("abc");
    expect(await new Blob(["﻿﻿a"]).text()).toBe("﻿a");
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
