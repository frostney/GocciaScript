/*---
description: DataView.prototype.setUint8
features: [DataView]
---*/

describe("DataView.prototype.setUint8", () => {
  test("writes one unsigned byte", () => {
    const buffer = new ArrayBuffer(1);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);
    expect(view.setUint8(0, 511)).toBeUndefined();
    expect(bytes[0]).toBe(255);
  });
});
