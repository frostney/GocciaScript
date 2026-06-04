/*---
description: DataView.prototype.setUint16
features: [DataView]
---*/

describe("DataView.prototype.setUint16", () => {
  test("writes big-endian by default and little-endian when requested", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setUint16(0, 0x1234);
    view.setUint16(2, 0x1234, true);
    expect(view.getUint8(0)).toBe(0x12);
    expect(view.getUint8(1)).toBe(0x34);
    expect(view.getUint8(2)).toBe(0x34);
    expect(view.getUint8(3)).toBe(0x12);
  });
});
