/*---
description: DataView.prototype.getUint32
features: [DataView]
---*/

describe("DataView.prototype.getUint32", () => {
  test("reads big-endian by default and little-endian when requested", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setUint8(0, 0x12);
    view.setUint8(1, 0x34);
    view.setUint8(2, 0x56);
    view.setUint8(3, 0x78);
    expect(view.getUint32(0)).toBe(0x12345678);
    expect(view.getUint32(0, true)).toBe(0x78563412);
  });
});
