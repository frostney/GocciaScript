/*---
description: DataView.prototype.getUint16
features: [DataView]
---*/

describe("DataView.prototype.getUint16", () => {
  test("reads big-endian by default and little-endian when requested", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setUint8(0, 0x12);
    view.setUint8(1, 0x34);
    expect(view.getUint16(0)).toBe(0x1234);
    expect(view.getUint16(0, true)).toBe(0x3412);
  });
});
