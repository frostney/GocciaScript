/*---
description: DataView.prototype.setUint32
features: [DataView]
---*/

describe("DataView.prototype.setUint32", () => {
  test("writes big-endian by default and little-endian when requested", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setUint32(0, 0x12345678);
    view.setUint32(4, 0x12345678, true);
    expect(view.getUint32(0)).toBe(0x12345678);
    expect(view.getUint32(4, true)).toBe(0x12345678);
  });

  test("wraps large finite numbers modulo 2^32", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setUint32(0, 1e20);
    expect(view.getUint32(0)).toBe(1661992960);
  });
});
