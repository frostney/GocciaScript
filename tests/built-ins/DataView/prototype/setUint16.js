/*---
description: DataView.prototype.setUint16
features: [DataView]
---*/

describe("DataView.prototype.setUint16", () => {
  test("writes big-endian by default and little-endian when requested", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);
    expect(view.setUint16(0, 0x1234)).toBeUndefined();
    expect(view.setUint16(2, 0x1234, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x12);
    expect(bytes[1]).toBe(0x34);
    expect(bytes[2]).toBe(0x34);
    expect(bytes[3]).toBe(0x12);
  });
});
