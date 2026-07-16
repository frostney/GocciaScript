/*---
description: DataView.prototype.getUint16
features: [DataView]
---*/

describe("DataView.prototype.getUint16", () => {
  test("reads big-endian by default and little-endian when requested", () => {
    const buffer = new ArrayBuffer(4);
    const bytes = new Uint8Array(buffer);
    bytes[1] = 0x12;
    bytes[2] = 0x34;
    const view = new DataView(buffer, 1, 2);
    expect(view.getUint16(0)).toBe(0x1234);
    expect(view.getUint16(0, true)).toBe(0x3412);
  });

  test("throws when two bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(2)).getUint16(1)).toThrow(RangeError);
  });
});
