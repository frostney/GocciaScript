/*---
description: DataView.prototype.getInt16
features: [DataView]
---*/

describe("DataView.prototype.getInt16", () => {
  test("reads signed 16-bit values from independent bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(4);
    const bytes = new Uint8Array(buffer);
    bytes[1] = 0x80;
    bytes[2] = 0x01;
    const view = new DataView(buffer, 1, 2);

    expect(view.getInt16(0)).toBe(-32767);
    expect(view.getInt16(0, true)).toBe(384);
  });

  test("throws when two bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(2)).getInt16(1)).toThrow(RangeError);
  });
});
