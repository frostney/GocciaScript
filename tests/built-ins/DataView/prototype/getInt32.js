/*---
description: DataView.prototype.getInt32
features: [DataView]
---*/

describe("DataView.prototype.getInt32", () => {
  test("reads signed 32-bit values from independent bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(6);
    const bytes = new Uint8Array(buffer);
    const fixture = [0x80, 0x00, 0x00, 0x01];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 4);

    expect(view.getInt32(0)).toBe(-2147483647);
    expect(view.getInt32(0, true)).toBe(16777344);
  });

  test("throws when four bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(4)).getInt32(1)).toThrow(RangeError);
  });
});
