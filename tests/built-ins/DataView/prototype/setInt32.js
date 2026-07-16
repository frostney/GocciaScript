/*---
description: DataView.prototype.setInt32
features: [DataView]
---*/

describe("DataView.prototype.setInt32", () => {
  test("writes signed 32-bit bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setInt32(0, -2147483647)).toBeUndefined();
    expect(view.setInt32(4, -2147483647, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x80);
    expect(bytes[1]).toBe(0x00);
    expect(bytes[2]).toBe(0x00);
    expect(bytes[3]).toBe(0x01);
    expect(bytes[4]).toBe(0x01);
    expect(bytes[5]).toBe(0x00);
    expect(bytes[6]).toBe(0x00);
    expect(bytes[7]).toBe(0x80);
  });
});
