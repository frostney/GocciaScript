/*---
description: DataView.prototype.setInt16
features: [DataView]
---*/

describe("DataView.prototype.setInt16", () => {
  test("writes signed 16-bit bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setInt16(0, -32767)).toBeUndefined();
    expect(view.setInt16(2, -32767, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x80);
    expect(bytes[1]).toBe(0x01);
    expect(bytes[2]).toBe(0x01);
    expect(bytes[3]).toBe(0x80);
  });
});
