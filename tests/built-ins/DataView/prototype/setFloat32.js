/*---
description: DataView.prototype.setFloat32
features: [DataView]
---*/

describe("DataView.prototype.setFloat32", () => {
  test("writes single-precision bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setFloat32(0, 1.5)).toBeUndefined();
    expect(view.setFloat32(4, 1.5, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x3f);
    expect(bytes[1]).toBe(0xc0);
    expect(bytes[2]).toBe(0x00);
    expect(bytes[3]).toBe(0x00);
    expect(bytes[4]).toBe(0x00);
    expect(bytes[5]).toBe(0x00);
    expect(bytes[6]).toBe(0xc0);
    expect(bytes[7]).toBe(0x3f);
  });

  test("preserves negative zero in the written bytes", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    view.setFloat32(0, -0);
    expect(bytes[0]).toBe(0x80);
    expect(bytes[1]).toBe(0x00);
    expect(bytes[2]).toBe(0x00);
    expect(bytes[3]).toBe(0x00);
  });
});
