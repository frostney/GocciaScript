/*---
description: DataView.prototype.setFloat64
features: [DataView]
---*/

describe("DataView.prototype.setFloat64", () => {
  test("writes double-precision bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(16);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setFloat64(0, Math.PI)).toBeUndefined();
    expect(view.setFloat64(8, Math.PI, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x40);
    expect(bytes[1]).toBe(0x09);
    expect(bytes[6]).toBe(0x2d);
    expect(bytes[7]).toBe(0x18);
    expect(bytes[8]).toBe(0x18);
    expect(bytes[9]).toBe(0x2d);
    expect(bytes[14]).toBe(0x09);
    expect(bytes[15]).toBe(0x40);
  });

  test("writes negative infinity without relying on the matching getter", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    view.setFloat64(0, -Infinity);
    expect(bytes[0]).toBe(0xff);
    expect(bytes[1]).toBe(0xf0);
    Array.from(bytes).slice(2).forEach((byte) => {
      expect(byte).toBe(0x00);
    });
  });
});
