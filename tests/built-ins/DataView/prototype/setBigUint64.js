/*---
description: DataView.prototype.setBigUint64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.setBigUint64", () => {
  test("writes unsigned BigInt bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(16);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);
    const value = 0x0102030405060708n;

    expect(view.setBigUint64(0, value)).toBeUndefined();
    expect(view.setBigUint64(8, value, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x01);
    expect(bytes[1]).toBe(0x02);
    expect(bytes[6]).toBe(0x07);
    expect(bytes[7]).toBe(0x08);
    expect(bytes[8]).toBe(0x08);
    expect(bytes[9]).toBe(0x07);
    expect(bytes[14]).toBe(0x02);
    expect(bytes[15]).toBe(0x01);
  });

  test("wraps BigInt values modulo two to the power of 64", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    view.setBigUint64(0, -1n);
    bytes.forEach((byte) => {
      expect(byte).toBe(0xff);
    });
  });

  test("rejects Number values", () => {
    expect(() => new DataView(new ArrayBuffer(8)).setBigUint64(0, 1)).toThrow(TypeError);
  });
});
