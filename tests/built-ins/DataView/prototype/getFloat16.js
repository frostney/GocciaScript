/*---
description: DataView.prototype.getFloat16
features: [DataView, Float16Array]
---*/

describe("DataView.prototype.getFloat16", () => {
  test("reads independent IEEE 754 half-precision bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(4);
    const bytes = new Uint8Array(buffer);
    bytes[1] = 0x3e;
    bytes[2] = 0x00;
    const view = new DataView(buffer, 1, 2);

    expect(view.getFloat16(0)).toBe(1.5);
    expect(view.getFloat16(0, true)).toBeCloseTo(0.0000036954879760742188, 10);
  });

  test("reads special half-precision values and preserves negative zero", () => {
    const buffer = new ArrayBuffer(6);
    const bytes = new Uint8Array(buffer);
    bytes[0] = 0x80;
    bytes[1] = 0x00;
    bytes[2] = 0x7c;
    bytes[3] = 0x00;
    bytes[4] = 0x7e;
    bytes[5] = 0x00;
    const view = new DataView(buffer);

    expect(Object.is(view.getFloat16(0), -0)).toBe(true);
    expect(view.getFloat16(2)).toBe(Infinity);
    expect(view.getFloat16(4)).toBeNaN();
  });

  test("throws when two bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(2)).getFloat16(1)).toThrow(RangeError);
  });
});
