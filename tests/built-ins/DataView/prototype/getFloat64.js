/*---
description: DataView.prototype.getFloat64
features: [DataView]
---*/

describe("DataView.prototype.getFloat64", () => {
  test("reads independent IEEE 754 double-precision bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(10);
    const bytes = new Uint8Array(buffer);
    const fixture = [0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 8);

    expect(view.getFloat64(0)).toBe(Math.PI);
    expect(view.getFloat64(0, true)).not.toBe(Math.PI);
  });

  test("reads special double-precision values and preserves negative zero", () => {
    const buffer = new ArrayBuffer(24);
    const bytes = new Uint8Array(buffer);
    const fixture = [
      0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];
    fixture.forEach((byte, index) => {
      bytes[index] = byte;
    });
    const view = new DataView(buffer);

    expect(Object.is(view.getFloat64(0), -0)).toBe(true);
    expect(view.getFloat64(8)).toBe(Infinity);
    expect(view.getFloat64(16)).toBeNaN();
  });

  test("throws when eight bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(8)).getFloat64(1)).toThrow(RangeError);
  });
});
