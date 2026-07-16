/*---
description: DataView.prototype.getFloat32
features: [DataView]
---*/

describe("DataView.prototype.getFloat32", () => {
  test("reads independent IEEE 754 single-precision bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(6);
    const bytes = new Uint8Array(buffer);
    const fixture = [0x3f, 0xc0, 0x00, 0x00];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 4);

    expect(view.getFloat32(0)).toBe(1.5);
    expect(view.getFloat32(0, true)).toBeCloseTo(6.896490392174587e-41, 45);
  });

  test("reads special single-precision values and preserves negative zero", () => {
    const buffer = new ArrayBuffer(12);
    const bytes = new Uint8Array(buffer);
    const fixture = [
      0x80, 0x00, 0x00, 0x00,
      0x7f, 0x80, 0x00, 0x00,
      0x7f, 0xc0, 0x00, 0x00,
    ];
    fixture.forEach((byte, index) => {
      bytes[index] = byte;
    });
    const view = new DataView(buffer);

    expect(Object.is(view.getFloat32(0), -0)).toBe(true);
    expect(view.getFloat32(4)).toBe(Infinity);
    expect(view.getFloat32(8)).toBeNaN();
  });

  test("throws when four bytes are not available", () => {
    expect(() => new DataView(new ArrayBuffer(4)).getFloat32(1)).toThrow(RangeError);
  });
});
