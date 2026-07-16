/*---
description: DataView.prototype.getBigInt64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.getBigInt64", () => {
  test("reads signed BigInt values from independent bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(10);
    const bytes = new Uint8Array(buffer);
    const fixture = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 8);

    expect(view.getBigInt64(0)).toBe(-2n);
    expect(view.getBigInt64(0, true)).toBe(-72057594037927937n);
  });

  test("throws when eight bytes are not available", () => {
    const view = new DataView(new ArrayBuffer(8));
    expect(() => view.getBigInt64(1)).toThrow(RangeError);
  });
});
