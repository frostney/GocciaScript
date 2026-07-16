/*---
description: DataView.prototype.getBigUint64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.getBigUint64", () => {
  test("reads unsigned BigInt values from independent bytes in both byte orders", () => {
    const buffer = new ArrayBuffer(10);
    const bytes = new Uint8Array(buffer);
    const fixture = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 8);

    expect(view.getBigUint64(0)).toBe(0x0102030405060708n);
    expect(view.getBigUint64(0, true)).toBe(0x0807060504030201n);
  });

  test("throws when eight bytes are not available", () => {
    const view = new DataView(new ArrayBuffer(8));
    expect(() => view.getBigUint64(1)).toThrow(RangeError);
  });
});
