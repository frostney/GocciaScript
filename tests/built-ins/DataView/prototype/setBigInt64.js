/*---
description: DataView.prototype.setBigInt64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.setBigInt64", () => {
  test("writes signed BigInt bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(16);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setBigInt64(0, -2n)).toBeUndefined();
    expect(view.setBigInt64(8, -2n, true)).toBeUndefined();
    expect(bytes[0]).toBe(0xff);
    expect(bytes[6]).toBe(0xff);
    expect(bytes[7]).toBe(0xfe);
    expect(bytes[8]).toBe(0xfe);
    expect(bytes[9]).toBe(0xff);
    expect(bytes[15]).toBe(0xff);
  });

  test("requires a BigInt value before range validation", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(() => view.setBigInt64(1, 1)).toThrow(TypeError);
  });

  test("throws RangeError after converting a valid BigInt value", () => {
    const view = new DataView(new ArrayBuffer(8));
    expect(() => view.setBigInt64(1, 1n)).toThrow(RangeError);
  });
});
