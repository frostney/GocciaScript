/*---
description: DataView.prototype.setFloat16
features: [DataView, Float16Array]
---*/

describe("DataView.prototype.setFloat16", () => {
  test("writes half-precision bytes in both byte orders and returns undefined", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setFloat16(0, 1.5)).toBeUndefined();
    expect(view.setFloat16(2, 1.5, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x3e);
    expect(bytes[1]).toBe(0x00);
    expect(bytes[2]).toBe(0x00);
    expect(bytes[3]).toBe(0x3e);
  });

  test("rounds overflow to infinity and preserves negative zero", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    view.setFloat16(0, 65520);
    view.setFloat16(2, -0);
    expect(bytes[0]).toBe(0x7c);
    expect(bytes[1]).toBe(0x00);
    expect(bytes[2]).toBe(0x80);
    expect(bytes[3]).toBe(0x00);
  });
});
