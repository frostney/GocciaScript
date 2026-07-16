/*---
description: DataView.prototype.getUint8
features: [DataView]
---*/

describe("DataView.prototype.getUint8", () => {
  test("reads one unsigned byte", () => {
    const buffer = new ArrayBuffer(2);
    const bytes = new Uint8Array(buffer);
    bytes[0] = 255;
    bytes[1] = 1;
    const view = new DataView(buffer);
    expect(view.getUint8(0)).toBe(255);
    expect(view.getUint8(1)).toBe(1);
  });

  test("throws RangeError when read extends past view", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(() => view.getUint8(1)).toThrow(RangeError);
  });
});
