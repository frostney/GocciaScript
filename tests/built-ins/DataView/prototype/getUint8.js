/*---
description: DataView.prototype.getUint8
features: [DataView]
---*/

describe("DataView.prototype.getUint8", () => {
  test("reads one unsigned byte", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setUint8(0, 255);
    view.setUint8(1, 1);
    expect(view.getUint8(0)).toBe(255);
    expect(view.getUint8(1)).toBe(1);
  });

  test("throws RangeError when read extends past view", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(() => view.getUint8(1)).toThrow(RangeError);
  });
});
