/*---
description: DataView.prototype.getInt16
features: [DataView]
---*/

describe("DataView.prototype.getInt16", () => {
  test("reads signed 16-bit values", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setUint8(0, 0xff);
    view.setUint8(1, 0xff);
    expect(view.getInt16(0)).toBe(-1);
  });
});
