/*---
description: DataView.prototype.setInt16
features: [DataView]
---*/

describe("DataView.prototype.setInt16", () => {
  test("writes signed 16-bit values", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setInt16(0, -1);
    expect(view.getUint16(0)).toBe(0xffff);
  });
});
