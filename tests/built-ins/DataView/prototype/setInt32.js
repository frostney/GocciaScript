/*---
description: DataView.prototype.setInt32
features: [DataView]
---*/

describe("DataView.prototype.setInt32", () => {
  test("writes signed 32-bit values", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setInt32(0, -1);
    expect(view.getUint32(0)).toBe(0xffffffff);
  });
});
