/*---
description: DataView.prototype.getInt32
features: [DataView]
---*/

describe("DataView.prototype.getInt32", () => {
  test("reads signed 32-bit values", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setUint32(0, 0xffffffff);
    expect(view.getInt32(0)).toBe(-1);
  });
});
