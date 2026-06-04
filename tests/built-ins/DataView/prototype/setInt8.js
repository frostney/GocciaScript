/*---
description: DataView.prototype.setInt8
features: [DataView]
---*/

describe("DataView.prototype.setInt8", () => {
  test("writes one signed byte", () => {
    const view = new DataView(new ArrayBuffer(1));
    view.setInt8(0, -1);
    expect(view.getUint8(0)).toBe(255);
  });
});
