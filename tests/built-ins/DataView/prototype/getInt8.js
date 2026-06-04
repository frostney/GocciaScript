/*---
description: DataView.prototype.getInt8
features: [DataView]
---*/

describe("DataView.prototype.getInt8", () => {
  test("reads one signed byte", () => {
    const view = new DataView(new ArrayBuffer(1));
    view.setUint8(0, 255);
    expect(view.getInt8(0)).toBe(-1);
  });
});
