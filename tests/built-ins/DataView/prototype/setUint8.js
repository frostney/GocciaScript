/*---
description: DataView.prototype.setUint8
features: [DataView]
---*/

describe("DataView.prototype.setUint8", () => {
  test("writes one unsigned byte", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(view.setUint8(0, 511)).toBeUndefined();
    expect(view.getUint8(0)).toBe(255);
  });
});
