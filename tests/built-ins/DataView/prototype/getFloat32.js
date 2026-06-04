/*---
description: DataView.prototype.getFloat32
features: [DataView]
---*/

describe("DataView.prototype.getFloat32", () => {
  test("reads IEEE 754 single precision values", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setFloat32(0, 1.5);
    expect(view.getFloat32(0)).toBe(1.5);
  });

  test("stores NaN", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setFloat32(0, NaN);
    expect(Number.isNaN(view.getFloat32(0))).toBe(true);
  });
});
