/*---
description: DataView.prototype.getFloat64
features: [DataView]
---*/

describe("DataView.prototype.getFloat64", () => {
  test("reads IEEE 754 double precision values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setFloat64(0, Math.PI);
    expect(view.getFloat64(0)).toBe(Math.PI);
  });
});
