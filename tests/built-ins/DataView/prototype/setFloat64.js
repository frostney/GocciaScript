/*---
description: DataView.prototype.setFloat64
features: [DataView]
---*/

describe("DataView.prototype.setFloat64", () => {
  test("writes double precision values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setFloat64(0, -Infinity);
    expect(view.getFloat64(0)).toBe(-Infinity);
  });
});
