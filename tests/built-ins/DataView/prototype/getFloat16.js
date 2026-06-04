/*---
description: DataView.prototype.getFloat16
features: [DataView, Float16Array]
---*/

describe("DataView.prototype.getFloat16", () => {
  test("reads IEEE 754 half precision values", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setFloat16(0, 1.5);
    expect(view.getFloat16(0)).toBe(1.5);
  });

  test("preserves negative zero", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setFloat16(0, -0);
    expect(Object.is(view.getFloat16(0), -0)).toBe(true);
  });
});
