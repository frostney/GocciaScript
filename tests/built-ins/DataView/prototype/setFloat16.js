/*---
description: DataView.prototype.setFloat16
features: [DataView, Float16Array]
---*/

describe("DataView.prototype.setFloat16", () => {
  test("writes half precision values", () => {
    const view = new DataView(new ArrayBuffer(2));
    view.setFloat16(0, 65520);
    expect(view.getFloat16(0)).toBe(Infinity);
  });
});
