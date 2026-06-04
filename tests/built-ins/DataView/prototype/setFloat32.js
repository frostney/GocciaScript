/*---
description: DataView.prototype.setFloat32
features: [DataView]
---*/

describe("DataView.prototype.setFloat32", () => {
  test("writes single precision values", () => {
    const view = new DataView(new ArrayBuffer(4));
    view.setFloat32(0, -0);
    expect(Object.is(view.getFloat32(0), -0)).toBe(true);
  });
});
