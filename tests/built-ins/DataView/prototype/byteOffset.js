/*---
description: DataView.prototype.byteOffset
features: [DataView]
---*/

describe("get DataView.prototype.byteOffset", () => {
  test("returns view byte offset", () => {
    const view = new DataView(new ArrayBuffer(8), 3, 4);
    expect(view.byteOffset).toBe(3);
  });

  test("throws TypeError for detached buffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer, 1);
    buffer.transfer();
    expect(() => view.byteOffset).toThrow(TypeError);
  });
});
