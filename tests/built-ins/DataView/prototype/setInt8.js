/*---
description: DataView.prototype.setInt8
features: [DataView]
---*/

describe("DataView.prototype.setInt8", () => {
  test("writes one signed byte, wraps values, and returns undefined", () => {
    const buffer = new ArrayBuffer(2);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);

    expect(view.setInt8(0, -1)).toBeUndefined();
    expect(view.setInt8(1, 257)).toBeUndefined();
    expect(bytes[0]).toBe(255);
    expect(bytes[1]).toBe(1);
  });
});
