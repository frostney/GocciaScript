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

  test("detached buffer is checked after ToIndex accepts MAX_SAFE_INTEGER", () => {
    const buffer = new ArrayBuffer(1);
    const view = new DataView(buffer);
    buffer.transfer();

    expect(() => view.getInt8(Math.pow(2, 53) - 1)).toThrow(TypeError);
    expect(() => view.getInt8(Math.pow(2, 53))).toThrow(RangeError);
  });
});
