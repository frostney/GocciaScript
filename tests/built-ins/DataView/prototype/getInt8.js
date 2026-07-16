/*---
description: DataView.prototype.getInt8
features: [DataView]
---*/

describe("DataView.prototype.getInt8", () => {
  test("reads one signed byte", () => {
    const buffer = new ArrayBuffer(3);
    const bytes = new Uint8Array(buffer);
    bytes[1] = 255;
    const view = new DataView(buffer, 1, 1);
    expect(view.getInt8(0)).toBe(-1);
  });

  test("detached buffer is checked after ToIndex accepts MAX_SAFE_INTEGER", () => {
    const buffer = new ArrayBuffer(1);
    const view = new DataView(buffer);
    buffer.transfer();

    expect(() => view.getInt8(Math.pow(2, 53) - 1)).toThrow(TypeError);
    expect(() => view.getInt8(Math.pow(2, 53))).toThrow(RangeError);
  });

  test("rejects incompatible receivers before converting the index", () => {
    let converted = false;
    const index = {
      valueOf() {
        converted = true;
        return 0;
      },
    };

    expect(() => DataView.prototype.getInt8.call({}, index)).toThrow(TypeError);
    expect(converted).toBe(false);
  });
});
