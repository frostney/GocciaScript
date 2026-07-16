/*---
description: DataView.prototype.getUint32
features: [DataView]
---*/

describe("DataView.prototype.getUint32", () => {
  test("reads big-endian by default and little-endian when requested", () => {
    const buffer = new ArrayBuffer(6);
    const bytes = new Uint8Array(buffer);
    const fixture = [0x12, 0x34, 0x56, 0x78];
    fixture.forEach((byte, index) => {
      bytes[index + 1] = byte;
    });
    const view = new DataView(buffer, 1, 4);
    expect(view.getUint32(0)).toBe(0x12345678);
    expect(view.getUint32(0, true)).toBe(0x78563412);
  });

  test("converts the index before checking view bounds", () => {
    const events = [];
    const view = new DataView(new ArrayBuffer(4));
    const index = {
      valueOf() {
        events.push("index");
        return 1.9;
      },
    };

    expect(() => view.getUint32(index)).toThrow(RangeError);
    expect(events).toEqual(["index"]);
    expect(() => view.getUint32(-1)).toThrow(RangeError);
  });

  test("checks detached state after converting the index", () => {
    const events = [];
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    buffer.transfer();
    const index = {
      valueOf() {
        events.push("index");
        return 0;
      },
    };

    expect(() => view.getUint32(index)).toThrow(TypeError);
    expect(events).toEqual(["index"]);
  });

  test("rejects incompatible receivers before converting the index", () => {
    let converted = false;
    const index = {
      valueOf() {
        converted = true;
        return 0;
      },
    };

    expect(() => DataView.prototype.getUint32.call({}, index)).toThrow(TypeError);
    expect(converted).toBe(false);
  });
});
