/*---
description: DataView.prototype.setUint32
features: [DataView]
---*/

describe("DataView.prototype.setUint32", () => {
  test("writes big-endian by default and little-endian when requested", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);
    expect(view.setUint32(0, 0x12345678)).toBeUndefined();
    expect(view.setUint32(4, 0x12345678, true)).toBeUndefined();
    expect(bytes[0]).toBe(0x12);
    expect(bytes[1]).toBe(0x34);
    expect(bytes[2]).toBe(0x56);
    expect(bytes[3]).toBe(0x78);
    expect(bytes[4]).toBe(0x78);
    expect(bytes[5]).toBe(0x56);
    expect(bytes[6]).toBe(0x34);
    expect(bytes[7]).toBe(0x12);
  });

  test("wraps large finite numbers modulo 2^32", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const bytes = new Uint8Array(buffer);
    view.setUint32(0, 1e20);
    expect(bytes[0]).toBe(0x63);
    expect(bytes[1]).toBe(0x10);
    expect(bytes[2]).toBe(0x00);
    expect(bytes[3]).toBe(0x00);
  });

  test("converts the index before the value and validates bounds afterward", () => {
    const calls = [];
    const view = new DataView(new ArrayBuffer(4));
    const index = {
      valueOf() {
        calls.push("index");
        return 1;
      },
    };
    const value = {
      valueOf() {
        calls.push("value");
        return 1;
      },
    };

    expect(() => view.setUint32(index, value)).toThrow(RangeError);
    expect(calls).toEqual(["index", "value"]);
  });

  test("checks detached state after converting the index and value", () => {
    const calls = [];
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    const index = {
      valueOf() {
        calls.push("index");
        return 0;
      },
    };
    const value = {
      valueOf() {
        calls.push("value");
        buffer.transfer();
        return 1;
      },
    };

    expect(() => view.setUint32(index, value)).toThrow(TypeError);
    expect(calls).toEqual(["index", "value"]);
  });

  test("rejects incompatible receivers before converting arguments", () => {
    const calls = [];
    const index = {
      valueOf() {
        calls.push("index");
        return 0;
      },
    };
    const value = {
      valueOf() {
        calls.push("value");
        return 1;
      },
    };

    expect(() => DataView.prototype.setUint32.call({}, index, value)).toThrow(TypeError);
    expect(calls).toEqual([]);
  });

  test("rejects BigInt values for numeric element kinds", () => {
    expect(() => new DataView(new ArrayBuffer(4)).setUint32(0, 1n)).toThrow(TypeError);
  });
});
