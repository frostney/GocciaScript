/*---
description: ArrayBuffer.prototype.resize
features: [ArrayBuffer, resizable-arraybuffer]
---*/

describe("ArrayBuffer.prototype.resize", () => {
  test("resize grows a resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.byteLength).toBe(4);
    buf.resize(8);
    expect(buf.byteLength).toBe(8);
  });

  test("resize shrinks a resizable buffer", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 16 });
    buf.resize(4);
    expect(buf.byteLength).toBe(4);
  });

  test("resize to 0", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 16 });
    buf.resize(0);
    expect(buf.byteLength).toBe(0);
  });

  test("resize to maxByteLength", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    buf.resize(16);
    expect(buf.byteLength).toBe(16);
  });

  test("resize to same size is a no-op", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 16 });
    buf.resize(8);
    expect(buf.byteLength).toBe(8);
  });

  test("resize returns undefined", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    const result = buf.resize(8);
    expect(result).toBe(undefined);
  });

  test("resize preserves existing data when growing", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    const view = new Uint8Array(buf);
    view[0] = 1;
    view[1] = 2;
    view[2] = 3;
    view[3] = 4;

    buf.resize(8);
    const newView = new Uint8Array(buf);
    expect(newView[0]).toBe(1);
    expect(newView[1]).toBe(2);
    expect(newView[2]).toBe(3);
    expect(newView[3]).toBe(4);
  });

  test("resize zero-fills new bytes when growing", () => {
    const buf = new ArrayBuffer(2, { maxByteLength: 8 });
    const view = new Uint8Array(buf);
    view[0] = 255;
    view[1] = 128;

    buf.resize(6);
    const newView = new Uint8Array(buf);
    expect(newView[0]).toBe(255);
    expect(newView[1]).toBe(128);
    expect(newView[2]).toBe(0);
    expect(newView[3]).toBe(0);
    expect(newView[4]).toBe(0);
    expect(newView[5]).toBe(0);
  });

  test("resize truncates data when shrinking", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 8 });
    const view = new Uint8Array(buf);
    view[0] = 10;
    view[1] = 20;
    view[2] = 30;
    view[3] = 40;

    buf.resize(2);
    const newView = new Uint8Array(buf);
    expect(newView.length).toBe(2);
    expect(newView[0]).toBe(10);
    expect(newView[1]).toBe(20);
  });

  test("throws TypeError for non-resizable buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(() => buf.resize(4)).toThrow(TypeError);
  });

  test("throws TypeError for detached buffer", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 16 });
    buf.transfer();
    expect(() => buf.resize(4)).toThrow(TypeError);
  });

  test("throws RangeError when newLength exceeds maxByteLength", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(() => buf.resize(17)).toThrow(RangeError);
  });

  test("throws RangeError for negative newLength", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(() => buf.resize(-1)).toThrow(RangeError);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const resize = ArrayBuffer.prototype.resize;
    expect(() => resize.call({}, 0)).toThrow(TypeError);
  });
});
