/*---
description: ArrayBuffer.prototype.transferToFixedLength
features: [ArrayBuffer, arraybuffer-transfer]
---*/

describe("ArrayBuffer.prototype.transferToFixedLength", () => {
  test("transfers data to a new fixed-length buffer", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 42;
    view[7] = 99;

    const newBuf = buf.transferToFixedLength();
    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(42);
    expect(newView[7]).toBe(99);
  });

  test("detaches the original buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transferToFixedLength();
    expect(buf.detached).toBe(true);
    expect(buf.byteLength).toBe(0);
  });

  test("new buffer has same byteLength when no argument", () => {
    const buf = new ArrayBuffer(16);
    const newBuf = buf.transferToFixedLength();
    expect(newBuf.byteLength).toBe(16);
  });

  test("always produces a non-resizable buffer from resizable source", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.resizable).toBe(true);

    const newBuf = buf.transferToFixedLength();
    expect(newBuf.resizable).toBe(false);
    expect(newBuf.byteLength).toBe(4);
    expect(newBuf.maxByteLength).toBe(4);
  });

  test("transferToFixedLength with larger size zero-fills", () => {
    const buf = new ArrayBuffer(4);
    const view = new Uint8Array(buf);
    view[0] = 1;
    view[1] = 2;

    const newBuf = buf.transferToFixedLength(8);
    expect(newBuf.byteLength).toBe(8);

    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(1);
    expect(newView[1]).toBe(2);
    expect(newView[4]).toBe(0);
    expect(newView[7]).toBe(0);
  });

  test("transferToFixedLength with smaller size truncates", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 10;
    view[1] = 20;
    view[2] = 30;

    const newBuf = buf.transferToFixedLength(2);
    expect(newBuf.byteLength).toBe(2);

    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(10);
    expect(newView[1]).toBe(20);
  });

  test("transferToFixedLength with 0", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transferToFixedLength(0);
    expect(newBuf.byteLength).toBe(0);
    expect(buf.detached).toBe(true);
  });

  test("new buffer is instanceof ArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transferToFixedLength();
    expect(newBuf instanceof ArrayBuffer).toBe(true);
  });

  test("throws TypeError on detached buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(() => buf.transferToFixedLength()).toThrow(TypeError);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const transferToFixedLength = ArrayBuffer.prototype.transferToFixedLength;
    expect(() => transferToFixedLength.call({})).toThrow(TypeError);
  });
});
