/*---
description: ArrayBuffer.prototype.transfer
features: [ArrayBuffer, arraybuffer-transfer]
---*/

describe("ArrayBuffer.prototype.transfer", () => {
  test("transfers data to a new buffer", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 1;
    view[1] = 2;
    view[2] = 3;

    const newBuf = buf.transfer();
    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(1);
    expect(newView[1]).toBe(2);
    expect(newView[2]).toBe(3);
  });

  test("detaches the original buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(buf.detached).toBe(true);
    expect(buf.byteLength).toBe(0);
  });

  test("new buffer has same byteLength when no argument", () => {
    const buf = new ArrayBuffer(16);
    const newBuf = buf.transfer();
    expect(newBuf.byteLength).toBe(16);
  });

  test("transfer with larger size zero-fills new bytes", () => {
    const buf = new ArrayBuffer(4);
    const view = new Uint8Array(buf);
    view[0] = 10;
    view[1] = 20;

    const newBuf = buf.transfer(8);
    expect(newBuf.byteLength).toBe(8);

    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(10);
    expect(newView[1]).toBe(20);
    expect(newView[4]).toBe(0);
    expect(newView[7]).toBe(0);
  });

  test("transfer with smaller size truncates", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 1;
    view[1] = 2;
    view[2] = 3;
    view[3] = 4;

    const newBuf = buf.transfer(2);
    expect(newBuf.byteLength).toBe(2);

    const newView = new Uint8Array(newBuf);
    expect(newView[0]).toBe(1);
    expect(newView[1]).toBe(2);
  });

  test("transfer with 0 creates empty buffer and detaches", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transfer(0);
    expect(newBuf.byteLength).toBe(0);
    expect(buf.detached).toBe(true);
  });

  test("transfer preserves resizability", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.resizable).toBe(true);

    const newBuf = buf.transfer();
    expect(newBuf.resizable).toBe(true);
    expect(newBuf.byteLength).toBe(4);
    expect(newBuf.maxByteLength).toBe(16);
  });

  test("transfer of resizable with larger size updates maxByteLength", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 8 });
    const newBuf = buf.transfer(12);
    expect(newBuf.resizable).toBe(true);
    expect(newBuf.byteLength).toBe(12);
    // maxByteLength = max(newLength, originalMaxByteLength)
    expect(newBuf.maxByteLength).toBe(12);
  });

  test("transfer of fixed-length buffer returns fixed-length", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transfer();
    expect(newBuf.resizable).toBe(false);
    expect(newBuf.maxByteLength).toBe(8);
  });

  test("new buffer is a distinct object", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transfer();
    expect(newBuf).not.toBe(buf);
  });

  test("new buffer is instanceof ArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    const newBuf = buf.transfer();
    expect(newBuf instanceof ArrayBuffer).toBe(true);
  });

  test("throws TypeError on detached buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(() => buf.transfer()).toThrow(TypeError);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const transfer = ArrayBuffer.prototype.transfer;
    expect(() => transfer.call({})).toThrow(TypeError);
  });

  test("transfer with undefined newLength uses current byteLength", () => {
    const buf = new ArrayBuffer(10);
    const newBuf = buf.transfer(undefined);
    expect(newBuf.byteLength).toBe(10);
  });
});
