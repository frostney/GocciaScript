/*---
description: ArrayBuffer.prototype.transferToImmutable
features: [ArrayBuffer, immutable-arraybuffer]
---*/

describe("ArrayBuffer.prototype.transferToImmutable", () => {
  test("transfers data to a new immutable buffer", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 42;
    view[7] = 99;

    const imm = buf.transferToImmutable();
    const immView = new Uint8Array(imm);
    expect(imm.immutable).toBe(true);
    expect(immView[0]).toBe(42);
    expect(immView[7]).toBe(99);
  });

  test("detaches the original buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transferToImmutable();
    expect(buf.detached).toBe(true);
    expect(buf.byteLength).toBe(0);
  });

  test("new buffer has same byteLength when no argument", () => {
    expect(new ArrayBuffer(16).transferToImmutable().byteLength).toBe(16);
  });

  test("produces a non-resizable buffer from a resizable source", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.resizable).toBe(true);

    const imm = buf.transferToImmutable();
    expect(imm.resizable).toBe(false);
    expect(imm.immutable).toBe(true);
    expect(imm.byteLength).toBe(4);
  });

  test("with a larger size zero-fills", () => {
    const buf = new ArrayBuffer(4);
    const view = new Uint8Array(buf);
    view[0] = 1;
    view[1] = 2;

    const imm = buf.transferToImmutable(8);
    expect(imm.byteLength).toBe(8);

    const immView = new Uint8Array(imm);
    expect(immView[0]).toBe(1);
    expect(immView[1]).toBe(2);
    expect(immView[4]).toBe(0);
    expect(immView[7]).toBe(0);
  });

  test("with a smaller size truncates", () => {
    const buf = new ArrayBuffer(8);
    const view = new Uint8Array(buf);
    view[0] = 10;
    view[1] = 20;
    view[2] = 30;

    const imm = buf.transferToImmutable(2);
    expect(imm.byteLength).toBe(2);

    const immView = new Uint8Array(imm);
    expect(immView[0]).toBe(10);
    expect(immView[1]).toBe(20);
  });

  test("new buffer is instanceof ArrayBuffer", () => {
    expect(new ArrayBuffer(8).transferToImmutable() instanceof ArrayBuffer).toBe(
      true,
    );
  });

  test("the immutable buffer cannot be resized", () => {
    const imm = new ArrayBuffer(4, { maxByteLength: 16 }).transferToImmutable();
    expect(() => imm.resize(8)).toThrow(TypeError);
  });

  test("an immutable buffer cannot be detached by transfer", () => {
    const imm = new ArrayBuffer(8).transferToImmutable();
    expect(() => imm.transfer()).toThrow(TypeError);
    expect(() => imm.transferToFixedLength()).toThrow(TypeError);
    expect(() => imm.transferToImmutable()).toThrow(TypeError);
    expect(imm.detached).toBe(false);
    expect(imm.immutable).toBe(true);
  });

  test("throws TypeError on a detached buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(() => buf.transferToImmutable()).toThrow(TypeError);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const fn = ArrayBuffer.prototype.transferToImmutable;
    expect(() => fn.call({})).toThrow(TypeError);
  });
});
