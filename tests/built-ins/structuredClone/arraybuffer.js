/*---
description: structuredClone clones ArrayBuffer
features: [structuredClone, ArrayBuffer]
---*/

describe("ArrayBuffer cloning", () => {
  test("clones an ArrayBuffer", () => {
    const original = new ArrayBuffer(8);
    const clone = structuredClone(original);
    expect(clone instanceof ArrayBuffer).toBe(true);
    expect(clone.byteLength).toBe(8);
  });

  test("clone is a distinct buffer", () => {
    const original = new ArrayBuffer(4);
    const clone = structuredClone(original);
    expect(clone).not.toBe(original);
  });

  test("clones zero-length ArrayBuffer", () => {
    const original = new ArrayBuffer(0);
    const clone = structuredClone(original);
    expect(clone.byteLength).toBe(0);
  });

  test("clones large ArrayBuffer", () => {
    const original = new ArrayBuffer(1024);
    const clone = structuredClone(original);
    expect(clone.byteLength).toBe(1024);
  });

  test("clones ArrayBuffer inside an object", () => {
    const buf = new ArrayBuffer(16);
    const obj = { buffer: buf, name: "test" };
    const clone = structuredClone(obj);
    expect(clone.buffer instanceof ArrayBuffer).toBe(true);
    expect(clone.buffer.byteLength).toBe(16);
    expect(clone.buffer).not.toBe(buf);
    expect(clone.name).toBe("test");
  });

  test("clones ArrayBuffer inside an array", () => {
    const buf = new ArrayBuffer(8);
    const arr = [buf, 42, "hello"];
    const clone = structuredClone(arr);
    expect(clone[0] instanceof ArrayBuffer).toBe(true);
    expect(clone[0].byteLength).toBe(8);
    expect(clone[0]).not.toBe(buf);
    expect(clone[1]).toBe(42);
    expect(clone[2]).toBe("hello");
  });

  test("clone preserves ArrayBuffer type (not SharedArrayBuffer)", () => {
    const buf = new ArrayBuffer(8);
    const clone = structuredClone(buf);
    expect(clone instanceof ArrayBuffer).toBe(true);
    expect(clone instanceof SharedArrayBuffer).toBe(false);
  });

  test("clone preserves byteLength of 1", () => {
    const buf = new ArrayBuffer(1);
    const clone = structuredClone(buf);
    expect(clone.byteLength).toBe(1);
  });

  test("clone of ArrayBuffer in nested structure", () => {
    const buf = new ArrayBuffer(4);
    const nested = { outer: { inner: buf } };
    const clone = structuredClone(nested);
    expect(clone.outer.inner instanceof ArrayBuffer).toBe(true);
    expect(clone.outer.inner.byteLength).toBe(4);
    expect(clone.outer.inner).not.toBe(buf);
  });

  test("clone preserves Symbol.toStringTag", () => {
    const buf = new ArrayBuffer(4);
    const clone = structuredClone(buf);
    expect(clone[Symbol.toStringTag]).toBe("ArrayBuffer");
    expect(Object.prototype.toString.call(clone)).toBe("[object ArrayBuffer]");
  });
});
