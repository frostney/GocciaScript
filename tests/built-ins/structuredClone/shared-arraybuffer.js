describe("structuredClone with SharedArrayBuffer", () => {
  test("clones a SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(8);
    const cloned = structuredClone(sab);
    expect(cloned instanceof SharedArrayBuffer).toBe(true);
    expect(cloned.byteLength).toBe(8);
  });

  test("clones a zero-length SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(0);
    const cloned = structuredClone(sab);
    expect(cloned instanceof SharedArrayBuffer).toBe(true);
    expect(cloned.byteLength).toBe(0);
  });

  test("cloned SharedArrayBuffer is a distinct instance", () => {
    const sab = new SharedArrayBuffer(16);
    const cloned = structuredClone(sab);
    expect(cloned).not.toBe(sab);
    expect(cloned.byteLength).toBe(16);
  });

  test("clones a large SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(4096);
    const cloned = structuredClone(sab);
    expect(cloned.byteLength).toBe(4096);
  });

  test("SharedArrayBuffer inside an object", () => {
    const sab = new SharedArrayBuffer(8);
    const obj = { buffer: sab, name: "test" };
    const cloned = structuredClone(obj);
    expect(cloned.buffer instanceof SharedArrayBuffer).toBe(true);
    expect(cloned.buffer.byteLength).toBe(8);
    expect(cloned.name).toBe("test");
  });

  test("SharedArrayBuffer inside an array", () => {
    const sab = new SharedArrayBuffer(4);
    const arr = [sab, 42];
    const cloned = structuredClone(arr);
    expect(cloned[0] instanceof SharedArrayBuffer).toBe(true);
    expect(cloned[0].byteLength).toBe(4);
    expect(cloned[1]).toBe(42);
  });

  test("clone preserves SharedArrayBuffer type (not ArrayBuffer)", () => {
    const sab = new SharedArrayBuffer(8);
    const cloned = structuredClone(sab);
    expect(cloned instanceof SharedArrayBuffer).toBe(true);
    expect(cloned instanceof ArrayBuffer).toBe(false);
  });

  test("clone preserves byteLength of 1", () => {
    const sab = new SharedArrayBuffer(1);
    const cloned = structuredClone(sab);
    expect(cloned.byteLength).toBe(1);
  });

  test("clone of SharedArrayBuffer in nested structure", () => {
    const sab = new SharedArrayBuffer(4);
    const nested = { outer: { inner: sab } };
    const cloned = structuredClone(nested);
    expect(cloned.outer.inner instanceof SharedArrayBuffer).toBe(true);
    expect(cloned.outer.inner.byteLength).toBe(4);
  });

  test("clone preserves Symbol.toStringTag", () => {
    const sab = new SharedArrayBuffer(4);
    const cloned = structuredClone(sab);
    expect(cloned[Symbol.toStringTag]).toBe("SharedArrayBuffer");
    expect(Object.prototype.toString.call(cloned)).toBe("[object SharedArrayBuffer]");
  });
});
