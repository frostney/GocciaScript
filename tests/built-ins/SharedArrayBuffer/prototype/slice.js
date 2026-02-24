describe("SharedArrayBuffer.prototype.slice", () => {
  test("slices the full buffer when no arguments are given", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice();
    expect(sliced.byteLength).toBe(8);
    expect(sliced instanceof SharedArrayBuffer).toBe(true);
  });

  test("slices from a start index", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(4);
    expect(sliced.byteLength).toBe(4);
  });

  test("slices with start and end", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(2, 6);
    expect(sliced.byteLength).toBe(4);
  });

  test("negative start index counts from end", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(-4);
    expect(sliced.byteLength).toBe(4);
  });

  test("negative end index counts from end", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(0, -2);
    expect(sliced.byteLength).toBe(6);
  });

  test("both negative indices", () => {
    const sab = new SharedArrayBuffer(10);
    const sliced = sab.slice(-6, -2);
    expect(sliced.byteLength).toBe(4);
  });

  test("returns empty buffer when start >= end", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(6, 2);
    expect(sliced.byteLength).toBe(0);
  });

  test("clamps start to 0", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(-100);
    expect(sliced.byteLength).toBe(8);
  });

  test("clamps end to length", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(0, 100);
    expect(sliced.byteLength).toBe(8);
  });

  // ES2026 §25.2.5.6 step 11: If new.[[ArrayBufferData]] is O.[[ArrayBufferData]], throw TypeError
  // Zero-length SharedArrayBuffers share the same backing data block, triggering the identity check.
  test("slicing a zero-length buffer throws TypeError", () => {
    const sab = new SharedArrayBuffer(0);
    expect(() => sab.slice()).toThrow(TypeError);
  });

  test("result is a new SharedArrayBuffer instance", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice();
    expect(sliced instanceof SharedArrayBuffer).toBe(true);
  });
});

describe("SharedArrayBuffer.prototype.slice edge cases", () => {
  test("undefined end defaults to byteLength", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.slice(0, undefined).byteLength).toBe(8);
    expect(sab.slice(2, undefined).byteLength).toBe(6);
  });

  test("undefined start defaults to 0", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.slice(undefined).byteLength).toBe(8);
    expect(sab.slice(undefined, 4).byteLength).toBe(4);
  });

  test("NaN start is treated as 0", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.slice(NaN).byteLength).toBe(8);
    expect(sab.slice(NaN, 4).byteLength).toBe(4);
  });

  test("NaN end is treated as 0", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.slice(0, NaN).byteLength).toBe(0);
  });

  test("both NaN gives empty buffer", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.slice(NaN, NaN).byteLength).toBe(0);
  });

  test("result is always a new instance", () => {
    const sab = new SharedArrayBuffer(8);
    const full = sab.slice();
    expect(full).not.toBe(sab);
    expect(full instanceof SharedArrayBuffer).toBe(true);
  });

  test("result is not an ArrayBuffer", () => {
    const sab = new SharedArrayBuffer(8);
    const sliced = sab.slice(0, 4);
    expect(sliced instanceof ArrayBuffer).toBe(false);
  });
});
