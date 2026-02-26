describe("SharedArrayBuffer constructor", () => {
  test("creates a SharedArrayBuffer with the specified byte length", () => {
    const sab = new SharedArrayBuffer(8);
    expect(sab.byteLength).toBe(8);
  });

  test("creates an empty SharedArrayBuffer with length 0", () => {
    const sab = new SharedArrayBuffer(0);
    expect(sab.byteLength).toBe(0);
  });

  test("creates a large SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(1024);
    expect(sab.byteLength).toBe(1024);
  });

  test("creates a buffer with length 1", () => {
    const sab = new SharedArrayBuffer(1);
    expect(sab.byteLength).toBe(1);
  });

  test("typeof returns 'object'", () => {
    const sab = new SharedArrayBuffer(4);
    expect(typeof sab).toBe("object");
  });

  test("instanceof SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(4);
    expect(sab instanceof SharedArrayBuffer).toBe(true);
  });

  test("not instanceof ArrayBuffer", () => {
    const sab = new SharedArrayBuffer(4);
    expect(sab instanceof ArrayBuffer).toBe(false);
  });

  test("byteLength is read-only (reflects allocated size)", () => {
    const sab = new SharedArrayBuffer(16);
    expect(sab.byteLength).toBe(16);
  });
});

describe("SharedArrayBuffer constructor error cases", () => {
  test("throws RangeError for negative length", () => {
    expect(() => new SharedArrayBuffer(-1)).toThrow(RangeError);
  });

  test("throws RangeError for large negative length", () => {
    expect(() => new SharedArrayBuffer(-100)).toThrow(RangeError);
  });

  test("throws RangeError for Infinity", () => {
    expect(() => new SharedArrayBuffer(Infinity)).toThrow(RangeError);
  });

  test("throws RangeError for -Infinity", () => {
    expect(() => new SharedArrayBuffer(-Infinity)).toThrow(RangeError);
  });
});

describe("SharedArrayBuffer constructor ToIndex coercion (ES2026 §6.2.4.2)", () => {
  test("no arguments: ToIndex(undefined) produces 0", () => {
    const sab = new SharedArrayBuffer();
    expect(sab.byteLength).toBe(0);
  });

  test("NaN: ToIntegerOrInfinity(NaN) produces 0", () => {
    const sab = new SharedArrayBuffer(NaN);
    expect(sab.byteLength).toBe(0);
  });

  test("non-integer float 1.5: truncated to 1", () => {
    const sab = new SharedArrayBuffer(1.5);
    expect(sab.byteLength).toBe(1);
  });

  test("negative float -0.5: truncated to 0", () => {
    const sab = new SharedArrayBuffer(-0.5);
    expect(sab.byteLength).toBe(0);
  });

  test("non-numeric string 'abc': ToNumber → NaN → 0", () => {
    const sab = new SharedArrayBuffer("abc");
    expect(sab.byteLength).toBe(0);
  });

  test("object {}: ToNumber → NaN → 0", () => {
    const sab = new SharedArrayBuffer({});
    expect(sab.byteLength).toBe(0);
  });

  test("numeric string is coerced to number", () => {
    const sab = new SharedArrayBuffer("8");
    expect(sab.byteLength).toBe(8);
  });

  test("true is coerced to 1", () => {
    const sab = new SharedArrayBuffer(true);
    expect(sab.byteLength).toBe(1);
  });

  test("false is coerced to 0", () => {
    const sab = new SharedArrayBuffer(false);
    expect(sab.byteLength).toBe(0);
  });

  test("null is coerced to 0", () => {
    const sab = new SharedArrayBuffer(null);
    expect(sab.byteLength).toBe(0);
  });

  test("undefined is coerced to 0", () => {
    const sab = new SharedArrayBuffer(undefined);
    expect(sab.byteLength).toBe(0);
  });

  test("float string '1.9' is truncated to 1", () => {
    const sab = new SharedArrayBuffer("1.9");
    expect(sab.byteLength).toBe(1);
  });

  test("negative integer -1 throws RangeError", () => {
    expect(() => new SharedArrayBuffer(-1)).toThrow(RangeError);
  });
});

describe("SharedArrayBuffer constructor meta", () => {
  test("byteLength cannot be changed (getter-only throws TypeError)", () => {
    const sab = new SharedArrayBuffer(8);
    expect(() => { sab.byteLength = 99; }).toThrow(TypeError);
    expect(sab.byteLength).toBe(8);
  });

  test("SharedArrayBuffer is not instanceof ArrayBuffer", () => {
    const sab = new SharedArrayBuffer(4);
    expect(sab instanceof ArrayBuffer).toBe(false);
  });

  test("throws TypeError when called without new", () => {
    expect(() => SharedArrayBuffer(8)).toThrow(TypeError);
  });

  test("throws TypeError when called without new and no arguments", () => {
    expect(() => SharedArrayBuffer()).toThrow(TypeError);
  });
});
