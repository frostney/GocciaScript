describe("FFIPointer read/write", () => {
  test("writes and reads u8", () => {
    const ptr = FFI.alloc(16);
    ptr.writeU8(0, 255);
    expect(ptr.readU8(0)).toBe(255);
    ptr.writeU8(1, 0);
    expect(ptr.readU8(1)).toBe(0);
    FFI.free(ptr);
  });

  test("writes and reads i8", () => {
    const ptr = FFI.alloc(16);
    ptr.writeI8(0, -128);
    expect(ptr.readI8(0)).toBe(-128);
    ptr.writeI8(1, 127);
    expect(ptr.readI8(1)).toBe(127);
    FFI.free(ptr);
  });

  test("writes and reads u16", () => {
    const ptr = FFI.alloc(16);
    ptr.writeU16(0, 65535);
    expect(ptr.readU16(0)).toBe(65535);
    FFI.free(ptr);
  });

  test("writes and reads i16", () => {
    const ptr = FFI.alloc(16);
    ptr.writeI16(0, -32768);
    expect(ptr.readI16(0)).toBe(-32768);
    FFI.free(ptr);
  });

  test("writes and reads u32", () => {
    const ptr = FFI.alloc(16);
    ptr.writeU32(0, 4294967295);
    expect(ptr.readU32(0)).toBe(4294967295);
    FFI.free(ptr);
  });

  test("writes and reads i32", () => {
    const ptr = FFI.alloc(16);
    ptr.writeI32(0, -2147483648);
    expect(ptr.readI32(0)).toBe(-2147483648);
    ptr.writeI32(4, 2147483647);
    expect(ptr.readI32(4)).toBe(2147483647);
    FFI.free(ptr);
  });

  test("writes and reads f32", () => {
    const ptr = FFI.alloc(16);
    ptr.writeF32(0, 3.14);
    const val = ptr.readF32(0);
    // f32 precision — compare within tolerance
    expect(val > 3.13 && val < 3.15).toBe(true);
    FFI.free(ptr);
  });

  test("writes and reads f64", () => {
    const ptr = FFI.alloc(16);
    ptr.writeF64(0, 3.141592653589793);
    expect(ptr.readF64(0)).toBe(3.141592653589793);
    FFI.free(ptr);
  });

  test("throws on out-of-bounds read", () => {
    const ptr = FFI.alloc(4);
    expect(() => ptr.readI32(4)).toThrow(RangeError);
    expect(() => ptr.readU8(4)).toThrow(RangeError);
    FFI.free(ptr);
  });

  test("throws on out-of-bounds write", () => {
    const ptr = FFI.alloc(4);
    expect(() => ptr.writeI32(4, 0)).toThrow(RangeError);
    FFI.free(ptr);
  });

  test("throws on freed pointer read", () => {
    const ptr = FFI.alloc(16);
    FFI.free(ptr);
    expect(() => ptr.readU8(0)).toThrow(TypeError);
  });

  test("throws on freed pointer write", () => {
    const ptr = FFI.alloc(16);
    FFI.free(ptr);
    expect(() => ptr.writeU8(0, 1)).toThrow(TypeError);
  });
});

describe("FFIPointer.offset", () => {
  test("creates an offset pointer", () => {
    const ptr = FFI.alloc(16);
    ptr.writeI32(0, 100);
    ptr.writeI32(4, 200);
    const offset = ptr.offset(4);
    expect(offset.readI32(0)).toBe(200);
    FFI.free(ptr);
  });

  test("offset pointer is borrowed", () => {
    const ptr = FFI.alloc(16);
    const offset = ptr.offset(4);
    expect(() => FFI.free(offset)).toThrow(TypeError);
    FFI.free(ptr);
  });
});

describe("FFIPointer with foreign functions", () => {
  test("passes allocated pointer to C function", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });
    const readI32 = lib.bind("read_i32", { args: ["pointer"], returns: "i32" });

    const buf = FFI.alloc(4);
    writeI32(buf, 42);
    expect(readI32(buf)).toBe(42);

    // Also verify via JS read
    expect(buf.readI32(0)).toBe(42);

    FFI.free(buf);
    lib.close();
  });
});
