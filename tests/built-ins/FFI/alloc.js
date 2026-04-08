describe("FFI.alloc and FFI.free", () => {
  test("allocates and frees memory", () => {
    const ptr = FFI.alloc(1024);
    expect(ptr.isNull).toBe(false);
    expect(ptr.size).toBe(1024);
    FFI.free(ptr);
  });

  test("allocated memory is zero-initialized", () => {
    const ptr = FFI.alloc(16);
    expect(ptr.readU8(0)).toBe(0);
    expect(ptr.readU8(15)).toBe(0);
    expect(ptr.readI32(0)).toBe(0);
    FFI.free(ptr);
  });

  test("throws on zero-size allocation", () => {
    expect(() => FFI.alloc(0)).toThrow(RangeError);
  });

  test("throws on negative size", () => {
    expect(() => FFI.alloc(-1)).toThrow(RangeError);
  });

  test("throws when freeing a borrowed pointer", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    const ptr = lib.symbol("get_answer");
    expect(() => FFI.free(ptr)).toThrow(TypeError);
    lib.close();
  });

  test("throws when freeing twice", () => {
    const ptr = FFI.alloc(64);
    FFI.free(ptr);
    expect(() => FFI.free(ptr)).toThrow(TypeError);
  });

  test("throws when freeing non-pointer", () => {
    expect(() => FFI.free(42)).toThrow(TypeError);
  });
});
