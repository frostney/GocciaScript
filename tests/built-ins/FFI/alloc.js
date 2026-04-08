describe("FFI pointer arguments with ArrayBuffer", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);

  test("passes ArrayBuffer as pointer argument", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });
    const readI32 = lib.bind("read_i32", { args: ["pointer"], returns: "i32" });

    const buf = new ArrayBuffer(4);
    writeI32(buf, 42);
    expect(readI32(buf)).toBe(42);
  });

  test("passes TypedArray as pointer argument", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });

    const arr = new Int32Array(4);
    writeI32(arr, 99);
    // The C function writes to the first 4 bytes of the typed array's buffer
    expect(arr[0]).toBe(99);
  });

  test("TypedArray with byte offset passes correct address", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });

    const buf = new ArrayBuffer(16);
    const view = new Int32Array(buf, 8); // offset 8 bytes into the buffer
    writeI32(view, 123);
    // view[0] maps to bytes 8-11 of the underlying buffer
    expect(view[0]).toBe(123);

    // Verify it didn't write to the start of the buffer
    const start = new Int32Array(buf, 0, 1);
    expect(start[0]).toBe(0);
  });

  test("passes null as pointer argument", () => {
    const isNull = lib.bind("is_null", { args: ["pointer"], returns: "i32" });
    expect(isNull(null)).toBe(1);
    expect(isNull(FFI.nullptr)).toBe(1);
  });

  test("C function can write into ArrayBuffer and JS reads it back", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });
    const readI32 = lib.bind("read_i32", { args: ["pointer"], returns: "i32" });

    const buf = new ArrayBuffer(4);
    const view = new Int32Array(buf);

    // Write via C
    writeI32(buf, 777);

    // Read via JS typed array view
    expect(view[0]).toBe(777);

    // Read via C
    expect(readI32(buf)).toBe(777);
  });

  test("throws on invalid pointer argument type", () => {
    const isNull = lib.bind("is_null", { args: ["pointer"], returns: "i32" });
    expect(() => isNull(42)).toThrow(TypeError);
    expect(() => isNull("hello")).toThrow(TypeError);
  });
});
