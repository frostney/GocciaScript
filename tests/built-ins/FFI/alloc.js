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

  test("a SharedArrayBuffer grow exposes zero bytes even where native code wrote past its length", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });

    // The buffer reserves maxByteLength bytes; the C function writes four of
    // them, three past byteLength, where no script can reach.
    const sab = new SharedArrayBuffer(1, { maxByteLength: 8 });
    writeI32(sab, 0x01020304);
    expect(new Uint8Array(sab)[0]).toBe(4);
    sab.grow(8);
    expect(Array.from(new Uint8Array(sab))).toEqual([4, 0, 0, 0, 0, 0, 0, 0]);
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

  test("TypedArray passes the buffer's current block after the buffer grew", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });

    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const view = new Int32Array(rab);
    rab.resize(64);
    writeI32(view, 321);
    expect(new Int32Array(rab)[0]).toBe(321);
    expect(view.length).toBe(16);
  });

  test("TypedArray past the end of its shrunk buffer passes a null pointer", () => {
    const isNull = lib.bind("is_null", { args: ["pointer"], returns: "i32" });

    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const view = new Int32Array(rab, 8, 2);
    rab.resize(4);
    expect(isNull(view)).toBe(1);
  });

  test("TypedArray over a detached buffer passes a null pointer", () => {
    const isNull = lib.bind("is_null", { args: ["pointer"], returns: "i32" });

    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const view = new Int32Array(rab);
    rab.transfer();
    expect(isNull(view)).toBe(1);
  });
});
