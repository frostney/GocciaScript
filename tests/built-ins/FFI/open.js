describe("FFI.open", () => {
  test("opens a shared library", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    expect(lib).toBeDefined();
    expect(lib.closed).toBe(false);
    lib.close();
  });

  test("exposes the library path", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    expect(lib.path).toBe("./fixtures/ffi/libfixture.dylib");
    lib.close();
  });

  test("throws on missing library", () => {
    expect(() => FFI.open("./nonexistent.dylib")).toThrow(TypeError);
  });

  test("throws without arguments", () => {
    expect(() => FFI.open()).toThrow(TypeError);
  });
});
