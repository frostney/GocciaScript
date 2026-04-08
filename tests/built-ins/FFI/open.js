describe("FFI.open", () => {
  test("opens a shared library", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    expect(lib).toBeDefined();
    expect(lib.closed).toBe(false);
    lib.close();
  });

  test("exposes the library path", () => {
    const path = "./fixtures/ffi/libfixture" + FFI.suffix;
    const lib = FFI.open(path);
    expect(lib.path).toBe(path);
    lib.close();
  });

  test("throws on missing library", () => {
    expect(() => FFI.open("./nonexistent" + FFI.suffix)).toThrow(TypeError);
  });

  test("throws without arguments", () => {
    expect(() => FFI.open()).toThrow(TypeError);
  });
});
