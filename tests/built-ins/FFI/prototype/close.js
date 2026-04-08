describe("FFILibrary.prototype.close", () => {
  test("closes the library", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    expect(lib.closed).toBe(false);
    lib.close();
    expect(lib.closed).toBe(true);
  });

  test("close is idempotent", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture.dylib");
    lib.close();
    lib.close(); // Should not throw
    expect(lib.closed).toBe(true);
  });
});
