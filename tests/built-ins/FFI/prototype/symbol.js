describe("FFILibrary.prototype.symbol", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture.dylib");

  test("returns an FFIPointer for a known symbol", () => {
    const ptr = lib.symbol("get_answer");
    expect(ptr.isNull).toBe(false);
    expect(ptr.address).not.toBe(0);
  });

  test("throws on unknown symbol", () => {
    expect(() => lib.symbol("nonexistent")).toThrow(TypeError);
  });

  test("throws when looking up symbol in closed library", () => {
    const closed = FFI.open("./fixtures/ffi/libfixture.dylib");
    closed.close();
    expect(() => closed.symbol("get_answer")).toThrow(TypeError);
  });
});
