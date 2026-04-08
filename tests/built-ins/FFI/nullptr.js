describe("FFI.nullptr", () => {
  test("is a null pointer", () => {
    expect(FFI.nullptr.isNull).toBe(true);
    expect(FFI.nullptr.address).toBe(0);
  });

  test("is the same object each time", () => {
    expect(FFI.nullptr === FFI.nullptr).toBe(true);
  });

  test("can be passed as a pointer argument", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const isNull = lib.bind("is_null", { args: ["pointer"], returns: "i32" });
    expect(isNull(FFI.nullptr)).toBe(1);
    lib.close();
  });
});
