describe("FFILibrary.prototype.close", () => {
  test("closes the library", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    expect(lib.closed).toBe(false);
    lib.close();
    expect(lib.closed).toBe(true);
  });

  test("close is idempotent", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    lib.close();
    lib.close(); // Should not throw
    expect(lib.closed).toBe(true);
  });

  test("keeps dependents alive across collection and invalidates them on close", () => {
    let collectedFunction;
    let collectedSymbol;
    (() => {
      const collectedLibrary = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
      collectedFunction = collectedLibrary.bind("get_answer", { args: [], returns: "i32" });
      collectedSymbol = collectedLibrary.symbol("get_answer");
    })();

    Goccia.gc();
    expect(collectedFunction()).toBe(42);
    expect(collectedSymbol.address).not.toBe(0);

    const closedLibrary = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const closedFunction = closedLibrary.bind("get_answer", { args: [], returns: "i32" });
    const closedSymbol = closedLibrary.symbol("get_answer");
    closedLibrary.close();

    expect(() => closedFunction()).toThrow(TypeError);
    expect(() => closedSymbol.address).toThrow(TypeError);
  });
});
