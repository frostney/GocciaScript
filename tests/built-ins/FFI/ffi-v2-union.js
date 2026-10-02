describe("FFI.union", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const Bytes4 = FFI.array("u8", 4);
  const Word = FFI.union({ asU32: "u32", asBytes: Bytes4 });
  const DoubleUnion = FFI.union({ asDouble: "f64", alternate: "f64" });

  afterAll(() => lib.close());

  test("shares storage between union fields", () => {
    const word = Word.create({ asU32: 0 });

    word.asBytes[0] = 1;

    expect(word.asU32).not.toBe(0);
    expect(word.asBytes.buffer).toBe(word.buffer);
  });

  test("passes and returns a union by value", () => {
    const addToWord = lib.bind("ffi_v2_add_to_word", {
      args: [Word, "u32"],
      returns: Word,
    });
    const firstByte = lib.bind("ffi_v2_word_first_byte", {
      args: [Word],
      returns: "u8",
    });
    const original = Word.create({ asU32: 40 });
    const result = addToWord(original, 2);

    expect(result.asU32).toBe(42);
    expect(firstByte(result)).toBe(result.asBytes[0]);
  });

  test("a number written over a pointer field ends the pointer's tie to its library", () => {
    const pointerLibrary = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const Overlay = FFI.union({ pointer: "pointer", low: "u32", whole: "f64" });
    const partly = Overlay.create({ pointer: pointerLibrary.symbol("get_answer") });
    const wholly = Overlay.create({ pointer: pointerLibrary.symbol("get_answer") });
    const untouched = Overlay.create({ pointer: pointerLibrary.symbol("get_answer") });

    partly.low = 5;
    wholly.whole = 0;
    pointerLibrary.close();

    expect(partly.pointer.address % 4294967296).toBe(5);
    expect(wholly.pointer.address).toBe(0);
    expect(wholly.pointer.isNull).toBe(true);
    expect(() => untouched.pointer.address).toThrow(TypeError);
  });

  test("classifies homogeneous floating-point unions", () => {
    const addToDoubleUnion = lib.bind("ffi_v2_add_to_double_union", {
      args: [DoubleUnion, "f64"],
      returns: DoubleUnion,
    });
    const result = addToDoubleUnion(DoubleUnion.create({ asDouble: 40 }), 2);

    expect(result.asDouble).toBe(42);
  });
});
