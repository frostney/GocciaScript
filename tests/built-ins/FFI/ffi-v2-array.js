describe("FFI.array", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const Bytes4 = FFI.array("u8", 4);

  afterAll(() => lib.close());

  test("passes and returns a fixed array by value", () => {
    const reverse = lib.bind("ffi_v2_reverse_bytes4", {
      args: [Bytes4],
      returns: Bytes4,
    });
    const input = Bytes4.create([1, 2, 3, 4]);
    const output = reverse(input);

    expect(output[0]).toBe(4);
    expect(output[1]).toBe(3);
    expect(output[2]).toBe(2);
    expect(output[3]).toBe(1);
    expect(input[0]).toBe(1);
    expect(input.buffer).toBeInstanceOf(ArrayBuffer);

    output[0] = 8;
    expect(output[0]).toBe(8);
  });
});
