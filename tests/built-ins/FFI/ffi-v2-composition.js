describe("FFI aggregate composition", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const Bytes4 = FFI.array("u8", 4);
  const Word = FFI.union({ asU32: "u32", asBytes: Bytes4 });
  const Point = FFI.struct({ x: "f64", y: "f64" });
  const Composite = FFI.struct({
    header: Bytes4,
    payload: Word,
    point: Point,
  });

  afterAll(() => lib.close());

  test("passes and returns nested arrays, unions, and structs by value", () => {
    const transform = lib.bind("ffi_v2_transform_composite", {
      args: [Composite],
      returns: Composite,
    });
    const getHeaderOffset = lib.bind("ffi_v2_composite_header_offset", {
      args: [],
      returns: "i32",
    });
    const getPayloadOffset = lib.bind("ffi_v2_composite_payload_offset", {
      args: [],
      returns: "i32",
    });
    const getPointOffset = lib.bind("ffi_v2_composite_point_offset", {
      args: [],
      returns: "i32",
    });
    const input = Composite.create({
      header: Bytes4.create([1, 2, 3, 4]),
      payload: Word.create({ asU32: 5 }),
      point: Point.create({ x: 6, y: 7 }),
    });
    const output = transform(input);

    expect(output.header[0]).toBe(11);
    expect(output.header[1]).toBe(2);
    expect(output.payload.asU32).toBe(25);
    expect(output.point.x).toBe(36);
    expect(output.point.y).toBe(47);
    expect(output.header.buffer).toBe(output.buffer);
    expect(output.payload.buffer).toBe(output.buffer);
    expect(output.point.buffer).toBe(output.buffer);
    expect(output.header.byteOffset).toBe(getHeaderOffset());
    expect(output.payload.byteOffset).toBe(getPayloadOffset());
    expect(output.point.byteOffset).toBe(getPointOffset());
  });
});
