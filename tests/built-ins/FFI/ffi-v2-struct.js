describe("FFI.struct", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const Point = FFI.struct({ x: "f64", y: "f64" });
  const LargeVector = FFI.struct({
    first: "f64",
    second: "f64",
    third: "f64",
  });
  const AlignedRecord = FFI.struct({
    tag: "u8",
    value: "f64",
    code: "u16",
  });
  const MixedRecord = FFI.struct({ value: "f64", tag: "i32" });
  const DoubleUnion = FFI.union({ asDouble: "f64", alternate: "f64" });
  const Float3 = FFI.struct({ first: "f32", second: "f32", third: "f32" });
  const Float1 = FFI.struct({ value: "f32" });

  afterAll(() => lib.close());

  test("passes and returns a small struct by value", () => {
    const addPoints = lib.bind("ffi_v2_add_points", {
      args: [Point, Point],
      returns: Point,
    });
    const distanceSquared = lib.bind("ffi_v2_point_distance_squared", {
      args: [Point, Point],
      returns: "f64",
    });
    const left = Point.create({ x: 1.5, y: 2.5 });
    const right = Point.create({ x: 3.5, y: 4.5 });
    const sum = addPoints(left, right);

    expect(sum.x).toBe(5);
    expect(sum.y).toBe(7);
    expect(distanceSquared(left, right)).toBe(8);

    left.x = 2.5;
    expect(distanceSquared(left, right)).toBe(5);
  });

  test("passes aggregate backing storage to pointer arguments", () => {
    const translate = lib.bind("ffi_v2_translate_point", {
      args: ["pointer", "f64", "f64"],
      returns: "void",
    });
    const point = Point.create({ x: 1, y: 2 });

    translate(point, 10, 20);

    expect(point.x).toBe(11);
    expect(point.y).toBe(22);
  });

  test("rejects detached aggregate arguments before entering native code", () => {
    const distanceSquared = lib.bind("ffi_v2_point_distance_squared", {
      args: [Point, Point],
      returns: "f64",
    });
    const point = Point.create({ x: 1, y: 2 });

    point.buffer.transfer();

    expect(() => distanceSquared(point, point)).toThrow(TypeError);
  });

  test("uses the hidden-return path for a large struct", () => {
    const makeLargeVector = lib.bind("ffi_v2_make_large_vector", {
      args: ["f64"],
      returns: LargeVector,
    });
    const sumLargeVector = lib.bind("ffi_v2_sum_large_vector", {
      args: [LargeVector],
      returns: "f64",
    });
    const value = makeLargeVector(10);

    expect(value.first).toBe(10);
    expect(value.second).toBe(11);
    expect(value.third).toBe(12);
    expect(sumLargeVector(value)).toBe(33);
  });

  test("matches native field alignment and padding", () => {
    const checksum = lib.bind("ffi_v2_aligned_record_checksum", {
      args: [AlignedRecord],
      returns: "f64",
    });
    const getSize = lib.bind("ffi_v2_aligned_record_size", {
      args: [],
      returns: "i32",
    });
    const getAlignment = lib.bind("ffi_v2_aligned_record_alignment", {
      args: [],
      returns: "i32",
    });
    const getValueOffset = lib.bind("ffi_v2_aligned_record_value_offset", {
      args: [],
      returns: "i32",
    });
    const getCodeOffset = lib.bind("ffi_v2_aligned_record_code_offset", {
      args: [],
      returns: "i32",
    });
    const value = AlignedRecord.create({ tag: 2, value: 10.5, code: 30 });
    const size = getSize();
    const alignment = getAlignment();
    const valueOffset = getValueOffset();
    const codeOffset = getCodeOffset();

    expect(checksum(value)).toBe(42.5);
    expect(AlignedRecord.size).toBe(size);
    expect(AlignedRecord.alignment).toBe(alignment);
    expect(value.buffer).toBeInstanceOf(ArrayBuffer);
    expect(value.buffer.byteLength).toBe(size);
    expect(alignment).toBeGreaterThan(0);
    expect(valueOffset).toBeGreaterThan(0);
    expect(valueOffset % alignment).toBe(0);
    expect(codeOffset).toBeGreaterThan(valueOffset + 7);
    expect(size).toBeGreaterThan(codeOffset + 1);
    expect(size % alignment).toBe(0);
  });

  test("handles mixed register classes and aggregate register rollback", () => {
    const makeMixedRecord = lib.bind("ffi_v2_make_mixed_record", {
      args: ["f64", "i32"],
      returns: MixedRecord,
    });
    const underPressure = lib.bind(
      "ffi_v2_mixed_record_under_register_pressure",
      {
        args: ["i32", "i32", "i32", "i32", "i32", "i32", "i32", MixedRecord],
        returns: "f64",
      },
    );
    const value = makeMixedRecord(10, 20);

    expect(value.value).toBe(10);
    expect(value.tag).toBe(20);
    expect(underPressure(1, 2, 3, 4, 5, 6, 7, value)).toBe(58);
  });

  test("spills an HFA when the remaining float registers are exhausted", () => {
    const underPressure = lib.bind("ffi_v2_point_after_seven_doubles", {
      args: ["f64", "f64", "f64", "f64", "f64", "f64", "f64", Point],
      returns: "f64",
    });

    expect(
      underPressure(1, 2, 3, 4, 5, 6, 7, Point.create({ x: 8, y: 9 })),
    ).toBe(45);
  });

  test("compacts consecutive spilled HFAs on Darwin ARM64", () => {
    const underPressure = lib.bind("ffi_v2_compact_spilled_hfas", {
      args: [
        DoubleUnion,
        DoubleUnion,
        DoubleUnion,
        DoubleUnion,
        DoubleUnion,
        DoubleUnion,
        Float3,
        Float1,
      ],
      returns: "f64",
    });
    const number = (value) => DoubleUnion.create({ asDouble: value });

    expect(
      underPressure(
        number(1),
        number(2),
        number(3),
        number(4),
        number(5),
        number(6),
        Float3.create({ first: 7, second: 8, third: 9 }),
        Float1.create({ value: 10 }),
      ),
    ).toBe(55);
  });
});
