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
  const PointerHolder = FFI.struct({ pointer: "pointer" });
  const MixedFloats = FFI.struct({ single: "f32", double: "f64", tag: "u8" });

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

  test("keeps aggregate backing storage alive while initializer getters run", () => {
    const Record = FFI.struct({ value: "i32" });
    const record = Record.create({
      get value() {
        Goccia.gc();
        return 42;
      },
    });

    expect(record.value).toBe(42);
  });

  test("invalidates guarded pointer fields when their library closes", () => {
    const pointerLibrary = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const holder = PointerHolder.create({
      pointer: pointerLibrary.symbol("get_answer"),
    });

    pointerLibrary.close();
    Goccia.gc();

    expect(() => holder.pointer.address).toThrow(TypeError);
  });

  test("invalidates guarded pointer fields returned in native aggregates", () => {
    const pointerLibrary = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const makeHolder = pointerLibrary.bind("ffi_v2_get_answer_pointer_holder", {
      args: [],
      returns: PointerHolder,
    });
    const holder = makeHolder();

    pointerLibrary.close();
    Goccia.gc();

    expect(() => holder.pointer.address).toThrow(TypeError);
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

  test("converts a number assigned to an integer field to the field's width", () => {
    const Integers = FFI.struct({
      signed8: "i8",
      unsigned8: "u8",
      signed16: "i16",
      unsigned16: "u16",
      signed32: "i32",
      unsigned32: "u32",
    });
    const value = Integers.create();

    value.signed8 = 300;
    value.unsigned8 = -1;
    value.signed16 = 40000;
    value.unsigned16 = -1;
    value.signed32 = 2147483648;
    value.unsigned32 = -1;
    expect(value.signed8).toBe(44);
    expect(value.unsigned8).toBe(255);
    expect(value.signed16).toBe(-25536);
    expect(value.unsigned16).toBe(65535);
    expect(value.signed32).toBe(-2147483648);
    expect(value.unsigned32).toBe(4294967295);

    value.signed8 = -129;
    value.unsigned8 = 256;
    value.signed32 = 3.9;
    value.unsigned32 = -3.9;
    expect(value.signed8).toBe(127);
    expect(value.unsigned8).toBe(0);
    expect(value.signed32).toBe(3);
    expect(value.unsigned32).toBe(4294967293);

    value.signed16 = NaN;
    value.unsigned16 = Infinity;
    value.signed32 = -Infinity;
    expect(value.signed16).toBe(0);
    expect(value.unsigned16).toBe(0);
    expect(value.signed32).toBe(0);
  });

  test("rounds a number assigned to a floating-point field to the field's precision", () => {
    const value = MixedFloats.create();

    value.single = 0.1;
    value.double = 0.1;
    expect(value.single).toBe(Math.fround(0.1));
    expect(value.single).not.toBe(0.1);
    expect(value.double).toBe(0.1);

    value.single = 16777217;
    value.double = 16777217;
    expect(value.single).toBe(16777216);
    expect(value.double).toBe(16777217);

    value.single = NaN;
    value.double = -0;
    expect(value.single).toBeNaN();
    expect(Object.is(value.double, -0)).toBe(true);

    value.single = Infinity;
    value.double = -Infinity;
    expect(value.single).toBe(Infinity);
    expect(value.double).toBe(-Infinity);
  });

  test("writes one field without disturbing its neighbours", () => {
    const value = MixedFloats.create({ single: 1.5, double: 2.5, tag: 7 });
    const bytes = new Uint8Array(value.buffer);
    const before = [...bytes];

    value.tag = 200;

    expect(value.single).toBe(1.5);
    expect(value.double).toBe(2.5);
    expect(value.tag).toBe(200);
    expect([...bytes].filter((byte, index) => byte !== before[index]).length).toBe(1);
  });

  test("coerces a non-number assigned to a numeric field", () => {
    const value = MixedFloats.create();
    const calls = [];

    value.tag = "12";
    expect(value.tag).toBe(12);
    value.tag = true;
    expect(value.tag).toBe(1);
    value.tag = null;
    expect(value.tag).toBe(0);
    value.tag = undefined;
    expect(value.tag).toBe(0);
    value.double = undefined;
    expect(value.double).toBeNaN();
    value.double = {
      valueOf() {
        calls.push("valueOf");
        return 6.25;
      },
    };
    expect(value.double).toBe(6.25);
    expect(calls).toEqual(["valueOf"]);
    expect(() => {
      value.tag = Symbol("tag");
    }).toThrow(TypeError);
    expect(value.tag).toBe(0);
  });

  test("reads and writes a boolean field by truthiness", () => {
    const Flags = FFI.struct({ before: "u8", flag: "bool", after: "u8" });
    const value = Flags.create({ before: 9, flag: true, after: 9 });

    expect(value.flag).toBe(true);
    value.flag = 0;
    expect(value.flag).toBe(false);
    value.flag = "yes";
    expect(value.flag).toBe(true);
    value.flag = "";
    expect(value.flag).toBe(false);
    value.flag = 2;
    expect(value.flag).toBe(true);
    expect(value.before).toBe(9);
    expect(value.after).toBe(9);
  });

  test("rejects a field write once the backing buffer is detached", () => {
    const value = MixedFloats.create({ single: 1, double: 2, tag: 3 });

    FFI.metadata(value).buffer.transfer();

    expect(() => {
      value.tag = 4;
    }).toThrow(TypeError);
    expect(() => {
      value.tag = "4";
    }).toThrow(TypeError);
    expect(() => value.tag).toThrow(TypeError);
    expect(() => value.double).toThrow(TypeError);
  });

  test("passes the current field values on every call", () => {
    const distanceSquared = lib.bind("ffi_v2_point_distance_squared", {
      args: [Point, Point],
      returns: "f64",
    });
    const left = Point.create({ x: 0, y: 0 });
    const right = Point.create({ x: 0, y: 0 });
    const results = [];

    for (const step of [1, 2, 3, 4]) {
      left.x = step;
      right.y = step * 2;
      results.push(distanceSquared(left, right));
    }

    expect(results).toEqual([5, 20, 45, 80]);
  });
});
