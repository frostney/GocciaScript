describe("FFILibrary.prototype.bind (variadic signatures)", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  afterAll(() => lib.close());

  test("calls an integer variadic function through an explicit typed tail", () => {
    const sum = lib.bind("ffi_variadic_sum_i32", {
      args: ["i32"],
      variadic: true,
      returns: "i32",
    });

    expect(
      sum(
        5,
        FFI.varargs(
          ["i8", "u8", "i16", "u16", "bool"],
          [-1, 2, -3, 4, true],
        ),
      ),
    ).toBe(3);
    expect(sum(0, FFI.varargs([], []))).toBe(0);
  });

  test("applies C default argument promotions to the variadic tail", () => {
    const checksum = lib.bind("ffi_variadic_promotions", {
      args: ["utf8string"],
      variadic: true,
      returns: "f64",
    });

    expect(
      checksum(
        "four",
        FFI.varargs(
          ["bool", "i8", "f32", "utf8string"],
          [true, 3, 2.5, "six"],
        ),
      ),
    ).toBe(13.5);
  });

  test("classifies aggregate values in a variadic tail", () => {
    const Point = FFI.struct({ x: "f64", y: "f64" });
    const OtherPoint = FFI.struct({ x: "f64", y: "f64" });
    const checksum = lib.bind("ffi_variadic_point_checksum", {
      args: ["i32"],
      variadic: true,
      returns: "f64",
    });

    expect(
      checksum(
        2,
        FFI.varargs(
          [Point, Point],
          [
            Point.create({ x: 1, y: 2 }),
            Point.create({ x: 10, y: 20 }),
          ],
        ),
      ),
    ).toBe(33);
    expect(() =>
      checksum(
        1,
        FFI.varargs(
          [Point],
          [OtherPoint.create({ x: 1, y: 2 })],
        ),
      ),
    ).toThrow(TypeError);
  });

  test("validates the explicit variadic marker before native entry", () => {
    const sum = lib.bind("ffi_variadic_sum_i32", {
      args: ["i32"],
      variadic: true,
      returns: "i32",
    });

    expect(() => sum(1)).toThrow(TypeError);
    expect(() => sum(2, 1, 2)).toThrow(TypeError);
    expect(() => sum(1, ["i32"], [1])).toThrow(TypeError);
    expect(() => FFI.varargs(["i32"], [])).toThrow(TypeError);
    expect(() => FFI.varargs("i32", [1])).toThrow(TypeError);
    expect(() =>
      lib.bind("ffi_variadic_sum_i32", {
        args: ["i32"],
        variadic: 1,
        returns: "i32",
      }),
    ).toThrow(TypeError);
  });

  test("bounds the combined fixed and variadic argument count", () => {
    const sum = lib.bind("ffi_variadic_sum_i32", {
      args: ["i32"],
      variadic: true,
      returns: "i32",
    });
    const types = [];
    const values = [];

    for (const unused of new Array(63)) {
      types.push("i32");
      values.push(1);
    }

    expect(sum(63, FFI.varargs(types, values))).toBe(63);
    types.push("i32");
    values.push(1);
    expect(() => sum(64, FFI.varargs(types, values))).toThrow(RangeError);
  });
});
