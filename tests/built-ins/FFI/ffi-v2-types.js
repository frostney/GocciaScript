describe("FFI type descriptors", () => {
  test("rejects invalid aggregate and callback definitions", () => {
    expect(() => FFI.struct({})).toThrow(TypeError);
    expect(() => FFI.union({})).toThrow(TypeError);
    expect(() => FFI.struct({ invalid: "void" })).toThrow(TypeError);
    expect(() => FFI.struct({ buffer: "u8" })).toThrow(TypeError);
    expect(() => FFI.union({ byteOffset: "u8" })).toThrow(TypeError);
    expect(() => FFI.array("u8", 0)).toThrow(TypeError);
    expect(() => FFI.array("u8", 1.5)).toThrow(TypeError);
    expect(() => FFI.callback({ args: "i32", returns: "i32" })).toThrow(
      TypeError,
    );
    expect(() =>
      FFI.callback({
        args: ["i32", "i32", "i32", "i32", "i32", "i32", "i32", "i32", "i32"],
        returns: "i32",
      }),
    ).toThrow(TypeError);
  });

  test("bounds aggregate size and descriptor nesting", () => {
    expect(() => FFI.array("u8", 65537)).toThrow(TypeError);
    expect(() => {
      let type = "u8";
      for (const unused of new Array(32)) type = FFI.array(type, 1);
    }).toThrow(TypeError);
  });

  test("uses nominal aggregate descriptors for nested fields", () => {
    const Point = FFI.struct({ x: "i32" });
    const OtherPoint = FFI.struct({ x: "i32" });
    const Holder = FFI.struct({ point: Point });

    expect(() => Holder.create({ point: OtherPoint.create({ x: 1 }) })).toThrow(
      TypeError,
    );
  });

  test("rejects detached aggregate backing stores and nested views", () => {
    const Point = FFI.struct({ x: "f64", y: "f64" });
    const Holder = FFI.struct({ point: Point });
    const point = Point.create({ x: 1, y: 2 });
    const holder = Holder.create({ point });
    const nestedPoint = holder.point;

    point.buffer.transfer();
    holder.buffer.transfer();

    expect(() => point.x).toThrow(TypeError);
    expect(() => {
      point.x = 3;
    }).toThrow(TypeError);
    expect(() => nestedPoint.x).toThrow(TypeError);
    expect(() => {
      nestedPoint.x = 3;
    }).toThrow(TypeError);
  });

  test("maps cumulative native call layout limits to JavaScript errors", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const Huge = FFI.array("u8", 65536);
    const HugeCallback = FFI.callback({
      args: [Huge, Huge],
      returns: "void",
    });

    try {
      expect(() =>
        lib.bind("get_answer", {
          args: [Huge, Huge],
          returns: "i32",
        }),
      ).toThrow(RangeError);
      expect(() => HugeCallback.create(() => {})).toThrow(RangeError);
    } finally {
      lib.close();
    }
  });
});
