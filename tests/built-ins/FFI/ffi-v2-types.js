describe("FFI type descriptors", () => {
  test("rejects invalid aggregate and callback definitions", () => {
    expect(() => FFI.struct({})).toThrow(TypeError);
    expect(() => FFI.union({})).toThrow(TypeError);
    expect(() => FFI.struct({ invalid: "void" })).toThrow(TypeError);
    expect(() => FFI.array("u8", 0)).toThrow(TypeError);
    expect(() => FFI.array("u8", 1.5)).toThrow(TypeError);
    expect(() => FFI.callback({ args: "i32", returns: "i32" })).toThrow(
      TypeError,
    );
  });

  test("prioritizes exact native field names over direct metadata properties", () => {
    const Collision = FFI.struct({ buffer: "i32", byteOffset: "i32" });
    const collision = Collision.create({ buffer: 17, byteOffset: 25 });
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const checksum = lib.bind("ffi_metadata_collision_checksum", {
      args: [Collision],
      returns: "i32",
    });
    const metadata = FFI.metadata(collision);
    const Bytes = FFI.array("u8", 4);
    const bytes = Bytes.create([1, 2, 3, 4]);
    const bytesMetadata = FFI.metadata(bytes);

    try {
      expect(collision.buffer).toBe(17);
      expect(collision.byteOffset).toBe(25);
      expect(checksum(collision)).toBe(42);
      expect(metadata.buffer).toBeInstanceOf(ArrayBuffer);
      expect(metadata.byteOffset).toBe(0);
      expect(metadata.size).toBe(8);
      expect(bytesMetadata.buffer).toBe(bytes.buffer);
      expect(bytesMetadata.byteOffset).toBe(0);
      expect(bytesMetadata.size).toBe(4);
      expect(bytesMetadata.length).toBe(4);
      expect(() => FFI.metadata({})).toThrow(TypeError);
      metadata.buffer.transfer();
      expect(() => FFI.metadata(collision)).toThrow(TypeError);
    } finally {
      lib.close();
    }
  });

  test("limits nullable descriptors to bound utf8string arguments", () => {
    const NullableUTF8 = FFI.nullable("utf8string");
    const FixedCallback = FFI.callback({
      args: [],
      variadic: false,
      returns: "void",
    });
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);

    try {
      expect(FixedCallback.kind).toBe("callback");
      expect(() => FFI.nullable("pointer")).toThrow(TypeError);
      expect(() => FFI.struct({ value: NullableUTF8 })).toThrow(TypeError);
      expect(() =>
        FFI.callback({ args: [NullableUTF8], returns: "void" }),
      ).toThrow(TypeError);
      expect(() =>
        FFI.callback({ args: [], returns: NullableUTF8 }),
      ).toThrow(TypeError);
      expect(() =>
        FFI.callback({ args: [], variadic: true, returns: "void" }),
      ).toThrow(TypeError);
      expect(() =>
        lib.bind("greeting", { args: [], returns: NullableUTF8 }),
      ).toThrow(TypeError);
    } finally {
      lib.close();
    }
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
