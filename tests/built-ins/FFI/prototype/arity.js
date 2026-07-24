describe("FFILibrary.prototype.bind (high-arity signatures)", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  afterAll(() => lib.close());

  test("calls a function with more than eight scalar arguments", () => {
    const sum9 = lib.bind("sum9", {
      args: [
        "i32",
        "i32",
        "i32",
        "i32",
        "i32",
        "i32",
        "i32",
        "i32",
        "i32",
      ],
      returns: "i32",
    });

    expect(sum9(1, 2, 3, 4, 5, 6, 7, 8, 9)).toBe(45);
  });

  test("calls a nine-argument DrawBillboardPro-shaped signature", () => {
    const Vector2 = FFI.struct({ x: "f32", y: "f32" });
    const Vector3 = FFI.struct({ x: "f32", y: "f32", z: "f32" });
    const Rectangle = FFI.struct({
      x: "f32",
      y: "f32",
      width: "f32",
      height: "f32",
    });
    const Camera3D = FFI.struct({
      position: Vector3,
      target: Vector3,
      up: Vector3,
      fovy: "f32",
      projection: "i32",
    });
    const Texture2D = FFI.struct({
      id: "u32",
      width: "i32",
      height: "i32",
      mipmaps: "i32",
      format: "i32",
    });
    const Color = FFI.struct({ r: "u8", g: "u8", b: "u8", a: "u8" });
    const checksum = lib.bind("ffi_draw_billboard_pro_checksum", {
      args: [
        Camera3D,
        Texture2D,
        Rectangle,
        Vector3,
        Vector3,
        Vector2,
        Vector2,
        "f32",
        Color,
      ],
      returns: "f64",
    });

    const camera = Camera3D.create({
      position: Vector3.create({ x: 1, y: 0, z: 0 }),
      target: Vector3.create({ x: 0, y: 2, z: 0 }),
      up: Vector3.create({ x: 0, y: 0, z: 3 }),
      fovy: 4,
      projection: 5,
    });
    const texture = Texture2D.create({
      id: 6,
      width: 7,
      height: 8,
      mipmaps: 9,
      format: 10,
    });
    const source = Rectangle.create({ x: 11, y: 12, width: 13, height: 14 });
    const position = Vector3.create({ x: 15, y: 16, z: 17 });
    const up = Vector3.create({ x: 18, y: 19, z: 20 });
    const size = Vector2.create({ x: 21, y: 22 });
    const origin = Vector2.create({ x: 23, y: 24 });
    const tint = Color.create({ r: 26, g: 27, b: 28, a: 29 });

    expect(
      checksum(camera, texture, source, position, up, size, origin, 25, tint),
    ).toBe(435);
  });
});
