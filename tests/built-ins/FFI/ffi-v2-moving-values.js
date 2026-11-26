// The engine's nursery moves what survives a collection, so the FFI must not
// keep a value's old address across one. Each persistent callback is closed in
// a finally block (see ffi-v2-callback.js for why).
describe("FFI values a collection moves", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const Point = FFI.struct({ x: "f64", y: "f64" });
  const I32Callback = FFI.callback({ args: ["i32"], returns: "i32" });
  // enough short-lived allocation to run several minor collections
  const churn = () => {
    let keep = 0;
    for (const i of Array.from({ length: 20000 }, (_, k) => k)) keep += [i, i, i].length;
    return keep;
  };

  test("a variadic call reads its values after the collection an open callback runs", () => {
    const checksum = lib.bind("ffi_variadic_point_checksum", { args: ["i32"], variadic: true, returns: "f64" });
    const call = () => checksum(2, FFI.varargs([Point, Point], [Point.create({ x: 1, y: 2 }), Point.create({ x: 3, y: 4 })]));
    const expected = call();
    const open = I32Callback.create((v) => v);
    try {
      for (const _ of [1, 2, 3]) expect(call()).toBe(expected);
    } finally {
      open.close();
    }
  });

  test("a struct's pointer field to a buffer still reaches the buffer after collections", () => {
    const writeI32 = lib.bind("write_i32", { args: ["pointer", "i32"], returns: "void" });
    const Holder = FFI.struct({ p: "pointer" });
    const bytes = new Int32Array(4);
    const holder = Holder.create({ p: bytes.buffer });
    churn();
    writeI32(holder.p, 7);
    expect(bytes[0]).toBe(7);
  });

  test("a callback a later argument's conversion closes is refused before the call", () => {
    const call = lib.bind("ffi_v2_call_i32_callback", { args: [I32Callback, "i32"], returns: "i32" });
    const handle = I32Callback.create((v) => v + 100);
    try {
      expect(() => call(handle, { valueOf() { handle.close(); return 5; } })).toThrow(TypeError);
      expect(handle.closed).toBe(true);
    } finally {
      handle.close();
    }
  });
});
