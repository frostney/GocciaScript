describe("FFILibrary.prototype.bind (mixed int/float signatures)", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);

  test("calls a function with (i32, f64) args", () => {
    const scale = lib.bind("scale_f64", { args: ["i32", "f64"], returns: "f64" });
    expect(scale(3, 2.5)).toBe(7.5);
    expect(scale(0, 100.0)).toBe(0);
    expect(scale(-2, 1.5)).toBe(-3);
  });

  test("calls a function with (f64) arg returning i32", () => {
    const round = lib.bind("round_f64", { args: ["f64"], returns: "i32" });
    expect(round(3.7)).toBe(4);
    expect(round(2.2)).toBe(2);
    expect(round(0.5)).toBe(1);
  });

  test("calls a function with (f64, i32, f64, i32) args", () => {
    const wsum = lib.bind("weighted_sum", {
      args: ["f64", "i32", "f64", "i32"],
      returns: "f64",
    });
    // a * weight_a + b * weight_b
    expect(wsum(1.5, 2, 3.0, 4)).toBe(15); // 1.5*2 + 3.0*4 = 3 + 12
    expect(wsum(10.0, 1, 20.0, 1)).toBe(30);
  });

  test("calls a function with (i32, f64, i32) args", () => {
    const indexScale = lib.bind("index_scale", {
      args: ["i32", "f64", "i32"],
      returns: "f64",
    });
    // (index + offset) * scale
    expect(indexScale(5, 2.0, 3)).toBe(16); // (5 + 3) * 2.0
    expect(indexScale(0, 1.5, 10)).toBe(15); // (0 + 10) * 1.5
  });

  test("mixed signatures no longer throw", () => {
    // This used to throw TypeError — now it works
    const scale = lib.bind("scale_f64", { args: ["i32", "f64"], returns: "f64" });
    expect(typeof scale).toBe("function");
  });

  test("f32 mixing still throws", () => {
    expect(() =>
      lib.bind("get_answer", { args: ["i32", "f32"], returns: "i32" })
    ).toThrow(TypeError);
  });

  test("mixed signatures over 4 args throw", () => {
    expect(() =>
      lib.bind("get_answer", {
        args: ["i32", "f64", "i32", "f64", "i32"],
        returns: "i32",
      })
    ).toThrow(TypeError);
  });
});
