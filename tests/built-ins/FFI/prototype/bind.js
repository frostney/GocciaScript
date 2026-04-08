describe("FFILibrary.prototype.bind", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture.dylib");

  test("binds and calls a no-arg function returning i32", () => {
    const getAnswer = lib.bind("get_answer", { args: [], returns: "i32" });
    expect(getAnswer()).toBe(42);
  });

  test("binds and calls a two-arg i32 function", () => {
    const add = lib.bind("add_i32", { args: ["i32", "i32"], returns: "i32" });
    expect(add(2, 3)).toBe(5);
    expect(add(-10, 10)).toBe(0);
    expect(add(100, 200)).toBe(300);
  });

  test("binds and calls multiplication", () => {
    const mul = lib.bind("mul_i32", { args: ["i32", "i32"], returns: "i32" });
    expect(mul(6, 7)).toBe(42);
    expect(mul(-3, 4)).toBe(-12);
  });

  test("binds and calls negation", () => {
    const negate = lib.bind("negate_i32", { args: ["i32"], returns: "i32" });
    expect(negate(42)).toBe(-42);
    expect(negate(-7)).toBe(7);
    expect(negate(0)).toBe(0);
  });

  test("binds and calls a void function", () => {
    const increment = lib.bind("increment_counter", { args: [], returns: "void" });
    const getCounter = lib.bind("get_counter", { args: [], returns: "i32" });
    const before = getCounter();
    increment();
    increment();
    expect(getCounter()).toBe(before + 2);
  });

  test("binds and calls f64 functions", () => {
    const addF64 = lib.bind("add_f64", { args: ["f64", "f64"], returns: "f64" });
    expect(addF64(1.5, 2.5)).toBe(4);
    expect(addF64(0.1, 0.2)).toBeCloseTo(0.3, 10);
  });

  test("binds and calls a no-arg f64 function", () => {
    const pi = lib.bind("pi", { args: [], returns: "f64" });
    expect(pi()).toBeCloseTo(3.14159265358979, 10);
  });

  test("binds and calls f32 functions", () => {
    const addF32 = lib.bind("add_f32", { args: ["f32", "f32"], returns: "f32" });
    const result = addF32(1.5, 2.5);
    expect(result).toBe(4);
  });

  test("binds and calls a cstring function", () => {
    const strlen = lib.bind("string_length", { args: ["cstring"], returns: "i32" });
    expect(strlen("hello")).toBe(5);
    expect(strlen("")).toBe(0);
    expect(strlen("GocciaScript")).toBe(12);
  });

  test("binds and calls a function returning cstring", () => {
    const greet = lib.bind("greeting", { args: [], returns: "cstring" });
    expect(greet()).toBe("hello from C");
  });

  test("binds and calls a 6-arg integer function", () => {
    const sum6 = lib.bind("sum6", {
      args: ["i32", "i32", "i32", "i32", "i32", "i32"],
      returns: "i32",
    });
    expect(sum6(1, 2, 3, 4, 5, 6)).toBe(21);
  });

  test("binds and calls a 4-arg f64 function", () => {
    const sum4 = lib.bind("sum4_f64", {
      args: ["f64", "f64", "f64", "f64"],
      returns: "f64",
    });
    expect(sum4(1.0, 2.0, 3.0, 4.0)).toBe(10);
  });

  test("throws on unknown symbol", () => {
    expect(() => lib.bind("nonexistent_fn", { args: [], returns: "void" })).toThrow(TypeError);
  });

  test("throws on unknown type", () => {
    expect(() => lib.bind("get_answer", { args: ["unknown"], returns: "i32" })).toThrow(TypeError);
  });

  test("throws on void argument type", () => {
    expect(() => lib.bind("get_answer", { args: ["void"], returns: "i32" })).toThrow(TypeError);
  });

  test("throws on mixed integer/float args", () => {
    expect(() =>
      lib.bind("get_answer", { args: ["i32", "f64"], returns: "i32" })
    ).toThrow(TypeError);
  });

  test("throws when binding from closed library", () => {
    const closed = FFI.open("./fixtures/ffi/libfixture.dylib");
    closed.close();
    expect(() => closed.bind("get_answer", { args: [], returns: "i32" })).toThrow(TypeError);
  });
});
