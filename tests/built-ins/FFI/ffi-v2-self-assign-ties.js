// A pointer read from a field that holds a library's symbol refuses to be used
// once that library is closed. This file opens the fixture once, so closing it
// unloads it.
describe("FFI aggregate assignment and library ties", () => {
  test("assigning a nested aggregate to itself keeps its library ties", () => {
    const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
    const Holder = FFI.struct({ p: "pointer" });
    const Outer = FFI.struct({ a: Holder, b: Holder });
    const self = Outer.create({ a: Holder.create({ p: lib.symbol("get_answer") }) });
    const copy = Outer.create({ a: Holder.create({ p: lib.symbol("get_answer") }) });
    self.a = self.a;
    copy.b = copy.a;
    lib.close();
    expect(() => self.a.p.address).toThrow(TypeError);
    expect(() => copy.a.p.address).toThrow(TypeError);
    expect(() => copy.b.p.address).toThrow(TypeError);
  });
});
