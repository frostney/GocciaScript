describe("FFI.suffix", () => {
  test("returns a string starting with a dot", () => {
    expect(typeof FFI.suffix).toBe("string");
    expect(FFI.suffix[0]).toBe(".");
  });

  test("is one of the known platform suffixes", () => {
    const known = [".dylib", ".so", ".dll"];
    expect(known.includes(FFI.suffix)).toBe(true);
  });
});
