describe("TextEncoder.prototype.encoding", () => {
  test("returns utf-8", () => {
    const enc = new TextEncoder();
    expect(enc.encoding).toBe("utf-8");
  });

  test("is a getter-only accessor (no setter defined)", () => {
    const desc = Object.getOwnPropertyDescriptor(TextEncoder.prototype, "encoding");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.configurable).toBe(true);
  });

  test("throws TypeError when called on a non-TextEncoder receiver", () => {
    expect(() =>
      Object.getOwnPropertyDescriptor(TextEncoder.prototype, "encoding").get.call({})
    ).toThrow(TypeError);
  });
});
