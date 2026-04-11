describe("TextDecoder.prototype.encoding", () => {
  test("returns the normalised label", () => {
    expect(new TextDecoder("utf-8").encoding).toBe("utf-8");
    expect(new TextDecoder("utf8").encoding).toBe("utf-8");
    expect(new TextDecoder("UTF-8").encoding).toBe("utf-8");
  });

  test("is a getter-only accessor (no setter defined)", () => {
    const desc = Object.getOwnPropertyDescriptor(TextDecoder.prototype, "encoding");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.configurable).toBe(true);
  });
});
