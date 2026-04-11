describe("TextDecoder.prototype.ignoreBOM", () => {
  test("is false by default", () => {
    expect(new TextDecoder().ignoreBOM).toBe(false);
  });

  test("is true when ignoreBOM: true is passed", () => {
    expect(new TextDecoder("utf-8", { ignoreBOM: true }).ignoreBOM).toBe(true);
  });

  test("is false when ignoreBOM: false is passed explicitly", () => {
    expect(new TextDecoder("utf-8", { ignoreBOM: false }).ignoreBOM).toBe(false);
  });

  test("is a getter-only accessor (no setter defined)", () => {
    const desc = Object.getOwnPropertyDescriptor(TextDecoder.prototype, "ignoreBOM");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.configurable).toBe(true);
  });
});
