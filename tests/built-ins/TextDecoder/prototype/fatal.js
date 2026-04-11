describe("TextDecoder.prototype.fatal", () => {
  test("is false by default", () => {
    expect(new TextDecoder().fatal).toBe(false);
  });

  test("is true when fatal: true is passed", () => {
    expect(new TextDecoder("utf-8", { fatal: true }).fatal).toBe(true);
  });

  test("is false when fatal: false is passed explicitly", () => {
    expect(new TextDecoder("utf-8", { fatal: false }).fatal).toBe(false);
  });

  test("is a getter-only accessor (no setter defined)", () => {
    const desc = Object.getOwnPropertyDescriptor(TextDecoder.prototype, "fatal");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.configurable).toBe(true);
  });
});
