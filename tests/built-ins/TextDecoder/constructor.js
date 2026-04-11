describe("TextDecoder constructor", () => {
  test("creates an instance with no arguments", () => {
    const dec = new TextDecoder();
    expect(dec instanceof TextDecoder).toBe(true);
  });

  test("defaults to utf-8 encoding", () => {
    const dec = new TextDecoder();
    expect(dec.encoding).toBe("utf-8");
  });

  test("accepts 'utf-8' label explicitly", () => {
    const dec = new TextDecoder("utf-8");
    expect(dec.encoding).toBe("utf-8");
  });

  test("accepts 'utf8' alias", () => {
    const dec = new TextDecoder("utf8");
    expect(dec.encoding).toBe("utf-8");
  });

  test("accepts 'UTF-8' (case-insensitive)", () => {
    const dec = new TextDecoder("UTF-8");
    expect(dec.encoding).toBe("utf-8");
  });

  test("accepts unicode-1-1-utf-8 alias", () => {
    const dec = new TextDecoder("unicode-1-1-utf-8");
    expect(dec.encoding).toBe("utf-8");
  });

  test("fatal defaults to false", () => {
    const dec = new TextDecoder();
    expect(dec.fatal).toBe(false);
  });

  test("ignoreBOM defaults to false", () => {
    const dec = new TextDecoder();
    expect(dec.ignoreBOM).toBe(false);
  });

  test("accepts fatal option", () => {
    const dec = new TextDecoder("utf-8", { fatal: true });
    expect(dec.fatal).toBe(true);
  });

  test("accepts ignoreBOM option", () => {
    const dec = new TextDecoder("utf-8", { ignoreBOM: true });
    expect(dec.ignoreBOM).toBe(true);
  });

  test("throws RangeError for unknown encoding label", () => {
    expect(() => new TextDecoder("latin1")).toThrow(RangeError);
  });

  test("throws RangeError for completely invalid label", () => {
    expect(() => new TextDecoder("not-an-encoding")).toThrow(RangeError);
  });

  test("has decode method", () => {
    const dec = new TextDecoder();
    expect(typeof dec.decode).toBe("function");
  });

  test("prototype has correct toStringTag", () => {
    const dec = new TextDecoder();
    expect(Object.prototype.toString.call(dec)).toBe("[object TextDecoder]");
  });
});
