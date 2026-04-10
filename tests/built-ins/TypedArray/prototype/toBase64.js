describe("Uint8Array.prototype.toBase64", () => {
  test("encodes bytes to base64 with padding", () => {
    const bytes = new Uint8Array([72, 101, 108, 108, 111]);
    expect(bytes.toBase64()).toBe("SGVsbG8=");
  });

  test("encodes bytes that produce no padding", () => {
    const bytes = new Uint8Array([72, 101, 108]);
    expect(bytes.toBase64()).toBe("SGVs");
  });

  test("encodes bytes that produce double padding", () => {
    const bytes = new Uint8Array([72]);
    expect(bytes.toBase64()).toBe("SA==");
  });

  test("returns empty string for empty array", () => {
    expect(new Uint8Array(0).toBase64()).toBe("");
  });

  test("omitPadding option removes trailing =", () => {
    const bytes = new Uint8Array([72, 101, 108, 108, 111]);
    expect(bytes.toBase64({ omitPadding: true })).toBe("SGVsbG8");
  });

  test("omitPadding with double padding", () => {
    const bytes = new Uint8Array([72]);
    expect(bytes.toBase64({ omitPadding: true })).toBe("SA");
  });

  test("omitPadding false is default", () => {
    const bytes = new Uint8Array([72]);
    expect(bytes.toBase64({ omitPadding: false })).toBe("SA==");
  });

  test("base64url alphabet uses - and _ instead of + and /", () => {
    const bytes = new Uint8Array([251, 255, 254]);
    expect(bytes.toBase64()).toBe("+//+");
    expect(bytes.toBase64({ alphabet: "base64url" })).toBe("-__-");
  });

  test("base64url with omitPadding", () => {
    const bytes = new Uint8Array([72]);
    expect(bytes.toBase64({ alphabet: "base64url", omitPadding: true })).toBe("SA");
  });

  test("throws TypeError for invalid alphabet option", () => {
    const bytes = new Uint8Array([1]);
    expect(() => bytes.toBase64({ alphabet: "invalid" })).toThrow(TypeError);
  });

  test("throws TypeError when called on non-Uint8Array", () => {
    const fn = Uint8Array.prototype.toBase64;
    expect(() => fn.call(new Int8Array(1))).toThrow(TypeError);
  });

  test("throws TypeError when options is not an object", () => {
    const bytes = new Uint8Array([1]);
    expect(() => bytes.toBase64("bad")).toThrow(TypeError);
  });

  test("works with buffer offset", () => {
    const buffer = new ArrayBuffer(8);
    const full = new Uint8Array(buffer);
    full[3] = 65;
    full[4] = 66;
    full[5] = 67;
    const view = new Uint8Array(buffer, 3, 3);
    expect(view.toBase64()).toBe("QUJD");
  });

  test("encodes all zero bytes", () => {
    const bytes = new Uint8Array([0, 0, 0]);
    expect(bytes.toBase64()).toBe("AAAA");
  });

  test("encodes all 255 bytes", () => {
    const bytes = new Uint8Array([255, 255, 255]);
    expect(bytes.toBase64()).toBe("////");
  });
});
