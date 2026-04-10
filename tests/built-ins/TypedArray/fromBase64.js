describe("Uint8Array.fromBase64", () => {
  test("decodes standard base64 string", () => {
    const result = Uint8Array.fromBase64("SGVsbG8=");
    expect(result.length).toBe(5);
    expect(result[0]).toBe(72);
    expect(result[1]).toBe(101);
    expect(result[4]).toBe(111);
  });

  test("decodes base64 without padding (loose mode)", () => {
    const result = Uint8Array.fromBase64("SGVsbG8");
    expect(result.length).toBe(5);
    expect(result[0]).toBe(72);
  });

  test("decodes empty string", () => {
    const result = Uint8Array.fromBase64("");
    expect(result.length).toBe(0);
  });

  test("decodes base64url alphabet", () => {
    const result = Uint8Array.fromBase64("-__-", { alphabet: "base64url" });
    expect(result.length).toBe(3);
    expect(result[0]).toBe(251);
    expect(result[1]).toBe(255);
    expect(result[2]).toBe(254);
  });

  test("skips ASCII whitespace", () => {
    const result = Uint8Array.fromBase64("S G V s\nb G 8 =");
    expect(result.length).toBe(5);
    expect(result[0]).toBe(72);
  });

  test("returns a Uint8Array instance", () => {
    const result = Uint8Array.fromBase64("QUJD");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(65);
    expect(result[1]).toBe(66);
    expect(result[2]).toBe(67);
  });

  test("throws TypeError when argument is not a string", () => {
    expect(() => Uint8Array.fromBase64(123)).toThrow(TypeError);
    expect(() => Uint8Array.fromBase64(null)).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => Uint8Array.fromBase64()).toThrow(TypeError);
  });

  test("throws SyntaxError for invalid characters", () => {
    expect(() => Uint8Array.fromBase64("!!!")).toThrow(SyntaxError);
    expect(() => Uint8Array.fromBase64("AB@D")).toThrow(SyntaxError);
  });

  test("throws SyntaxError for single trailing character", () => {
    expect(() => Uint8Array.fromBase64("QUJDA")).toThrow(SyntaxError);
  });

  test("strict mode requires proper padding", () => {
    expect(() => Uint8Array.fromBase64("SGVsbG8", {
      lastChunkHandling: "strict"
    })).toThrow(SyntaxError);
  });

  test("strict mode accepts properly padded input", () => {
    const result = Uint8Array.fromBase64("SGVsbG8=", {
      lastChunkHandling: "strict"
    });
    expect(result.length).toBe(5);
  });

  test("strict mode rejects non-zero padding bits", () => {
    // "AR==" has non-zero padding bits (the R encodes 0x11, trailing bits are non-zero)
    expect(() => Uint8Array.fromBase64("AR==", {
      lastChunkHandling: "strict"
    })).toThrow(SyntaxError);
  });

  test("stop-before-partial ignores incomplete final chunk", () => {
    const result = Uint8Array.fromBase64("QUJDQQ", {
      lastChunkHandling: "stop-before-partial"
    });
    expect(result.length).toBe(3);
    expect(result[0]).toBe(65);
    expect(result[1]).toBe(66);
    expect(result[2]).toBe(67);
  });

  test("throws TypeError for invalid alphabet option", () => {
    expect(() => Uint8Array.fromBase64("AA==", { alphabet: "invalid" })).toThrow(TypeError);
  });

  test("throws TypeError for invalid lastChunkHandling option", () => {
    expect(() => Uint8Array.fromBase64("AA==", { lastChunkHandling: "invalid" })).toThrow(TypeError);
  });

  test("decodes all zeros", () => {
    const result = Uint8Array.fromBase64("AAAA");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(0);
    expect(result[2]).toBe(0);
  });

  test("decodes all 255s", () => {
    const result = Uint8Array.fromBase64("////");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(255);
    expect(result[1]).toBe(255);
    expect(result[2]).toBe(255);
  });
});
