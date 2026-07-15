describe("Uint8Array.prototype.setFromBase64", () => {
  test("has built-in method attributes", () => {
    expect(Object.getOwnPropertyDescriptor(Uint8Array.prototype, "setFromBase64")).toEqual({
      value: Uint8Array.prototype.setFromBase64,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  });

  test("decodes base64 into existing array", () => {
    const target = new Uint8Array(5);
    const result = target.setFromBase64("SGVsbG8=");
    expect(result.read).toBe(8);
    expect(result.written).toBe(5);
    expect(target[0]).toBe(72);
    expect(target[4]).toBe(111);
  });

  test("returns { read, written } object", () => {
    const target = new Uint8Array(3);
    const result = target.setFromBase64("QUJD");
    expect(result.read).toBe(4);
    expect(result.written).toBe(3);
  });

  test("does not consume chunk when output would overflow target", () => {
    // "QUJD" decodes to 3 bytes (ABC) but target only has 2 bytes
    // A full 4-char chunk produces 3 bytes atomically — can't partially write it
    const target = new Uint8Array(2);
    const result = target.setFromBase64("QUJD");
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("consumes only chunks that fully fit in target", () => {
    // "QUJDREVG" = "QUJD" (ABC, 3 bytes) + "REVG" (DEF, 3 bytes)
    // Target has 4 bytes: first chunk (3 bytes) fits, second chunk (3 bytes) does not
    const target = new Uint8Array(4);
    const result = target.setFromBase64("QUJDREVG");
    expect(result.read).toBe(4);
    expect(result.written).toBe(3);
    expect(target[0]).toBe(65);
    expect(target[1]).toBe(66);
    expect(target[2]).toBe(67);
  });

  test("handles empty input", () => {
    const target = new Uint8Array(5);
    const result = target.setFromBase64("");
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("handles base64url alphabet", () => {
    const target = new Uint8Array(3);
    const result = target.setFromBase64("-__-", { alphabet: "base64url" });
    expect(result.written).toBe(3);
    expect(target[0]).toBe(251);
    expect(target[1]).toBe(255);
    expect(target[2]).toBe(254);
  });

  test("skips whitespace in input", () => {
    const target = new Uint8Array(5);
    const result = target.setFromBase64("S G V s b G 8 =");
    expect(result.written).toBe(5);
    expect(target[0]).toBe(72);
  });

  test("throws TypeError when called on non-Uint8Array", () => {
    const fn = Uint8Array.prototype.setFromBase64;
    expect(() => fn.call(new Int8Array(1), "AA==")).toThrow(TypeError);
  });

  test("throws TypeError when first argument is not a string", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromBase64(123)).toThrow(TypeError);
  });

  test("throws SyntaxError for invalid base64 characters", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromBase64("!!!")).toThrow(SyntaxError);
  });

  test("loose mode decodes partial last chunk", () => {
    const target = new Uint8Array(5);
    const result = target.setFromBase64("SGVsbG8");
    expect(result.written).toBe(5);
  });

  test("strict mode rejects incomplete chunk without padding", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromBase64("SGVsbG8", { lastChunkHandling: "strict" })).toThrow(SyntaxError);
  });

  test("writes decoded bytes before throwing on later invalid data", () => {
    const target = new Uint8Array([255, 255, 255, 255, 255]);
    expect(() => target.setFromBase64("MjYyZm.9v")).toThrow(SyntaxError);
    expect(target[0]).toBe(50);
    expect(target[1]).toBe(54);
    expect(target[2]).toBe(50);
    expect(target[3]).toBe(255);
    expect(target[4]).toBe(255);
  });

  test("does not scan trailing garbage after target is full", () => {
    const target = new Uint8Array(3);
    const result = target.setFromBase64("aaaa#");
    expect(result.read).toBe(4);
    expect(result.written).toBe(3);
    expect(target[0]).toBe(105);
    expect(target[1]).toBe(166);
    expect(target[2]).toBe(154);
  });

  test("zero-length target ignores malformed input", () => {
    const target = new Uint8Array(0);
    const result = target.setFromBase64("#", {
      lastChunkHandling: "strict"
    });
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("throws TypeError when options detach the target before decoding", () => {
    const buffer = new ArrayBuffer(3);
    const target = new Uint8Array(buffer);
    let getterCalls = 0;
    const options = {};
    Object.defineProperty(options, "alphabet", {
      get() {
        getterCalls += 1;
        buffer.transfer();
        return "base64";
      }
    });

    expect(() => target.setFromBase64("Zg==", options)).toThrow(TypeError);
    expect(getterCalls).toBe(1);
  });

  test("throws TypeError on immutable targets before reading options", () => {
    const buffer = new ArrayBuffer(3);
    const seed = new Uint8Array(buffer);
    seed[0] = 1;
    const target = new Uint8Array(buffer.transferToImmutable());
    let getterCalls = 0;
    const options = {};
    Object.defineProperty(options, "alphabet", {
      get() {
        getterCalls += 1;
        return "base64";
      }
    });

    expect(() => target.setFromBase64("Zg==", options)).toThrow(TypeError);
    expect(getterCalls).toBe(0);
    expect(target[0]).toBe(1);
  });
});
