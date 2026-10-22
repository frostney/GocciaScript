describe("Uint8Array.prototype.setFromHex", () => {
  test("has built-in method attributes", () => {
    expect(Object.getOwnPropertyDescriptor(Uint8Array.prototype, "setFromHex")).toEqual({
      value: Uint8Array.prototype.setFromHex,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  });

  test("decodes hex into existing array", () => {
    const target = new Uint8Array(5);
    const result = target.setFromHex("48656c6c6f");
    expect(result.read).toBe(10);
    expect(result.written).toBe(5);
    expect(target[0]).toBe(72);
    expect(target[4]).toBe(111);
  });

  test("returns { read, written } object", () => {
    const target = new Uint8Array(3);
    const result = target.setFromHex("414243");
    expect(result.read).toBe(6);
    expect(result.written).toBe(3);
  });

  test("stops writing when target is full", () => {
    const target = new Uint8Array(2);
    const result = target.setFromHex("414243");
    expect(result.written).toBe(2);
    expect(result.read).toBe(4);
    expect(target[0]).toBe(65);
    expect(target[1]).toBe(66);
  });

  test("handles empty input", () => {
    const target = new Uint8Array(5);
    const result = target.setFromHex("");
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("accepts uppercase hex", () => {
    const target = new Uint8Array(3);
    const result = target.setFromHex("AABBCC");
    expect(result.written).toBe(3);
    expect(target[0]).toBe(0xAA);
    expect(target[1]).toBe(0xBB);
    expect(target[2]).toBe(0xCC);
  });

  test("accepts mixed case hex", () => {
    const target = new Uint8Array(2);
    const result = target.setFromHex("aAbB");
    expect(target[0]).toBe(0xAA);
    expect(target[1]).toBe(0xBB);
  });

  test("throws TypeError when called on non-Uint8Array", () => {
    const fn = Uint8Array.prototype.setFromHex;
    expect(() => fn.call(new Int8Array(1), "ff")).toThrow(TypeError);
  });

  test("throws TypeError when first argument is not a string", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex(123)).toThrow(TypeError);
  });

  test("throws SyntaxError for odd-length hex string", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex("abc")).toThrow(SyntaxError);
  });

  test("throws SyntaxError for invalid hex characters", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex("zzzz")).toThrow(SyntaxError);
  });

  test("writes decoded bytes before throwing on later invalid data", () => {
    const target = new Uint8Array([255, 255, 255]);
    expect(() => target.setFromHex("aaag")).toThrow(SyntaxError);
    expect(target[0]).toBe(0xAA);
    expect(target[1]).toBe(255);
    expect(target[2]).toBe(255);
  });

  test("throws TypeError on detached targets", () => {
    const buffer = new ArrayBuffer(2);
    const target = new Uint8Array(buffer);
    buffer.transfer();

    expect(() => target.setFromHex("aa")).toThrow(TypeError);
  });

  test("throws TypeError on immutable targets before reading the string", () => {
    const buffer = new ArrayBuffer(2);
    const seed = new Uint8Array(buffer);
    seed[0] = 1;
    const target = new Uint8Array(buffer.transferToImmutable());

    expect(() => target.setFromHex("aa")).toThrow(TypeError);
    expect(target[0]).toBe(1);
  });

  test("writes into a length-tracking view after its buffer grew", () => {
    const rab = new ArrayBuffer(2, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    rab.resize(8);
    expect(view.setFromHex("0102030405060708")).toEqual({ read: 16, written: 8 });
    expect(Array.from(view)).toEqual([1, 2, 3, 4, 5, 6, 7, 8]);
    expect(Array.from(new Uint8Array(rab))).toEqual([1, 2, 3, 4, 5, 6, 7, 8]);
  });

  test("writes only the length a view has after its buffer shrank", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    rab.resize(4);
    expect(view.setFromHex("0102030405060708")).toEqual({ read: 8, written: 4 });
    expect(Array.from(view)).toEqual([1, 2, 3, 4]);
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 8 });
    const view = new Uint8Array(rab, 0, 8);
    rab.resize(4);
    expect(() => view.setFromHex("0102")).toThrow(TypeError);
  });
});
