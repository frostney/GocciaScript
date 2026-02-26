describe("SharedArrayBuffer Symbol.toStringTag", () => {
  test("toStringTag is 'SharedArrayBuffer'", () => {
    const sab = new SharedArrayBuffer(0);
    expect(sab[Symbol.toStringTag]).toBe("SharedArrayBuffer");
  });

  test("toStringTag on a non-empty buffer", () => {
    const sab = new SharedArrayBuffer(16);
    expect(sab[Symbol.toStringTag]).toBe("SharedArrayBuffer");
  });

  test("Object.prototype.toString returns [object SharedArrayBuffer]", () => {
    const sab = new SharedArrayBuffer(8);
    expect(Object.prototype.toString.call(sab)).toBe("[object SharedArrayBuffer]");
  });

  test("Object.prototype.toString on zero-length buffer", () => {
    const sab = new SharedArrayBuffer(0);
    expect(Object.prototype.toString.call(sab)).toBe("[object SharedArrayBuffer]");
  });
});
