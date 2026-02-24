describe("TypedArray Symbol.toStringTag", () => {
  test("Int8Array tag", () => {
    expect(Object.prototype.toString.call(new Int8Array(0))).toBe("[object Int8Array]");
  });
  test("Uint8Array tag", () => {
    expect(Object.prototype.toString.call(new Uint8Array(0))).toBe("[object Uint8Array]");
  });
  test("Uint8ClampedArray tag", () => {
    expect(Object.prototype.toString.call(new Uint8ClampedArray(0))).toBe("[object Uint8ClampedArray]");
  });
  test("Int16Array tag", () => {
    expect(Object.prototype.toString.call(new Int16Array(0))).toBe("[object Int16Array]");
  });
  test("Uint16Array tag", () => {
    expect(Object.prototype.toString.call(new Uint16Array(0))).toBe("[object Uint16Array]");
  });
  test("Int32Array tag", () => {
    expect(Object.prototype.toString.call(new Int32Array(0))).toBe("[object Int32Array]");
  });
  test("Uint32Array tag", () => {
    expect(Object.prototype.toString.call(new Uint32Array(0))).toBe("[object Uint32Array]");
  });
  test("Float32Array tag", () => {
    expect(Object.prototype.toString.call(new Float32Array(0))).toBe("[object Float32Array]");
  });
  test("Float64Array tag", () => {
    expect(Object.prototype.toString.call(new Float64Array(0))).toBe("[object Float64Array]");
  });
});
