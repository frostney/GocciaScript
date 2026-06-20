describe("Atomics.add", () => {
  test("is exposed on the Atomics namespace object", () => {
    expect(typeof Atomics).toBe("object");
    expect(typeof Atomics.add).toBe("function");
    expect(Object.prototype.toString.call(Atomics)).toBe("[object Atomics]");
  });

  test("returns the old value and writes the sum", () => {
    const sab = new SharedArrayBuffer(8);
    const i32 = new Int32Array(sab);
    i32[0] = 5;

    expect(Atomics.add(i32, 0, 7)).toBe(5);
    expect(i32[0]).toBe(12);
  });

  test("wraps according to the target integer typed array kind", () => {
    const sab = new SharedArrayBuffer(1);
    const i8 = new Int8Array(sab);
    i8[0] = 127;

    expect(Atomics.add(i8, 0, 1)).toBe(127);
    expect(i8[0]).toBe(-128);
  });

  test("throws RangeError for out-of-bounds index", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);

    expect(() => Atomics.add(i32, 1, 1)).toThrow(RangeError);
  });

  test("works for non-shared ArrayBuffer views", () => {
    const buffer = new ArrayBuffer(4);
    const i32 = new Int32Array(buffer);

    expect(Atomics.add(i32, 0, 1)).toBe(0);
    expect(Atomics.load(i32, 0)).toBe(1);
  });

  test("throws TypeError for non-integer typed arrays", () => {
    const sab = new SharedArrayBuffer(4);
    const f32 = new Float32Array(sab);

    expect(() => Atomics.add(f32, 0, 1)).toThrow(TypeError);
  });
});
