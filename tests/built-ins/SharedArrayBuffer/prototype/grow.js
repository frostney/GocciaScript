describe("SharedArrayBuffer.prototype.grow", () => {
  test("raises byteLength and returns undefined", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 16 });
    expect(sab.grow(12)).toBeUndefined();
    expect(sab.byteLength).toBe(12);
    expect(sab.maxByteLength).toBe(16);
  });

  test("keeps the bytes written before the grow, through every view", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 64 });
    const fixed = new Uint8Array(sab, 0, 4);
    const tracking = new Uint8Array(sab);
    fixed.set([1, 2, 3, 4]);
    sab.grow(64);
    expect(Array.from(fixed)).toEqual([1, 2, 3, 4]);
    expect(Array.from(tracking.subarray(0, 4))).toEqual([1, 2, 3, 4]);
    tracking[0] = 9;
    expect(fixed[0]).toBe(9);
  });

  test("exposes zero bytes past the old length", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 32 });
    new Uint8Array(sab).fill(255);
    sab.grow(32);
    expect(Array.from(new Uint8Array(sab, 4))).toEqual(new Array(28).fill(0));
  });

  test("length-tracking views follow the grow and fixed-length views keep their length", () => {
    const sab = new SharedArrayBuffer(8, { maxByteLength: 32 });
    const tracking = new Int32Array(sab);
    const fixed = new Int32Array(sab, 0, 2);
    sab.grow(24);
    expect(tracking.length).toBe(6);
    expect(fixed.length).toBe(2);
  });

  test("growing to the current length does nothing", () => {
    const sab = new SharedArrayBuffer(8, { maxByteLength: 16 });
    sab.grow(8);
    expect(sab.byteLength).toBe(8);
  });

  test("throws RangeError when shrinking or growing past maxByteLength", () => {
    const sab = new SharedArrayBuffer(8, { maxByteLength: 16 });
    expect(() => sab.grow(4)).toThrow(RangeError);
    expect(() => sab.grow(17)).toThrow(RangeError);
    expect(sab.byteLength).toBe(8);
  });

  test("throws TypeError for a fixed-length buffer before converting newLength", () => {
    const sab = new SharedArrayBuffer(8);
    let conversions = 0;
    const newLength = { valueOf: () => { conversions++; return 8; } };
    expect(() => sab.grow(newLength)).toThrow(TypeError);
    expect(conversions).toBe(0);
  });

  test("throws TypeError for a receiver that is not a SharedArrayBuffer", () => {
    const grow = SharedArrayBuffer.prototype.grow;
    expect(() => grow.call(new ArrayBuffer(8, { maxByteLength: 16 }), 8)).toThrow(TypeError);
    expect(() => grow.call({}, 8)).toThrow(TypeError);
  });

  // The buffer reserves maxByteLength bytes, but only byteLength of them are
  // its contents: nothing may reach the bytes past byteLength.
  test("views cannot reach past byteLength before a grow", () => {
    const sab = new SharedArrayBuffer(8, { maxByteLength: 64 });
    expect(new Uint8Array(sab).length).toBe(8);
    expect(() => new Uint8Array(sab, 16)).toThrow(RangeError);
    expect(() => new Int32Array(sab, 0, 4)).toThrow(RangeError);
    expect(() => new DataView(sab, 0, 16)).toThrow(RangeError);
    expect(() => new DataView(sab).getInt8(8)).toThrow(RangeError);
    expect(() => Atomics.load(new Int32Array(sab, 0, 2), 2)).toThrow(RangeError);
    expect(sab.slice(0).byteLength).toBe(8);
    expect(sab.slice(0, 64).byteLength).toBe(8);
    sab.grow(16);
    expect(new Int32Array(sab, 0, 4).length).toBe(4);
    expect(sab.slice(0, 64).byteLength).toBe(16);
  });

  test("structuredClone copies byteLength bytes", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 64 });
    new Uint8Array(sab).set([1, 2, 3, 4]);
    sab.grow(6);
    const clone = structuredClone(sab);
    expect(clone.byteLength).toBe(6);
    expect(Array.from(new Uint8Array(clone))).toEqual([1, 2, 3, 4, 0, 0]);
  });
});
