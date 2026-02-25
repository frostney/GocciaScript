describe("TypedArray Symbol.toStringTag", () => {
  const constructors = [
    [Int8Array, "[object Int8Array]"],
    [Uint8Array, "[object Uint8Array]"],
    [Uint8ClampedArray, "[object Uint8ClampedArray]"],
    [Int16Array, "[object Int16Array]"],
    [Uint16Array, "[object Uint16Array]"],
    [Int32Array, "[object Int32Array]"],
    [Uint32Array, "[object Uint32Array]"],
    [Float32Array, "[object Float32Array]"],
    [Float64Array, "[object Float64Array]"],
  ];
  constructors.forEach((pair) => {
    const Ctor = pair[0];
    const tag = pair[1];
    test(tag, () => {
      expect(Object.prototype.toString.call(new Ctor(0))).toBe(tag);
    });
    test(tag + " accessor returns tag", () => {
      const typedArrayProto = Object.getPrototypeOf(Ctor.prototype);
      const desc = Object.getOwnPropertyDescriptor(typedArrayProto, Symbol.toStringTag);
      expect(desc.get).toBeDefined();
      const shortTag = tag.slice(8, -1);
      expect(desc.get.call(new Ctor(0))).toBe(shortTag);
    });
    test(tag + " accessor returns undefined for non-TypedArray receiver", () => {
      const typedArrayProto = Object.getPrototypeOf(Ctor.prototype);
      const desc = Object.getOwnPropertyDescriptor(typedArrayProto, Symbol.toStringTag);
      expect(desc.get.call({})).toBe(undefined);
    });
  });
});
