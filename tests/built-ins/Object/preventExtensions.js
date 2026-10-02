describe("Object.preventExtensions", () => {
  test("prevents adding new properties", () => {
    const obj = { a: 1 };
    Object.preventExtensions(obj);
    expect(() => { obj.b = 2; }).toThrow(TypeError);
  });

  test("allows modifying existing properties", () => {
    const obj = { a: 1 };
    Object.preventExtensions(obj);
    obj.a = 2;
    expect(obj.a).toBe(2);
  });

  test("returns the same object", () => {
    const obj = {};
    const result = Object.preventExtensions(obj);
    expect(result).toBe(obj);
  });

  test("throws when the object internal method returns false", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 8 });

    expect(() => Object.preventExtensions(new Uint8Array(buffer))).toThrow(TypeError);
  });

  test("non-objects are returned as-is", () => {
    expect(Object.preventExtensions(42)).toBe(42);
  });
});

describe("Object.preventExtensions on a class", () => {
  test("blocks new statics and leaves length and name configurable", () => {
    class K {
      constructor(a) {}
    }

    expect(Object.preventExtensions(K)).toBe(K);
    expect(Object.isExtensible(K)).toBe(false);
    expect(Object.isSealed(K)).toBe(false);
    expect(() => {
      K.added = 1;
    }).toThrow(TypeError);
    expect(() => Object.defineProperty(K, "added", { value: 1 })).toThrow(TypeError);
    expect(Object.getOwnPropertyDescriptors(K)).toEqual({
      length: { value: 1, writable: false, enumerable: false, configurable: true },
      name: { value: "K", writable: false, enumerable: false, configurable: true },
      prototype: { value: K.prototype, writable: false, enumerable: false, configurable: false },
    });
  });

  test("still allows redefining and deleting the length and name the class already has", () => {
    class K {}
    Object.preventExtensions(K);

    Object.defineProperty(K, "name", { value: "Renamed" });
    Object.defineProperties(K, { length: { value: 7 } });
    expect(Object.getOwnPropertyDescriptor(K, "name")).toEqual({
      value: "Renamed",
      writable: false,
      enumerable: false,
      configurable: true,
    });
    expect(Object.getOwnPropertyDescriptor(K, "length")).toEqual({
      value: 7,
      writable: false,
      enumerable: false,
      configurable: true,
    });

    expect(delete K.name).toBe(true);
    expect(Object.getOwnPropertyNames(K)).toEqual(["length", "prototype"]);
    expect(K.name).toBe("");
  });

  test("a static field after a static block that prevents extensions throws", () => {
    expect(() => {
      class K {
        static {
          Object.preventExtensions(this);
        }
        static after = 1;
      }
    }).toThrow(TypeError);
  });
});
