describe("Object.seal", () => {
  test("prevents adding new properties", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    expect(() => { obj.b = 2; }).toThrow(TypeError);
  });

  test("allows modifying existing writable properties", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    obj.a = 2;
    expect(obj.a).toBe(2);
  });

  test("returns the same object", () => {
    const obj = { a: 1 };
    const result = Object.seal(obj);
    expect(result).toBe(obj);
  });

  test("non-objects are returned as-is", () => {
    expect(Object.seal(42)).toBe(42);
    expect(Object.seal("hello")).toBe("hello");
  });

  test("sealed object is not extensible", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    expect(Object.isExtensible(obj)).toBe(false);
  });

  test("sealing String objects preserves virtual indices and length", () => {
    const str = new String("abc");
    str.foo = 10;

    Object.seal(str);

    const index = Object.getOwnPropertyDescriptor(str, "0");
    const length = Object.getOwnPropertyDescriptor(str, "length");
    const foo = Object.getOwnPropertyDescriptor(str, "foo");

    expect(Object.isSealed(str)).toBe(true);
    expect(index.value).toBe("a");
    expect(index.writable).toBe(false);
    expect(index.enumerable).toBe(true);
    expect(index.configurable).toBe(false);
    expect(length.value).toBe(3);
    expect(length.writable).toBe(false);
    expect(length.enumerable).toBe(false);
    expect(length.configurable).toBe(false);
    expect(foo.value).toBe(10);
    expect(foo.writable).toBe(true);
    expect(foo.configurable).toBe(false);
  });
});

describe("Object.seal on a class", () => {
  test("returns the class, makes its own properties non-configurable and keeps writable statics writable", () => {
    class Settings {
      static level = 1;
    }

    expect(Object.seal(Settings)).toBe(Settings);
    expect(Object.isSealed(Settings)).toBe(true);
    expect(Object.isFrozen(Settings)).toBe(false);
    expect(Object.isExtensible(Settings)).toBe(false);

    Settings.level = 2;
    expect(Settings.level).toBe(2);
    expect(() => {
      Settings.added = 1;
    }).toThrow(TypeError);
    expect(() => {
      delete Settings.level;
    }).toThrow(TypeError);
    expect(() => {
      delete Settings.name;
    }).toThrow(TypeError);
    expect(Object.getOwnPropertyDescriptors(Settings)).toEqual({
      length: { value: 0, writable: false, enumerable: false, configurable: false },
      name: { value: "Settings", writable: false, enumerable: false, configurable: false },
      prototype: { value: Settings.prototype, writable: false, enumerable: false, configurable: false },
      level: { value: 2, writable: true, enumerable: true, configurable: false },
    });
  });

  test("a sealed class with no writable static is also frozen", () => {
    class Empty {}
    class Base {
      constructor(a) {}
    }
    class Derived extends Base {}

    for (const K of [Empty, Derived, class {}]) {
      Object.seal(K);
      expect(Object.isSealed(K)).toBe(true);
      expect(Object.isFrozen(K)).toBe(true);
    }
  });

  test("rejects a new value for the name of a sealed class and accepts the current one", () => {
    class Sealed {}
    Object.seal(Sealed);

    expect(() => Object.defineProperty(Sealed, "name", { value: "Other" })).toThrow(TypeError);
    Object.defineProperty(Sealed, "name", { value: "Sealed" });
    expect(Sealed.name).toBe("Sealed");
  });

  test("seals a built-in constructor", () => {
    Object.seal(WeakMap);

    expect(Object.isSealed(WeakMap)).toBe(true);
    expect(Object.getOwnPropertyDescriptor(WeakMap, "length")).toEqual({
      value: 0,
      writable: false,
      enumerable: false,
      configurable: false,
    });
    expect(new WeakMap().has({})).toBe(false);
  });
});
