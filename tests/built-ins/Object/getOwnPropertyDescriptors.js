describe("Object.getOwnPropertyDescriptors", () => {
  test("returns descriptors for all own string-keyed data properties", () => {
    const obj = { a: 1, b: 2 };
    const descs = Object.getOwnPropertyDescriptors(obj);

    expect(descs.a).toEqual({
      value: 1,
      writable: true,
      enumerable: true,
      configurable: true,
    });
    expect(descs.b).toEqual({
      value: 2,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("returns descriptors for non-enumerable properties", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      writable: false,
      enumerable: false,
      configurable: false,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.hidden).toEqual({
      value: 42,
      writable: false,
      enumerable: false,
      configurable: false,
    });
  });

  test("returns descriptors for accessor properties", () => {
    const obj = {};
    let value = 10;
    const getter = () => value;
    const setter = (v) => { value = v; };
    Object.defineProperty(obj, "prop", {
      get: getter,
      set: setter,
      enumerable: true,
      configurable: true,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.prop.get).toBe(getter);
    expect(descs.prop.set).toBe(setter);
    expect(descs.prop.enumerable).toBe(true);
    expect(descs.prop.configurable).toBe(true);
    expect(descs.prop.value).toBeUndefined();
    expect(descs.prop.writable).toBeUndefined();
  });

  test("returns descriptors for getter-only accessor", () => {
    const obj = {};
    const getter = () => 99;
    Object.defineProperty(obj, "readOnly", {
      get: getter,
      enumerable: false,
      configurable: true,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.readOnly.get).toBe(getter);
    expect(descs.readOnly.set).toBeUndefined();
    expect(descs.readOnly.enumerable).toBe(false);
    expect(descs.readOnly.configurable).toBe(true);
  });

  test("returns descriptors for setter-only accessor", () => {
    const obj = {};
    let stored;
    const setter = (v) => { stored = v; };
    Object.defineProperty(obj, "writeOnly", {
      set: setter,
      enumerable: true,
      configurable: false,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.writeOnly.get).toBeUndefined();
    expect(descs.writeOnly.set).toBe(setter);
    expect(descs.writeOnly.enumerable).toBe(true);
    expect(descs.writeOnly.configurable).toBe(false);
  });

  test("returns descriptors for symbol-keyed properties", () => {
    const sym = Symbol("mySymbol");
    const obj = {};
    obj[sym] = "symbolValue";

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs[sym]).toEqual({
      value: "symbolValue",
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("returns descriptors for both string and symbol keys", () => {
    const sym = Symbol("s");
    const obj = { name: "test" };
    obj[sym] = 42;

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.name.value).toBe("test");
    expect(descs[sym].value).toBe(42);
  });

  test("returns empty object for object with no own properties", () => {
    const obj = Object.create({ inherited: true });
    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(Object.keys(descs).length).toBe(0);
  });

  test("does not include inherited properties", () => {
    const parent = { parentProp: 1 };
    const child = Object.create(parent);
    child.childProp = 2;

    const descs = Object.getOwnPropertyDescriptors(child);
    expect(descs.childProp).toEqual({
      value: 2,
      writable: true,
      enumerable: true,
      configurable: true,
    });
    expect(descs.parentProp).toBeUndefined();
  });

  test("handles mixed data and accessor properties", () => {
    const obj = { x: 1 };
    Object.defineProperty(obj, "y", {
      get: () => 2,
      enumerable: true,
      configurable: true,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);

    expect(descs.x.value).toBe(1);
    expect(descs.x.writable).toBe(true);

    expect(descs.y.get).toBeDefined();
    expect(descs.y.value).toBeUndefined();
  });

  test("preserves all descriptor attributes from defineProperty", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: true,
      enumerable: false,
      configurable: true,
    });
    Object.defineProperty(obj, "b", {
      value: 2,
      writable: false,
      enumerable: true,
      configurable: false,
    });

    const descs = Object.getOwnPropertyDescriptors(obj);

    expect(descs.a.writable).toBe(true);
    expect(descs.a.enumerable).toBe(false);
    expect(descs.a.configurable).toBe(true);

    expect(descs.b.writable).toBe(false);
    expect(descs.b.enumerable).toBe(true);
    expect(descs.b.configurable).toBe(false);
  });

  test("with number coerces to empty descriptor object", () => {
    expect(Object.getOwnPropertyDescriptors(42)).toEqual({});
    expect(Object.getOwnPropertyDescriptors(true)).toEqual({});
  });

  test("with string returns character index descriptors", () => {
    const descs = Object.getOwnPropertyDescriptors("hi");
    expect(descs["0"].value).toBe("h");
    expect(descs["0"].enumerable).toBe(true);
    expect(descs["0"].writable).toBe(false);
    expect(descs["0"].configurable).toBe(false);
    expect(descs["1"].value).toBe("i");
    expect(descs["length"].value).toBe(2);
    expect(descs["length"].enumerable).toBe(false);
  });

  test("throws TypeError for null", () => {
    expect(() => Object.getOwnPropertyDescriptors(null)).toThrow(TypeError);
  });

  test("throws TypeError for undefined", () => {
    expect(() => Object.getOwnPropertyDescriptors(undefined)).toThrow(TypeError);
  });

  test("works with frozen objects", () => {
    const obj = { a: 1 };
    Object.freeze(obj);

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.a.value).toBe(1);
    expect(descs.a.writable).toBe(false);
    expect(descs.a.configurable).toBe(false);
  });

  test("works with sealed objects", () => {
    const obj = { a: 1 };
    Object.seal(obj);

    const descs = Object.getOwnPropertyDescriptors(obj);
    expect(descs.a.value).toBe(1);
    expect(descs.a.writable).toBe(true);
    expect(descs.a.configurable).toBe(false);
  });

  test("ignores extra arguments", () => {
    const obj = { a: 1 };
    const descs = Object.getOwnPropertyDescriptors(obj, "extra", 123);
    expect(descs.a.value).toBe(1);
  });

  test("can be used with Object.defineProperties for shallow clone", () => {
    const original = { x: 1, y: 2 };
    Object.defineProperty(original, "z", {
      value: 3,
      enumerable: false,
    });

    const clone = Object.create(Object.getPrototypeOf(original));
    Object.defineProperties(clone, Object.getOwnPropertyDescriptors(original));

    expect(clone.x).toBe(1);
    expect(clone.y).toBe(2);
    expect(clone.z).toBe(3);

    const cloneDescs = Object.getOwnPropertyDescriptors(clone);
    expect(cloneDescs.z.enumerable).toBe(false);
  });
});
