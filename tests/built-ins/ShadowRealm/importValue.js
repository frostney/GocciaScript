describe("ShadowRealm.prototype.importValue", () => {
  test("is a function with name 'importValue' and length 2", () => {
    expect(typeof ShadowRealm.prototype.importValue).toBe("function");
    expect(ShadowRealm.prototype.importValue.name).toBe("importValue");
    expect(ShadowRealm.prototype.importValue.length).toBe(2);
  });

  test("is a writable, non-enumerable, configurable own method", () => {
    const descriptor = Object.getOwnPropertyDescriptor(
      ShadowRealm.prototype,
      "importValue",
    );
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test("has Function.prototype as its [[Prototype]]", () => {
    expect(Object.getPrototypeOf(ShadowRealm.prototype.importValue)).toBe(
      Function.prototype,
    );
  });

  test("is not a constructor", () => {
    const realm = new ShadowRealm();
    expect(() => new ShadowRealm.prototype.importValue("", "x")).toThrow(
      TypeError,
    );
    expect(() => new realm.importValue("./helpers/import-value.js", "x")).toThrow(
      TypeError,
    );
  });

  test("throws a TypeError on an incompatible receiver", () => {
    expect(() =>
      ShadowRealm.prototype.importValue.call({}, "specifier", "name"),
    ).toThrow(TypeError);
  });

  test("throws a TypeError when the export name is not a string, without coercing it", () => {
    const realm = new ShadowRealm();
    let coerced = false;
    const exportName = {
      toString() {
        coerced = true;
        return "x";
      },
    };
    expect(() =>
      realm.importValue("./helpers/import-value.js", exportName),
    ).toThrow(TypeError);
    expect(coerced).toBe(false);
  });

  test("coerces the specifier with ToString and surfaces a throwing coercion synchronously", () => {
    const realm = new ShadowRealm();
    let calls = 0;
    const specifier = {
      toString() {
        calls += 1;
        throw new RangeError("from toString");
      },
    };
    expect(() => realm.importValue(specifier, "x")).toThrow(RangeError);
    expect(calls).toBe(1);
  });

  test("returns a promise that belongs to the caller realm", () => {
    const realm = new ShadowRealm();
    const result = realm.importValue("./helpers/import-value.js", "x");
    expect(result instanceof Promise).toBe(true);
  });

  test("resolves with a named export value", async () => {
    const realm = new ShadowRealm();
    expect(await realm.importValue("./helpers/import-value.js", "x")).toBe(1);
  });

  test("resolves with the default export", async () => {
    const realm = new ShadowRealm();
    expect(await realm.importValue("./helpers/import-value.js", "default")).toBe(
      42,
    );
  });

  test("rejects with a TypeError when the export does not exist", async () => {
    const realm = new ShadowRealm();
    await expect(
      realm.importValue("./helpers/import-value.js", "missing"),
    ).rejects.toThrow(TypeError);
  });

  test("rejects with a TypeError when the module cannot be resolved", async () => {
    const realm = new ShadowRealm();
    await expect(
      realm.importValue("./helpers/does-not-exist.js", "x"),
    ).rejects.toThrow(TypeError);
  });

  test("does not leak imported bindings into the caller realm", async () => {
    const realm = new ShadowRealm();
    await realm.importValue("./helpers/import-value.js", "x");
    expect(typeof globalThis.x).toBe("undefined");
  });

  test("imports into the child realm, isolated across separate ShadowRealms", async () => {
    const a = new ShadowRealm();
    const b = new ShadowRealm();
    expect(await a.importValue("./helpers/import-value.js", "x")).toBe(1);
    expect(await b.importValue("./helpers/import-value.js", "x")).toBe(1);
  });
});
