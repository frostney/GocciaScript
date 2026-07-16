describe("ShadowRealm constructor", () => {
  test("is a function with name 'ShadowRealm' and length 0", () => {
    expect(typeof ShadowRealm).toBe("function");
    expect(ShadowRealm.name).toBe("ShadowRealm");
    expect(ShadowRealm.length).toBe(0);
  });

  test("new ShadowRealm() returns an object whose prototype is ShadowRealm.prototype", () => {
    const realm = new ShadowRealm();
    expect(typeof realm).toBe("object");
    expect(Object.getPrototypeOf(realm)).toBe(ShadowRealm.prototype);
    expect(realm instanceof ShadowRealm).toBe(true);
  });

  test("each new ShadowRealm() instance is a distinct object", () => {
    expect(new ShadowRealm() === new ShadowRealm()).toBe(false);
  });

  test("calling ShadowRealm without new throws a TypeError", () => {
    expect(() => ShadowRealm()).toThrow(TypeError);
  });

  test("a new instance is extensible", () => {
    expect(Object.isExtensible(new ShadowRealm())).toBe(true);
  });

  test("the constructor is extensible", () => {
    expect(Object.isExtensible(ShadowRealm)).toBe(true);
  });

  test("ShadowRealm.prototype is a non-writable, non-enumerable, non-configurable own property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(ShadowRealm, "prototype");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
  });

  test("Object.prototype.toString tags a ShadowRealm instance as [object ShadowRealm]", () => {
    expect(Object.prototype.toString.call(new ShadowRealm())).toBe(
      "[object ShadowRealm]",
    );
  });

  test("globalThis.ShadowRealm is a writable, non-enumerable, configurable own property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, "ShadowRealm");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });
});
