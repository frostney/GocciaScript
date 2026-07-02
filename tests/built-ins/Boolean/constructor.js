describe("Boolean constructor", () => {
  test("new Boolean() creates wrapper object", () => {
    const b = new Boolean(true);
    expect(typeof b).toBe("object");
    expect(b.valueOf()).toBe(true);
  });

  test("Boolean wrapper prototypes are wired to the realm intrinsics", () => {
    expect(Object.getPrototypeOf(Boolean.prototype)).toBe(Object.prototype);
    expect(Object.getPrototypeOf(new Boolean(true))).toBe(Boolean.prototype);
  });

  test("boxed Boolean respects an explicit null prototype", () => {
    const boxed = Object(true);

    Object.setPrototypeOf(boxed, null);

    expect(Object.getPrototypeOf(boxed)).toBeNull();
    expect(boxed.valueOf).toBeUndefined();
    expect(boxed.toString).toBeUndefined();
  });

  test("newTarget fallback uses intrinsic Boolean.prototype, not mutable global Boolean", () => {
    const OriginalBoolean = Boolean;
    const originalPrototype = Boolean.prototype;
    const fakePrototype = {};

    const NewTarget = Object.defineProperty((class Target {}).bind(null), "prototype", {
      value: null,
    });

    globalThis.Boolean = { prototype: fakePrototype };
    try {
      const boxed = Reflect.construct(OriginalBoolean, [true], NewTarget);
      expect(Object.getPrototypeOf(boxed)).toBe(originalPrototype);
    } finally {
      globalThis.Boolean = OriginalBoolean;
    }
  });

  test("new Boolean(false).valueOf() is false", () => {
    const b = new Boolean(false);
    expect(b.valueOf()).toBe(false);
  });

  test("Boolean() as function returns primitive", () => {
    const b = Boolean(1);
    expect(typeof b).toBe("boolean");
    expect(b).toBe(true);
  });

  test("new Boolean() instanceof Boolean", () => {
    const b = new Boolean(true);
    expect(b instanceof Boolean).toBe(true);
  });
});
