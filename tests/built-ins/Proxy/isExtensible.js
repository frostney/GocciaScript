describe("Proxy isExtensible trap", () => {
  test("intercepts Object.isExtensible with matching result", () => {
    const proxy = new Proxy(
      {},
      {
        isExtensible: () => true,
      }
    );
    expect(Object.isExtensible(proxy)).toBe(true);
  });

  test("throws TypeError when trap result mismatches target", () => {
    // Target is extensible but trap returns false — invariant violation
    const proxy = new Proxy(
      {},
      {
        isExtensible: () => false,
      }
    );
    expect(() => Object.isExtensible(proxy)).toThrow(TypeError);
  });

  test("trap can return false when target is non-extensible", () => {
    const target = {};
    Object.preventExtensions(target);
    const proxy = new Proxy(target, {
      isExtensible: () => false,
    });
    expect(Object.isExtensible(proxy)).toBe(false);
  });

  test("falls back to target when no trap", () => {
    const target = {};
    const proxy = new Proxy(target, {});
    expect(Object.isExtensible(proxy)).toBe(true);
    Object.preventExtensions(target);
    expect(Object.isExtensible(proxy)).toBe(false);
  });

  test("receives correct target", () => {
    const target = {};
    let receivedTarget;
    const proxy = new Proxy(target, {
      isExtensible: (t) => {
        receivedTarget = t;
        return Object.isExtensible(t);
      },
    });
    Object.isExtensible(proxy);
    expect(receivedTarget).toBe(target);
  });
});
