describe("a throw crossing the ShadowRealm boundary", () => {
  test("runs none of the thrown error's name or message getters in the realm", () => {
    const realm = new ShadowRealm();
    realm.evaluate(`
      globalThis.log = 0;
      globalThis.thrower = () => {
        const e = new Error("x");
        Object.defineProperty(e, "message", { get() { globalThis.log = globalThis.log + 1; return "m"; } });
        Object.defineProperty(e, "name", { get() { globalThis.log = globalThis.log + 1; return "N"; } });
        throw e;
      };
    `);
    expect(() => realm.evaluate("thrower()")).toThrow(TypeError);
    expect(realm.evaluate("globalThis.log")).toBe(0);
  });

  test("a wrapped function's throw runs none of them either", () => {
    const realm = new ShadowRealm();
    const wrapped = realm.evaluate(`
      globalThis.log = 0;
      () => {
        const e = new Error("x");
        Object.defineProperty(e, "message", { get() { globalThis.log = globalThis.log + 1; return "m"; } });
        throw e;
      }
    `);
    expect(() => wrapped()).toThrow(TypeError);
    expect(realm.evaluate("globalThis.log")).toBe(0);
  });
});
