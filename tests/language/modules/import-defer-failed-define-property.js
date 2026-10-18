import defer * as ns from "./helpers/deferred-throws.js";

describe("import defer namespace of a module that failed", () => {
  test("defining through a proxy rethrows the evaluation error every time", () => {
    // The deferred module evaluates, and fails, on the first define.
    globalThis.__gocciaArmDeferredThrow = true;
    const proxy = new Proxy(ns, {});
    const messages = [0, 1, 2].map((i) => {
      try {
        Reflect.defineProperty(proxy, "x", { value: i });
        return "no error";
      } catch (e) {
        return e.message;
      }
    });

    expect(messages).toEqual([
      "deferred module failed",
      "deferred module failed",
      "deferred module failed",
    ]);
  });
});
