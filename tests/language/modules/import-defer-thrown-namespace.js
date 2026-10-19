import defer * as ns from "./helpers/deferred-throws.js";

describe("a deferred module namespace thrown through native code", () => {
  test("is caught unchanged and does not evaluate its module on the way", () => {
    // Evaluating the module would throw its own error in place of the value.
    globalThis.__gocciaArmDeferredThrow = true;
    let caught;
    try {
      [1].forEach(() => { throw ns; });
    } catch (e) {
      caught = e;
    }
    expect(caught).toBe(ns);
  });
});
