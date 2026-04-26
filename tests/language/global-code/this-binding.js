/*---
description: >
  At the top level of a script (non-module entry point), `this` resolves to
  the global object via §9.4.3 ResolveThisBinding ->
  GlobalEnvironmentRecord.GetThisBinding (§9.1.1.4.4), whose [[ThisValue]]
  is the global object set up in §9.1.2.5 NewGlobalEnvironment.
---*/

test("script-level this is globalThis", () => {
  expect(this).toBe(globalThis);
});

test("script-level this is an object", () => {
  expect(typeof this).toBe("object");
  expect(this === undefined).toBe(false);
});

test("script-level this exposes globals", () => {
  expect(this.globalThis).toBe(globalThis);
});
