/*---
description: >
  The default runtime has no global eval. Hosts opt in through the engine's
  EnableEval option, which this runtime does not use.
features: [global-properties, eval]
---*/

describe("eval is absent by default", () => {
  test("typeof eval is 'undefined'", () => {
    expect(typeof eval).toBe("undefined");
  });

  test("globalThis has no eval property", () => {
    expect("eval" in globalThis).toBe(false);
    expect(Object.getOwnPropertyDescriptor(globalThis, "eval")).toBeUndefined();
  });
});
