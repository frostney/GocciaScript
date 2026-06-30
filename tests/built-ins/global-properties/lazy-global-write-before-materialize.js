/*---
description: >
  Writing a lazy runtime global before it is ever read replaces the lazy
  descriptor with the written value; a later read must return that value rather
  than clobbering it by materializing the backing object. This file touches YAML
  only via the write below, so the write genuinely precedes materialization
  (each test file runs in a fresh engine).
features: [global-properties, globalThis]
---*/

// First and only touch of YAML in this file: assign before any read.
globalThis.YAML = 123;

describe("writing a lazy global before first read", () => {
  test("the written value survives and the lazy backing never materializes over it", () => {
    expect(globalThis.YAML).toBe(123);
    expect(YAML).toBe(123);
    const desc = Object.getOwnPropertyDescriptor(globalThis, "YAML");
    expect(desc.value).toBe(123);
    expect(typeof YAML.parse).toBe("undefined");
  });
});
