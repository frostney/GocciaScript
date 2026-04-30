/*---
description: >
  --source-type=module loads the entry script as a Module. Per
  ES2026 §16.2.1.6.4, a Module Environment Record's [[ThisValue]] is
  undefined, so top-level `this` resolves to undefined rather than
  globalThis. The directory-level goccia.json sets the source type so
  every file in this directory runs as a Module.
---*/

const topLevelThis = this;
const topLevelThisType = typeof this;

describe("source-type=module entry", () => {
  test("top-level this is undefined", () => {
    expect(topLevelThis).toBeUndefined();
  });

  test("top-level typeof this is 'undefined'", () => {
    expect(topLevelThisType).toBe("undefined");
  });

  test("top-level this is not globalThis", () => {
    expect(topLevelThis === globalThis).toBe(false);
  });
});
