/*---
description: >
  A value an imported module's top level read through one of its bindings and
  then dropped is reclaimed by a collection while the module's body still
  runs. The module's bindings live in its environment record, so a read copies
  the value into a register of the module's frame; each top-level statement
  clears the registers it used once it ends. An imported module has no
  completion value (ES2026 §16.2.1.7.3.2 ExecuteModule), so a bare awaited
  expression statement keeps nothing either.
features: [modules, top-level-await, Goccia.gc]
---*/

const expectReleased = ({ SIZE, grown }) => {
  expect(grown).toBeLessThan(SIZE / 2);
};

describe("a value an imported module dropped at its top level", () => {
  test("is reclaimed after an await and a read of its binding", async () => {
    const module = await import("./helpers/released-read-after-await.js");
    expect(module.length).toBe(module.SIZE);
    expectReleased(module);
  });

  test("is reclaimed after a read of its binding without an await", async () => {
    const module = await import("./helpers/released-read-without-await.js");
    expect(module.length).toBe(module.SIZE);
    expectReleased(module);
  });

  test("is reclaimed after a destructuring declaration reads it", async () => {
    const module = await import("./helpers/released-destructuring-read.js");
    expect(module.length).toBe(module.SIZE);
    expectReleased(module);
  });

  test("is reclaimed after a catch clause hands it to a binding", async () => {
    const module = await import("./helpers/released-caught-value.js");
    expect(module.length).toBe(module.SIZE);
    expectReleased(module);
  });

  test("is reclaimed after a bare await statement", async () => {
    expectReleased(await import("./helpers/released-bare-await.js"));
  });
});
