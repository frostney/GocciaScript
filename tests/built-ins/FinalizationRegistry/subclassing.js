/*---
description: FinalizationRegistry supports subclass construction
features: [FinalizationRegistry, class]
---*/

test("FinalizationRegistry can be subclassed", () => {
  class ChildFinalizationRegistry extends FinalizationRegistry {}
  const registry = new ChildFinalizationRegistry(() => {});
  expect(registry instanceof ChildFinalizationRegistry).toBe(true);
  expect(registry instanceof FinalizationRegistry).toBe(true);
});

test("FinalizationRegistry subclass can add instance fields", () => {
  class ChildFinalizationRegistry extends FinalizationRegistry {
    constructor(cleanup) {
      super(cleanup);
      this.name = "child";
    }
  }

  const registry = new ChildFinalizationRegistry(() => {});
  expect(registry.name).toBe("child");
});
