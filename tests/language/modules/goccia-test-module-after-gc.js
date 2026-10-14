/*---
description: >
  The goccia:test namespace survives a collection that runs before the module
  is first imported. The runner builds the testing helpers before the file
  runs, and the module hands out that same builtin object later.
features: [modules, runtime-modules]
---*/

describe("goccia:test imported after a collection", () => {
  test("the namespace still exposes the testing helpers", async () => {
    Goccia.gc();
    Array.from({ length: 200 }, (_, i) => ({ i, s: "x" + i, a: [i] }));
    Goccia.gc();

    const testing = await import("goccia:test");

    expect(typeof testing.expect).toBe("function");
    expect(typeof testing.describe).toBe("function");
    expect(typeof testing.test).toBe("function");
  });
});
