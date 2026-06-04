/*---
description: DataView @@toStringTag
features: [DataView]
---*/

describe("DataView @@toStringTag", () => {
  test("Object.prototype.toString reports DataView", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(Object.prototype.toString.call(view)).toBe("[object DataView]");
  });
});
