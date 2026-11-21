/*---
description: Blob.prototype[Symbol.toStringTag]
features: [Blob]
---*/

describe("Blob.prototype[Symbol.toStringTag]", () => {
  test("is Blob, on the prototype, not writable or enumerable", () => {
    const desc = Object.getOwnPropertyDescriptor(Blob.prototype, Symbol.toStringTag);
    expect(desc.value).toBe("Blob");
    expect(desc.writable).toBe(false);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(Object.prototype.toString.call(new Blob())).toBe("[object Blob]");
  });
});
