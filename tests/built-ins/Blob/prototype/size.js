/*---
description: Blob.prototype.size (File API §3.2)
features: [Blob]
---*/

describe("Blob.prototype.size", () => {
  test("is the number of bytes", () => {
    expect(new Blob().size).toBe(0);
    expect(new Blob(["ab", new Uint8Array(3)]).size).toBe(5);
  });

  test("is a configurable, enumerable accessor without a setter", () => {
    const desc = Object.getOwnPropertyDescriptor(Blob.prototype, "size");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("throws on a receiver that is not a Blob", () => {
    expect(() => Blob.prototype.size).toThrow(TypeError);
    const get = Object.getOwnPropertyDescriptor(Blob.prototype, "size").get;
    expect(() => get.call({})).toThrow(TypeError);
  });

  test("cannot be assigned", () => {
    const b = new Blob(["ab"]);
    expect(() => {
      b.size = 5;
    }).toThrow(TypeError);
    expect(b.size).toBe(2);
  });
});
