/*---
description: Blob.prototype.type (File API §3.2)
features: [Blob]
---*/

describe("Blob.prototype.type", () => {
  test("is the normalised type given to the constructor", () => {
    expect(new Blob().type).toBe("");
    expect(new Blob([], { type: "Image/PNG" }).type).toBe("image/png");
  });

  test("is a configurable, enumerable accessor without a setter", () => {
    const desc = Object.getOwnPropertyDescriptor(Blob.prototype, "type");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("throws on a receiver that is not a Blob", () => {
    expect(() => Blob.prototype.type).toThrow(TypeError);
  });

  test("cannot be assigned", () => {
    const b = new Blob([], { type: "x/y" });
    expect(() => {
      b.type = "a/b";
    }).toThrow(TypeError);
    expect(b.type).toBe("x/y");
  });
});
