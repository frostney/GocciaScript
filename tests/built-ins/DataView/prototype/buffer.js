/*---
description: DataView.prototype.buffer
features: [DataView]
---*/

describe("get DataView.prototype.buffer", () => {
  test("returns the viewed buffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer, 2, 4);
    expect(view.buffer).toBe(buffer);
  });

  test("returns a detached viewed buffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    buffer.transfer();
    expect(view.buffer).toBe(buffer);
  });

  test("can be shadowed by an own property", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    Object.defineProperty(view, "buffer", { value: "shadow" });
    expect(view.buffer).toBe("shadow");
  });

  test("throws TypeError when called on non-DataView", () => {
    const getter = Object.getOwnPropertyDescriptor(DataView.prototype, "buffer").get;
    expect(() => getter.call({})).toThrow(TypeError);
  });

  test("throws TypeError when inherited from a DataView", () => {
    const view = new DataView(new ArrayBuffer(8));
    const child = Object.create(view);
    expect(() => child.buffer).toThrow(TypeError);
  });
});
