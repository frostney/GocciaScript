/*---
description: DataView.prototype.byteLength
features: [DataView]
---*/

describe("get DataView.prototype.byteLength", () => {
  test("returns fixed view byte length", () => {
    const view = new DataView(new ArrayBuffer(8), 2, 4);
    expect(view.byteLength).toBe(4);
  });

  test("auto-length view updates after resize", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 16 });
    const view = new DataView(buffer, 1);
    buffer.resize(10);
    expect(view.byteLength).toBe(9);
  });

  test("can be shadowed by an own property", () => {
    const view = new DataView(new ArrayBuffer(8), 2, 4);
    Object.defineProperty(view, "byteLength", { value: 99 });
    expect(view.byteLength).toBe(99);
  });

  test("throws TypeError for detached buffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    buffer.transfer();
    expect(() => view.byteLength).toThrow(TypeError);
  });

  test("throws TypeError for out-of-bounds fixed view after resize", () => {
    const buffer = new ArrayBuffer(8, { maxByteLength: 16 });
    const view = new DataView(buffer, 4, 4);
    buffer.resize(6);
    expect(() => view.byteLength).toThrow(TypeError);
  });

  test("throws TypeError when inherited from a DataView", () => {
    const view = new DataView(new ArrayBuffer(8));
    const child = Object.create(view);
    expect(() => child.byteLength).toThrow(TypeError);
  });
});
