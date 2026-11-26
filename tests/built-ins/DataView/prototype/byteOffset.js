/*---
description: DataView.prototype.byteOffset
features: [DataView]
---*/

describe("get DataView.prototype.byteOffset", () => {
  test("returns view byte offset", () => {
    const view = new DataView(new ArrayBuffer(8), 3, 4);
    expect(view.byteOffset).toBe(3);
  });

  test("can be shadowed by an own property", () => {
    const view = new DataView(new ArrayBuffer(8), 3, 4);
    Object.defineProperty(view, "byteOffset", { value: 88 });
    expect(view.byteOffset).toBe(88);
  });

  // ECMA-262 main 25.3.4.3 step 5: +0 once the view is out of bounds, a
  // detached buffer's included (ES2026 threw a TypeError; test262 follows
  // main).
  test("returns 0 for a detached buffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer, 1);
    buffer.transfer();
    expect(view.byteOffset).toBe(0);
  });

  test("throws TypeError when inherited from a DataView", () => {
    const view = new DataView(new ArrayBuffer(8));
    const child = Object.create(view);
    expect(() => child.byteOffset).toThrow(TypeError);
  });
});
