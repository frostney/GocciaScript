/*---
description: DataView.prototype.setBigInt64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.setBigInt64", () => {
  test("writes signed BigInt values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setBigInt64(0, -2n);
    expect(view.getBigInt64(0)).toBe(-2n);
  });

  test("converts value before range validation", () => {
    const view = new DataView(new ArrayBuffer(1));
    expect(() => view.setBigInt64(1, 1)).toThrow(TypeError);
  });
});
