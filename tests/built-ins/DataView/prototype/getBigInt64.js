/*---
description: DataView.prototype.getBigInt64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.getBigInt64", () => {
  test("reads signed BigInt values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setBigInt64(0, -1n);
    expect(view.getBigInt64(0)).toBe(-1n);
  });
});
