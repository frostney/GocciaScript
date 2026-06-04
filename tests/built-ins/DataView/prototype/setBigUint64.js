/*---
description: DataView.prototype.setBigUint64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.setBigUint64", () => {
  test("writes unsigned BigInt values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setBigUint64(0, 18446744073709551615n);
    expect(view.getBigUint64(0)).toBe(18446744073709551615n);
  });
});
