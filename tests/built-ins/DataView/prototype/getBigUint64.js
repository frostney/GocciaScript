/*---
description: DataView.prototype.getBigUint64
features: [DataView, BigInt]
---*/

describe("DataView.prototype.getBigUint64", () => {
  test("reads unsigned BigInt values", () => {
    const view = new DataView(new ArrayBuffer(8));
    view.setBigUint64(0, 18446744073709551615n);
    expect(view.getBigUint64(0)).toBe(18446744073709551615n);
  });
});
