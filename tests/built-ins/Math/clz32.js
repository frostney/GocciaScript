/*---
description: Math.clz32
features: [Math.clz32]
---*/

describe("Math.clz32", () => {
  test("counts leading zero bits in an unsigned 32-bit integer", () => {
    expect(Math.clz32(1)).toBe(31);
    expect(Math.clz32(0x80000000)).toBe(0);
    expect(Math.clz32(0x0f000000)).toBe(4);
    expect(Math.clz32(-1)).toBe(0);
  });

  test("uses ToUint32 conversion", () => {
    expect(Math.clz32()).toBe(32);
    expect(Math.clz32(NaN)).toBe(32);
    expect(Math.clz32(Infinity)).toBe(32);
    expect(Math.clz32(4294967296)).toBe(32);
    expect(Math.clz32("1")).toBe(31);
  });
});
