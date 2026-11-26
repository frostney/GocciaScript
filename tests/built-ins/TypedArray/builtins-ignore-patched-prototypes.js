/*---
description: Built-ins written in the builtins5 prelude do not call an Array.prototype.push a script replaced
features: [Uint8Array, FinalizationRegistry, Math.sumPrecise]
---*/

test("Uint8Array.fromBase64 and fromHex with Array.prototype.push replaced", () => {
  const push = Array.prototype.push;
  Array.prototype.push = () => { throw new Error("push called"); };
  try {
    expect(Array.from(Uint8Array.fromBase64("AQID"))).toEqual([1, 2, 3]);
    expect(Array.from(Uint8Array.fromHex("0a0b"))).toEqual([10, 11]);
  } finally {
    Array.prototype.push = push;
  }
});

test("Math.sumPrecise and FinalizationRegistry with Array.prototype.push replaced", () => {
  const push = Array.prototype.push;
  Array.prototype.push = () => { throw new Error("push called"); };
  try {
    expect(Math.sumPrecise([1e20, 0.1, -1e20])).toBe(0.1);
    const registry = new FinalizationRegistry(() => {});
    registry.register({}, "held");
    expect(registry.unregister({})).toBe(false);
  } finally {
    Array.prototype.push = push;
  }
});
