/*---
description: BigInt bitwise operations
features: [bigint]
---*/

test("bitwise AND", () => {
  expect(0b1100n & 0b1010n).toBe(0b1000n);
});

test("bitwise OR", () => {
  expect(0b1100n | 0b1010n).toBe(0b1110n);
});

test("bitwise XOR", () => {
  expect(0b1100n ^ 0b1010n).toBe(0b0110n);
});

test("bitwise NOT", () => {
  expect(~0n).toBe(-1n);
  expect(~(-1n)).toBe(0n);
  expect(~42n).toBe(-43n);
});

test("left shift", () => {
  expect(1n << 3n).toBe(8n);
  expect(1n << 64n).toBe(18446744073709551616n);
});

test("right shift (arithmetic)", () => {
  expect(8n >> 2n).toBe(2n);
  expect(-1n >> 1n).toBe(-1n);
});
