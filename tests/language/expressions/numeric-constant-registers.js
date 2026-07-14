/*---
description: Bytecode numeric constants preserve Number values when loaded into scalar registers
---*/

const loadNumericConstants = () => [
  0,
  1,
  2_147_483_648,
  -2_147_483_649,
  9_007_199_254_740_991,
  9_007_199_254_740_993,
  0.125,
  1e309,
];

test("numeric constants survive repeated scalar register loads", () => {
  const first = loadNumericConstants();
  const second = loadNumericConstants();

  expect(first).toEqual(second);
  expect(first[2]).toBe(2_147_483_648);
  expect(first[3]).toBe(-2_147_483_649);
  expect(first[4]).toBe(9_007_199_254_740_991);
  expect(first[5]).toBe(9_007_199_254_740_992);
  expect(first[6]).toBe(0.125);
  expect(first[7]).toBe(Infinity);
});
