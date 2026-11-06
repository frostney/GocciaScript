/*---
description: Ambiguous await falls back to Script after strict Module parsing fails
features: [compat-non-strict-mode, async-await]
---*/
globalThis.await = {0: 1};
await[0];
delete missing;
const indexed = await[0];
const topLevelThis = this;
test("sloppy Script survives a failed strict Module reading", () => {
  expect(indexed).toBe(1);
  expect(topLevelThis).toBe(globalThis);
  expect(globalThis.await[0]).toBe(1);
});
