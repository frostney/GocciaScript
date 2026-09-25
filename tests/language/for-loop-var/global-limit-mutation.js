/*---
description: counted-for reevaluates a global-backed var limit
features: [compat-traditional-for-loop, compat-var]
---*/

var countedForGlobalLimit = 3;
let countedForGlobalLimitCount = 0;
for (let i = 0; i < countedForGlobalLimit; i = i + 1) {
  countedForGlobalLimitCount += 1;
  globalThis.countedForGlobalLimit = 0;
}

test("counted-for reevaluates a global-backed var limit mutated via globalThis", () => {
  expect(countedForGlobalLimitCount).toBe(1);
});
