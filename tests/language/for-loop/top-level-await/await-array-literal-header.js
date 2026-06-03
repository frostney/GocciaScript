/*---
description: top-level await array literals parse in traditional for headers
features: [compat-traditional-for-loop, top-level-await]
---*/

let count = 0;

for (await []; await []; await []) {
  count++;
  break;
}

test("top-level await array literals parse in traditional for headers", () => {
  expect(count).toBe(1);
});
