/*---
description: While loops work correctly including do-while variant
features: [while-statement, do-while-statement]
---*/

test.skip("while loop with false condition", () => {
  let executed = false;
  while (false) {
    executed = true;
  }
  expect(executed).toBeFalsy();
});

test.skip("do-while loop", () => {
  let i = 0;
  let count = 0;
  do {
    count++;
    i++;
  } while (i < 3);

  expect(count).toBe(3);
  expect(i).toBe(3);
});

test.skip("do-while executes at least once", () => {
  let executed = false;
  do {
    executed = true;
  } while (false);

  expect(executed).toBeTruthy();
});
