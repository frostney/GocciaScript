/*---
description: A while loop whose condition is a method call stops when the call returns a falsy value
features: [compat-while-loops, optional-chaining]
---*/

test("a while loop on a method call or an optional call stops on a falsy result", () => {
  let i = 0;
  const it = { next() { return i++ < 3 ? {} : null; } };
  let n = 0;
  while (it.next()) {
    n++;
    if (n > 10) break;
  }
  expect(n).toBe(3);
  let j = 0;
  const it2 = { next() { return j++ < 2 ? 1 : 0; } };
  let m = 0;
  while (it2.next?.()) {
    m++;
    if (m > 10) break;
  }
  expect(m).toBe(2);
});
