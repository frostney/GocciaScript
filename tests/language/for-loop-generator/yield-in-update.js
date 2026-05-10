/*---
description: yield in traditional for-loop update resumes mid-update
features: [compat-traditional-for-loop, compat-function, generators]
---*/

test("yield in update yields current value before increment", () => {
  function* gen() {
    for (let i = 0; i < 3; i = (yield i, i + 1)) {}
  }
  const out = [];
  for (const v of gen()) out.push(v);
  expect(out).toEqual([0, 1, 2]);
});

test("update resumes without re-running body", () => {
  let bodyCalls = 0;
  function* gen() {
    for (let i = 0; i < 3; i = (yield i, i + 1)) bodyCalls++;
  }
  const g = gen();
  g.next(); g.next(); g.next(); g.next();
  expect(bodyCalls).toBe(3);
});

test("update preserves header value across resumes", () => {
  function* gen() {
    for (let i = 0; i < 5; i = (yield i, i + 1)) {}
  }
  const g = gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().value).toBe(3);
  expect(g.next().value).toBe(4);
  expect(g.next().done).toBe(true);
});
