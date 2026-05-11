/*---
description: yield in traditional for-loop condition resumes mid-test
features: [compat-traditional-for-loop, generators]
---*/

test("yield in condition emits one value per iteration", () => {
  const obj = {
    *gen() {
      for (let i = 0; (yield i, i < 3); i++) {}
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual([0, 1, 2, 3]);
});

test("condition resumes without re-running init", () => {
  let initCalls = 0;
  const obj = {
    *gen() {
      for (let i = ((initCalls++), 0); (yield i, i < 2); i++) {}
    },
  };
  const g = obj.gen();
  g.next(); g.next(); g.next(); g.next();
  expect(initCalls).toBe(1);
});

test("body side effects accumulate across condition-yield resumes", () => {
  const obj = {
    *gen() {
      const seen = [];
      for (let i = 0; (yield i, i < 3); i++) seen.push(i);
      return seen;
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().value).toBe(3);
  expect(g.next()).toEqual({ value: [0, 1, 2], done: true });
});
