/*---
description: yield in traditional for-loop with var init resumes without re-running init
features: [compat-traditional-for-loop, compat-var, generators]
---*/

test("yield in body advances var counter across resumes", () => {
  const obj = {
    *gen() {
      for (var i = 0; i < 3; i++) yield i;
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().done).toBe(true);
});

test("var init runs exactly once across resumes", () => {
  let initCalls = 0;
  const obj = {
    *gen() {
      for (var i = ((initCalls++), 0); i < 3; i++) yield i;
    },
  };
  const g = obj.gen();
  g.next(); g.next(); g.next(); g.next();
  expect(initCalls).toBe(1);
});

test("var binding is visible after generator loop completes", () => {
  const obj = {
    *gen() {
      for (var k = 5; k < 8; k++) yield k;
      yield 'final-k=' + k;
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(5);
  expect(g.next().value).toBe(6);
  expect(g.next().value).toBe(7);
  expect(g.next().value).toBe('final-k=8');
});

test("yield in update with var init", () => {
  const obj = {
    *gen() {
      for (var i = 0; i < 3; i = (yield i, i + 1)) {}
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual([0, 1, 2]);
});
