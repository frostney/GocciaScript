/*---
description: yield in traditional for-loop init resumes mid-init
features: [compat-traditional-for-loop, generators]
---*/

test("yield in init suspends before first iteration", () => {
  const obj = {
    *gen() {
      for (let i = yield 'init'; i < 3; i++) yield i;
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe('init');
  expect(g.next(0).value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().done).toBe(true);
});

test("init receives value from second next() call", () => {
  const obj = {
    *gen() {
      for (let i = yield 'start'; i < 5; i++) yield i;
    },
  };
  const g = obj.gen();
  g.next();
  expect(g.next(3).value).toBe(3);
  expect(g.next().value).toBe(4);
  expect(g.next().done).toBe(true);
});

test("yield in multi-binding init", () => {
  const obj = {
    *gen() {
      for (let i = yield 'a', j = 100; i < 2; i++, j--) yield [i, j];
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe('a');
  expect(g.next(0).value).toEqual([0, 100]);
  expect(g.next().value).toEqual([1, 99]);
  expect(g.next().done).toBe(true);
});
