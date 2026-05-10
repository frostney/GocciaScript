/*---
description: break/continue/return in traditional for-loop generators clear suspended state
features: [compat-traditional-for-loop, generators]
---*/

test("break inside body exits and does not resume the loop", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 10; i++) {
        if (i === 2) break;
        yield i;
      }
      yield 'after';
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe('after');
  expect(g.next().done).toBe(true);
});

test("continue runs update and skips remainder", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 5; i++) {
        if (i === 2) continue;
        yield i;
      }
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual([0, 1, 3, 4]);
});

test("return from body short-circuits the generator", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 10; i++) {
        if (i === 2) return 'done-' + i;
        yield i;
      }
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next()).toEqual({ value: 'done-2', done: true });
});

test("nested for loops each track their own iteration state", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 2; i++)
        for (let j = 0; j < 2; j++)
          yield [i, j];
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual([[0, 0], [0, 1], [1, 0], [1, 1]]);
});

test("zero-iteration loop with yield after", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 0; i++) yield i;
      yield 'after';
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe('after');
  expect(g.next().done).toBe(true);
});

test("generator return() in mid-loop clears for-loop state", () => {
  const obj = {
    *gen() {
      try {
        for (let i = 0; i < 10; i++) yield i;
      } finally {
        yield 'cleanup';
      }
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.return('early').value).toBe('cleanup');
  expect(g.next()).toEqual({ value: 'early', done: true });
});

test("yield* delegates from inside for-loop body", () => {
  const obj = {
    *inner() { yield 'a'; yield 'b'; },
    *gen() {
      for (let i = 0; i < 2; i++) yield* obj.inner();
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual(['a', 'b', 'a', 'b']);
});
