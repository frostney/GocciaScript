/*---
description: yield in traditional for-loop body preserves loop state across resumes
features: [compat-traditional-for-loop, generators]
---*/

const factory = {
  basic() {
    return ({
      *gen() {
        for (let i = 0; i < 3; i++) yield i;
      },
    }).gen();
  },
  doubled() {
    return ({
      *gen() {
        for (let i = 0; i < 5; i++) yield i * 2;
      },
    }).gen();
  },
  countdown() {
    return ({
      *gen() {
        for (let i = 3; i > 0; i--) yield i;
      },
    }).gen();
  },
  withSeen() {
    return ({
      *gen() {
        const seen = [];
        for (let i = 0; i < 3; i++) {
          seen.push(i);
          yield seen.length;
        }
        return seen;
      },
    }).gen();
  },
  commaInit() {
    return ({
      *gen() {
        for (let i = 0, j = 10; i < 3; i++, j--) yield [i, j];
      },
    }).gen();
  },
  destructInit() {
    return ({
      *gen() {
        for (let [i, j] = [0, 10]; i < 3; i++, j--) yield [i, j];
      },
    }).gen();
  },
  constInit() {
    return ({
      *gen() {
        for (const arr = [10, 20, 30], len = arr.length; arr[0] < 100; ) {
          yield arr.shift();
        }
      },
    }).gen();
  },
};

test("yield in body advances loop counter across resumes", () => {
  const g = factory.basic();
  expect(g.next()).toEqual({ value: 0, done: false });
  expect(g.next()).toEqual({ value: 1, done: false });
  expect(g.next()).toEqual({ value: 2, done: false });
  expect(g.next()).toEqual({ value: undefined, done: true });
});

test("yield in body collects values across multiple resumes", () => {
  const out = [];
  for (const v of factory.doubled()) out.push(v);
  expect(out).toEqual([0, 2, 4, 6, 8]);
});

test("init side effects run exactly once", () => {
  let initCalls = 0;
  const obj = {
    *gen() {
      for (let i = ((initCalls++), 0); i < 3; i++) yield i;
    },
  };
  const g = obj.gen();
  g.next(); g.next(); g.next(); g.next();
  expect(initCalls).toBe(1);
});

test("yield in body with countdown", () => {
  const g = factory.countdown();
  expect(g.next().value).toBe(3);
  expect(g.next().value).toBe(2);
  expect(g.next().value).toBe(1);
  expect(g.next().done).toBe(true);
});

test("body with multiple statements before yield", () => {
  const g = factory.withSeen();
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().value).toBe(3);
  expect(g.next()).toEqual({ value: [0, 1, 2], done: true });
});

test("comma-separated init bindings advance per iteration", () => {
  const g = factory.commaInit();
  expect(g.next().value).toEqual([0, 10]);
  expect(g.next().value).toEqual([1, 9]);
  expect(g.next().value).toEqual([2, 8]);
  expect(g.next().done).toBe(true);
});

test("destructuring init advances per iteration", () => {
  const g = factory.destructInit();
  expect(g.next().value).toEqual([0, 10]);
  expect(g.next().value).toEqual([1, 9]);
  expect(g.next().value).toEqual([2, 8]);
  expect(g.next().done).toBe(true);
});

test("const binding in init yields per iteration", () => {
  const g = factory.constInit();
  expect(g.next().value).toBe(10);
  expect(g.next().value).toBe(20);
  expect(g.next().value).toBe(30);
  expect(g.next().done).toBe(true);
});
