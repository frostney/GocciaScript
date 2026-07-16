/*---
description: yield in traditional for-loop update resumes mid-update
features: [compat-traditional-for-loop, generators, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

test("yield in update yields current value before increment", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 3; i = (yield i, i + 1)) {}
    },
  };
  const out = [];
  for (const v of obj.gen()) out.push(v);
  expect(out).toEqual([0, 1, 2]);
});

test("update resumes without re-running body", () => {
  let bodyCalls = 0;
  const obj = {
    *gen() {
      for (let i = 0; i < 3; i = (yield i, i + 1)) bodyCalls++;
    },
  };
  const g = obj.gen();
  g.next(); g.next(); g.next(); g.next();
  expect(bodyCalls).toBe(3);
});

test("update preserves header value across resumes", () => {
  const obj = {
    *gen() {
      for (let i = 0; i < 5; i = (yield i, i + 1)) {}
    },
  };
  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(g.next().value).toBe(1);
  expect(g.next().value).toBe(2);
  expect(g.next().value).toBe(3);
  expect(g.next().value).toBe(4);
  expect(g.next().done).toBe(true);
});

test.runIf(hasGoccia)("update resume roots the active update scope across explicit GC", () => {
  const obj = {
    *gen() {
      for (let i = 42; i < 43; i = (yield "update", Goccia.gc(), i + 1)) {}
    },
  };

  const g = obj.gen();
  expect(g.next().value).toBe("update");
  expect(g.next().done).toBe(true);
});
