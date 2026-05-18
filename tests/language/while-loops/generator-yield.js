/*---
description: Generator yield resumes correctly inside while and do...while loops
features: [compat-while-loops, generators]
---*/

test("yield in while body resumes without rechecking condition", () => {
  let checks = 0;
  const obj = {
    *gen() {
      let i = 0;
      while ((checks++, i < 3)) {
        yield i;
        i++;
      }
    },
  };

  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(checks).toBe(1);
  expect(g.next().value).toBe(1);
  expect(checks).toBe(2);
  expect(g.next().value).toBe(2);
  expect(checks).toBe(3);
  expect(g.next().done).toBe(true);
  expect(checks).toBe(4);
});

test("yield in do...while body resumes before condition", () => {
  let checks = 0;
  const obj = {
    *gen() {
      let i = 0;
      do {
        yield i;
        i++;
      } while ((checks++, i < 3));
    },
  };

  const g = obj.gen();
  expect(g.next().value).toBe(0);
  expect(checks).toBe(0);
  expect(g.next().value).toBe(1);
  expect(checks).toBe(1);
  expect(g.next().value).toBe(2);
  expect(checks).toBe(2);
  expect(g.next().done).toBe(true);
  expect(checks).toBe(3);
});

test("yield in while condition resumes the condition expression", () => {
  const obj = {
    *gen() {
      let i = 0;
      while ((yield "check " + i) && i < 2) {
        yield i;
        i++;
      }
      yield "done";
    },
  };

  const g = obj.gen();
  expect(g.next().value).toBe("check 0");
  expect(g.next(true).value).toBe(0);
  expect(g.next().value).toBe("check 1");
  expect(g.next(true).value).toBe(1);
  expect(g.next().value).toBe("check 2");
  expect(g.next(false).value).toBe("done");
  expect(g.next().done).toBe(true);
});
