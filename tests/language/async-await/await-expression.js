/*---
description: Await expression pauses and resumes async functions
features: [async-await]
---*/

describe("await expression", () => {
  test("await resolves a Promise", () => {
    const fn = async () => {
      const result = await Promise.resolve(42);
      return result;
    };
    return fn().then((v) => {
      expect(v).toBe(42);
    });
  });

  test("await with non-Promise passes through value", () => {
    const fn = async () => {
      const result = await 42;
      return result;
    };
    return fn().then((v) => {
      expect(v).toBe(42);
    });
  });

  test("multiple awaits in sequence", () => {
    const fn = async () => {
      const a = await Promise.resolve(1);
      const b = await Promise.resolve(2);
      const c = await Promise.resolve(3);
      return a + b + c;
    };
    return fn().then((v) => {
      expect(v).toBe(6);
    });
  });

  test("await with chained Promises", () => {
    const fn = async () => {
      const result = await Promise.resolve(10).then((v) => v * 2);
      return result;
    };
    return fn().then((v) => {
      expect(v).toBe(20);
    });
  });

  test("await preserves order of operations", () => {
    const log = [];
    const fn = async () => {
      log.push("before");
      const val = await Promise.resolve("middle");
      log.push(val);
      log.push("after");
      return log;
    };
    return fn().then((result) => {
      expect(result).toEqual(["before", "middle", "after"]);
    });
  });
});
