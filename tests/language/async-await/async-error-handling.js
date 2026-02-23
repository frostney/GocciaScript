/*---
description: Error handling in async functions with try/catch
features: [async-await]
---*/

describe("async error handling", () => {
  test("await rejected Promise throws in async function", () => {
    const fn = async () => {
      try {
        await Promise.reject("fail");
        return "should not reach";
      } catch (e) {
        return e;
      }
    };
    return fn().then((v) => {
      expect(v).toBe("fail");
    });
  });

  test("thrown error in async function rejects the Promise", () => {
    const fn = async () => {
      throw new Error("async error");
    };
    return fn().catch((e) => {
      expect(e instanceof Error).toBe(true);
      expect(e.message).toBe("async error");
    });
  });

  test("try/catch in async function catches synchronous errors", () => {
    const fn = async () => {
      try {
        const obj = null;
        obj.property;
      } catch (e) {
        return "caught";
      }
    };
    return fn().then((v) => {
      expect(v).toBe("caught");
    });
  });

  test("error after await rejects the Promise", () => {
    const fn = async () => {
      await Promise.resolve();
      throw new Error("after await");
    };
    return fn().catch((e) => {
      expect(e.message).toBe("after await");
    });
  });

  test("nested try/catch with await", () => {
    const fn = async () => {
      try {
        try {
          await Promise.reject("inner");
        } catch (e) {
          return "caught: " + e;
        }
      } catch (e) {
        return "outer: " + e;
      }
    };
    return fn().then((v) => {
      expect(v).toBe("caught: inner");
    });
  });
});
