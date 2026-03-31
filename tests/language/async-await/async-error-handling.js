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

  test("await already-rejected Promise throws", async () => {
    try {
      await Promise.reject("fail");
    } catch (e) {
      expect(e).toBe("fail");
      return;
    }

    expect(true).toBe(false);
  });

  test("await thenable rejection throws", async () => {
    const thenable = {
      then(resolve, reject) {
        reject("thenable error");
      },
    };

    try {
      await thenable;
    } catch (e) {
      expect(e).toBe("thenable error");
      return;
    }

    expect(true).toBe(false);
  });

  test("finally runs after successful await", async () => {
    let successFinallyRan = false;
    const successFn = async () => {
      try {
        return await Promise.resolve(42);
      } finally {
        successFinallyRan = true;
      }
    };

    expect(await successFn()).toBe(42);
    expect(successFinallyRan).toBe(true);
  });

  test("finally runs after failed await", async () => {
    let failureFinallyRan = false;
    const failureFn = async () => {
      try {
        await Promise.reject("error");
      } catch (e) {
        return e;
      } finally {
        failureFinallyRan = true;
      }
    };

    expect(await failureFn()).toBe("error");
    expect(failureFinallyRan).toBe(true);
  });

  test("finally return overrides try return values", async () => {
    const fromTry = async () => {
      try {
        return "try";
      } finally {
        return "finally";
      }
    };

    expect(await fromTry()).toBe("finally");
  });

  test("finally return overrides catch return values", async () => {
    const fromCatch = async () => {
      try {
        throw "error";
      } catch (e) {
        return "catch";
      } finally {
        return "finally";
      }
    };

    expect(await fromCatch()).toBe("finally");
  });
});
