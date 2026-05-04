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

  test("await undefined returns undefined", async () => {
    expect(await undefined).toBeUndefined();
  });

  test("await null returns null", async () => {
    expect(await null).toBeNull();
  });

  test("await numbers return numbers", async () => {
    expect(await 42).toBe(42);
    expect(await 0).toBe(0);
    expect(await -1).toBe(-1);
    expect(await NaN).toBeNaN();
    expect(await Infinity).toBe(Infinity);
  });

  test("await strings return strings", async () => {
    expect(await "hello").toBe("hello");
    expect(await "").toBe("");
  });

  test("await booleans return booleans", async () => {
    expect(await true).toBe(true);
    expect(await false).toBe(false);
  });

  test("await plain objects returns the original object", async () => {
    const obj = { x: 1 };
    expect(await obj).toBe(obj);
  });

  test("await arrays returns the original array", async () => {
    const arr = [1, 2, 3];
    expect(await arr).toBe(arr);
  });

  test("await synchronous thenables unwraps resolved values", async () => {
    const thenable = {
      then(resolve) {
        resolve(42);
      },
    };
    expect(await thenable).toBe(42);
  });

  test("await asynchronous thenables unwraps resolved values", async () => {
    const asyncThenable = {
      then(resolve) {
        Promise.resolve(99).then((value) => resolve(value));
      },
    };

    expect(await asyncThenable).toBe(99);
  });

  test("await treats non-callable then as a normal property", async () => {
    const obj = { then: 42 };

    expect(await obj).toBe(obj);
  });

  test("await works in addition expressions", async () => {
    const sum = (await Promise.resolve(10)) + (await Promise.resolve(20));
    expect(sum).toBe(30);
  });

  test("await works in comparison expressions", async () => {
    const comparison = (await Promise.resolve(5)) > (await Promise.resolve(3));
    expect(comparison).toBe(true);
  });

  test("await accepts prefix update operands", async () => {
    let value = 1;
    expect(await ++value).toBe(2);
    expect(value).toBe(2);
    expect(await --value).toBe(1);
    expect(value).toBe(1);
  });

  test("await works in ternary conditions", async () => {
    const ternary = (await Promise.resolve(true)) ? "yes" : "no";
    expect(ternary).toBe("yes");
  });

  test("await works in template literal interpolation", async () => {
    const awaitedValue = await Promise.resolve(42);
    const template = `value: ${awaitedValue}`;
    expect(template).toBe("value: 42");
  });

  test("await works as a function argument", async () => {
    const identity = (x) => x;
    expect(identity(await Promise.resolve(42))).toBe(42);
  });

  test("await works in array literals", async () => {
    const array = [await Promise.resolve(1), await Promise.resolve(2), await Promise.resolve(3)];
    expect(array).toEqual([1, 2, 3]);
  });

  test("await works in object literals", async () => {
    const object = {
      a: await Promise.resolve(1),
      b: await Promise.resolve(2),
    };
    expect(object).toEqual({ a: 1, b: 2 });
  });

  test("nested async functions await independently", async () => {
    const outer = async () => {
      const inner = async () => await Promise.resolve("inner");
      const innerResult = await inner();
      return `outer(${innerResult})`;
    };

    expect(await outer()).toBe("outer(inner)");
  });

  test("errors in nested async functions propagate through await", async () => {
    const inner = async () => {
      throw "inner error";
    };

    const outer = async () => {
      try {
        await inner();
      } catch (e) {
        return e;
      }
    };

    expect(await outer()).toBe("inner error");
  });
});
