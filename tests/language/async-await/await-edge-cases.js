/*---
description: >
  Edge cases for await expressions per ES2026 §27.7.5.3 and test262
  test/language/expressions/await/
features: [async-await]
---*/

describe("await with primitive values (pass-through)", () => {
  test("await undefined returns undefined", async () => {
    const result = await undefined;
    expect(result).toBeUndefined();
  });

  test("await null returns null", async () => {
    const result = await null;
    expect(result).toBeNull();
  });

  test("await number returns number", async () => {
    expect(await 42).toBe(42);
    expect(await 0).toBe(0);
    expect(await -1).toBe(-1);
    expect(await NaN).toBeNaN();
    expect(await Infinity).toBe(Infinity);
  });

  test("await string returns string", async () => {
    expect(await "hello").toBe("hello");
    expect(await "").toBe("");
  });

  test("await boolean returns boolean", async () => {
    expect(await true).toBe(true);
    expect(await false).toBe(false);
  });
});

describe("await with objects (non-Promise, non-thenable)", () => {
  test("await plain object returns the object", async () => {
    const obj = { x: 1 };
    const result = await obj;
    expect(result).toBe(obj);
  });

  test("await array returns the array", async () => {
    const arr = [1, 2, 3];
    const result = await arr;
    expect(result).toBe(arr);
  });
});

describe("await with already-settled Promises", () => {
  test("await already-resolved Promise extracts value", async () => {
    const p = Promise.resolve(42);
    expect(await p).toBe(42);
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

  test("await Promise.resolve(undefined) returns undefined", async () => {
    expect(await Promise.resolve(undefined)).toBeUndefined();
  });

  test("await Promise.resolve(null) returns null", async () => {
    expect(await Promise.resolve(null)).toBeNull();
  });

  test("await Promise.resolve(0) returns 0", async () => {
    expect(await Promise.resolve(0)).toBe(0);
  });

  test("await Promise.resolve(false) returns false", async () => {
    expect(await Promise.resolve(false)).toBe(false);
  });

  test("await Promise.resolve('') returns empty string", async () => {
    expect(await Promise.resolve("")).toBe("");
  });
});

describe("async function return value semantics", () => {
  test("async function with no return resolves to undefined", async () => {
    const fn = async () => {};
    const result = await fn();
    expect(result).toBeUndefined();
  });

  test("async function returning a value resolves to that value", async () => {
    const fn = async () => 42;
    expect(await fn()).toBe(42);
  });

  test("async function returning a resolved Promise unwraps one level", async () => {
    const fn = async () => Promise.resolve(42);
    const result = await fn();
    expect(result).toBe(42);
  });

  test("async function returning a rejected Promise propagates rejection", async () => {
    const fn = async () => Promise.reject("async rejection");
    try {
      await fn();
    } catch (e) {
      expect(e).toBe("async rejection");
      return;
    }
    expect(true).toBe(false);
  });
});

describe("async try/finally interactions", () => {
  test("finally runs after successful await", async () => {
    let finallyRan = false;
    const fn = async () => {
      try {
        return await Promise.resolve(42);
      } finally {
        finallyRan = true;
      }
    };
    const result = await fn();
    expect(result).toBe(42);
    expect(finallyRan).toBe(true);
  });

  test("finally runs after failed await", async () => {
    let finallyRan = false;
    const fn = async () => {
      try {
        await Promise.reject("error");
      } catch (e) {
        return e;
      } finally {
        finallyRan = true;
      }
    };
    const result = await fn();
    expect(result).toBe("error");
    expect(finallyRan).toBe(true);
  });

  test("finally return overrides try return", async () => {
    const fn = async () => {
      try {
        return "try";
      } finally {
        return "finally";
      }
    };
    expect(await fn()).toBe("finally");
  });

  test("finally return overrides catch return", async () => {
    const fn = async () => {
      try {
        throw "error";
      } catch (e) {
        return "catch";
      } finally {
        return "finally";
      }
    };
    expect(await fn()).toBe("finally");
  });
});

describe("await in expression positions", () => {
  test("await as operand of addition", async () => {
    const result = (await Promise.resolve(10)) + (await Promise.resolve(20));
    expect(result).toBe(30);
  });

  test("await as operand of comparison", async () => {
    const result = (await Promise.resolve(5)) > (await Promise.resolve(3));
    expect(result).toBe(true);
  });

  test("await in ternary condition", async () => {
    const result = (await Promise.resolve(true)) ? "yes" : "no";
    expect(result).toBe("yes");
  });

  test("awaited value in template literal", async () => {
    const val = await Promise.resolve(42);
    const result = `value: ${val}`;
    expect(result).toBe("value: 42");
  });

  test("await as argument to function call", async () => {
    const identity = (x) => x;
    expect(identity(await Promise.resolve(42))).toBe(42);
  });

  test("await in array literal", async () => {
    const arr = [await Promise.resolve(1), await Promise.resolve(2), await Promise.resolve(3)];
    expect(arr).toEqual([1, 2, 3]);
  });

  test("await in object literal value", async () => {
    const obj = { a: await Promise.resolve(1), b: await Promise.resolve(2) };
    expect(obj.a).toBe(1);
    expect(obj.b).toBe(2);
  });
});

describe("nested async functions", () => {
  test("inner async function can await independently", async () => {
    const outer = async () => {
      const inner = async () => await Promise.resolve("inner");
      const innerResult = await inner();
      return `outer(${innerResult})`;
    };
    expect(await outer()).toBe("outer(inner)");
  });

  test("error in inner async function propagates through await", async () => {
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

describe("await with thenable objects", () => {
  test("await on thenable unwraps the value", async () => {
    // test262: await-awaits-thenables.js
    const thenable = {
      then(resolve) {
        resolve(42);
      }
    };
    expect(await thenable).toBe(42);
  });

  test("await on thenable with rejection", async () => {
    const thenable = {
      then(resolve, reject) {
        reject("thenable error");
      }
    };
    try {
      await thenable;
    } catch (e) {
      expect(e).toBe("thenable error");
      return;
    }
    expect(true).toBe(false);
  });

  test("await on object with non-callable then is not a thenable", async () => {
    // test262: await-awaits-thenable-not-callable.js
    const obj = { then: 42 };
    const result = await obj;
    expect(result).toBe(obj);
  });

  test("await on object without then is not a thenable", async () => {
    const obj = { value: 42 };
    const result = await obj;
    expect(result).toBe(obj);
  });

  test("thenable that resolves asynchronously via Promise", async () => {
    const thenable = {
      then(resolve) {
        Promise.resolve(99).then((v) => resolve(v));
      }
    };
    expect(await thenable).toBe(99);
  });
});
