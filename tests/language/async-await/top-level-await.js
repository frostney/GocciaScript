/*---
description: Top-level await (ES2022+) allows await outside async functions
features: [async-await, top-level-await]
---*/

// True top-level await: these run at module scope, not inside any function
const resolvedValue = await Promise.resolve(42);
const nonPromiseValue = await 99;
const chainedValue = await Promise.resolve(10).then((v) => v * 2);
const thenable = {
  then(resolve) {
    resolve("thenable-result");
  },
};
const thenableValue = await thenable;

const [allA, allB, allC] = await Promise.all([
  Promise.resolve(1),
  Promise.resolve(2),
  Promise.resolve(3),
]);

let topLevelRejection;
try {
  await Promise.reject("top-level rejection");
} catch (e) {
  topLevelRejection = e;
}

describe("top-level await", () => {
  test("await resolves a Promise at file scope", () => {
    expect(resolvedValue).toBe(42);
  });

  test("await passes through non-Promise values at file scope", () => {
    expect(nonPromiseValue).toBe(99);
  });

  test("await resolves chained Promises at file scope", () => {
    expect(chainedValue).toBe(20);
  });

  test("await unwraps thenables at file scope", () => {
    expect(thenableValue).toBe("thenable-result");
  });

  test("await works with Promise.all at file scope", () => {
    expect(allA).toBe(1);
    expect(allB).toBe(2);
    expect(allC).toBe(3);
  });

  test("await try-catch handles rejection at file scope", () => {
    expect(topLevelRejection).toBe("top-level rejection");
  });

  test("await undefined returns undefined", async () => {
    const x = await undefined;
    expect(x).toBeUndefined();
  });

  test("await null returns null", async () => {
    const x = await null;
    expect(x).toBeNull();
  });

  test("await string passes through", async () => {
    const x = await "hello";
    expect(x).toBe("hello");
  });

  test("await boolean passes through", async () => {
    const x = await true;
    expect(x).toBe(true);
  });

  test("multiple sequential top-level awaits", async () => {
    const a = await Promise.resolve(1);
    const b = await Promise.resolve(2);
    const c = await Promise.resolve(3);
    expect(a + b + c).toBe(6);
  });

  test("top-level await in expressions", async () => {
    const sum = (await Promise.resolve(10)) + (await Promise.resolve(20));
    expect(sum).toBe(30);
  });

  test("top-level await with assignment", async () => {
    let x;
    x = await Promise.resolve("hello");
    expect(x).toBe("hello");
  });

  test("top-level await with async function call", async () => {
    const fn = async () => 42;
    const x = await fn();
    expect(x).toBe(42);
  });

  test("top-level await in array literal", async () => {
    const arr = [
      await Promise.resolve(1),
      await Promise.resolve(2),
      await Promise.resolve(3),
    ];
    expect(arr).toEqual([1, 2, 3]);
  });

  test("top-level await in object literal", async () => {
    const obj = {
      a: await Promise.resolve(1),
      b: await Promise.resolve(2),
    };
    expect(obj).toEqual({ a: 1, b: 2 });
  });

  test("top-level await in ternary condition", async () => {
    const result = (await Promise.resolve(true)) ? "yes" : "no";
    expect(result).toBe("yes");
  });

  test("top-level await in template literal", async () => {
    const val = await Promise.resolve(42);
    const str = `value: ${val}`;
    expect(str).toBe("value: 42");
  });

  test("top-level await as function argument", async () => {
    const identity = (x) => x;
    const result = identity(await Promise.resolve(42));
    expect(result).toBe(42);
  });

  test("top-level await preserves object identity", async () => {
    const obj = { x: 1 };
    const result = await obj;
    expect(result).toBe(obj);
  });

  test("top-level await with nested async functions", async () => {
    const inner = async () => {
      const val = await Promise.resolve("inner");
      return val;
    };
    const result = await inner();
    expect(result).toBe("inner");
  });

  test("top-level await error propagation from async function", async () => {
    const failing = async () => {
      throw new Error("async fail");
    };

    let caught;
    try {
      await failing();
    } catch (e) {
      caught = e;
    }
    expect(caught.message).toBe("async fail");
  });
});
