/*---
description: >
  Tests for expect().resolves and expect().rejects matchers
  Vitest/Jest-compatible Promise unwrapping for async assertions
features: [async-await, Promises]
---*/

describe("expect().resolves", () => {
  test("resolves with a fulfilled Promise value", async () => {
    await expect(Promise.resolve(42)).resolves.toBe(42);
  });

  test("resolves with a string value", async () => {
    await expect(Promise.resolve("hello")).resolves.toBe("hello");
  });

  test("resolves with null", async () => {
    await expect(Promise.resolve(null)).resolves.toBeNull();
  });

  test("resolves with undefined", async () => {
    await expect(Promise.resolve(undefined)).resolves.toBeUndefined();
  });

  test("resolves with an array", async () => {
    await expect(Promise.resolve([1, 2, 3])).resolves.toEqual([1, 2, 3]);
  });

  test("resolves with an object", async () => {
    await expect(Promise.resolve({ a: 1 })).resolves.toEqual({ a: 1 });
  });

  test("resolves works with async function", async () => {
    const asyncFn = async () => 99;
    await expect(asyncFn()).resolves.toBe(99);
  });

  test("resolves works with called async function", async () => {
    await expect((async () => "from fn")()).resolves.toBe("from fn");
  });

  test("resolves supports .not negation", async () => {
    await expect(Promise.resolve(10)).resolves.not.toBe(20);
  });

  test("resolves with boolean true", async () => {
    await expect(Promise.resolve(true)).resolves.toBeTruthy();
  });

  test("resolves with zero is falsy", async () => {
    await expect(Promise.resolve(0)).resolves.toBeFalsy();
  });
});

describe("expect().rejects", () => {
  test("rejects with a rejected Promise value", async () => {
    await expect(Promise.reject("error")).rejects.toBe("error");
  });

  test("rejects with a number rejection reason", async () => {
    await expect(Promise.reject(404)).rejects.toBe(404);
  });

  test("rejects.toThrow with TypeError", async () => {
    const asyncFn = async () => {
      throw new TypeError("bad type");
    };
    await expect(asyncFn()).rejects.toThrow(TypeError);
  });

  test("rejects.toThrow with RangeError", async () => {
    const asyncFn = async () => {
      throw new RangeError("out of range");
    };
    await expect(asyncFn()).rejects.toThrow(RangeError);
  });

  test("rejects.toThrow with generic Error", async () => {
    const asyncFn = async () => {
      throw new Error("something went wrong");
    };
    await expect(asyncFn()).rejects.toThrow(Error);
  });

  test("rejects.toThrow without type argument", async () => {
    const asyncFn = async () => {
      throw new Error("any error");
    };
    await expect(asyncFn()).rejects.toThrow();
  });

  test("rejects works with called async function", async () => {
    await expect((async () => {
      throw "direct error";
    })()).rejects.toBe("direct error");
  });

  test("rejects supports .not negation", async () => {
    await expect(Promise.reject("oops")).rejects.not.toBe("different");
  });

  test("rejects with for-await-of on non-iterable", async () => {
    await expect((async () => {
      for await (const x of null) {}
    })()).rejects.toThrow(TypeError);
  });

  test("rejects with object rejection reason", async () => {
    await expect(Promise.reject({ code: 42 })).rejects.toEqual({ code: 42 });
  });
});
