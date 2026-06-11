describe("Promise.withResolvers", () => {
  test("returns object with promise, resolve, and reject", () => {
    const result = Promise.withResolvers();
    const { promise, resolve, reject } = result;
    expect(result instanceof Object).toBe(true);
    expect(typeof resolve).toBe("function");
    expect(typeof reject).toBe("function");
  });

  test("resolve fulfills the promise", () => {
    const { promise, resolve } = Promise.withResolvers();
    resolve(42);
    promise.then((value) => {
      expect(value).toBe(42);
    });
  });

  test("reject rejects the promise", () => {
    const { promise, reject } = Promise.withResolvers();
    reject("error");
    promise.catch((reason) => {
      expect(reason).toBe("error");
    });
  });
});
