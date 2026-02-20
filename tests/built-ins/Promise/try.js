describe("Promise.try", () => {
  test("resolves with return value of synchronous function", () => {
    const p = Promise.try(() => 42);
    p.then((value) => {
      expect(value).toBe(42);
    });
  });

  test("rejects with thrown error from synchronous function", () => {
    const p = Promise.try(() => {
      throw new Error("oops");
    });
    p.catch((err) => {
      expect(err.message).toBe("oops");
    });
  });

  test("resolves with promise from async function", () => {
    const p = Promise.try(() => Promise.resolve(99));
    p.then((value) => {
      expect(value).toBe(99);
    });
  });
});
