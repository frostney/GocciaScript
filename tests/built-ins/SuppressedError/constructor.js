describe("SuppressedError", () => {
  test("SuppressedError is a function", () => {
    expect(typeof SuppressedError).toBe("function");
  });

  test("creates an error with error and suppressed properties", () => {
    const err = new SuppressedError("new error", "old error");
    expect(err.error).toBe("new error");
    expect(err.suppressed).toBe("old error");
    expect(err.message).toBe("");
  });

  test("accepts a message as third argument", () => {
    const err = new SuppressedError("new", "old", "An error was suppressed");
    expect(err.message).toBe("An error was suppressed");
    expect(err.error).toBe("new");
    expect(err.suppressed).toBe("old");
  });

  test("is an instance of SuppressedError", () => {
    const err = new SuppressedError("a", "b");
    expect(err instanceof SuppressedError).toBe(true);
    expect(err instanceof Error).toBe(true);
  });

  test("has name 'SuppressedError'", () => {
    const err = new SuppressedError("a", "b");
    expect(err.name).toBe("SuppressedError");
  });

  test("has a stack property", () => {
    const err = new SuppressedError("a", "b");
    expect(typeof err.stack).toBe("string");
  });

  test("error and suppressed can be any type", () => {
    const err = new SuppressedError(42, null, "test");
    expect(err.error).toBe(42);
    expect(err.suppressed).toBe(null);
  });

  test("defaults to undefined for missing arguments", () => {
    const err = new SuppressedError();
    expect(err.error).toBe(undefined);
    expect(err.suppressed).toBe(undefined);
    expect(err.message).toBe("");
  });
});
