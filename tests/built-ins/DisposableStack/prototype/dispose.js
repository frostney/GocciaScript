describe("DisposableStack.prototype.dispose", () => {
  test("is idempotent (disposer runs exactly once)", () => {
    const stack = new DisposableStack();
    let calls = 0;
    stack.use({ [Symbol.dispose]() { calls += 1; } });
    stack.dispose();
    stack.dispose(); // second call is a no-op
    expect(calls).toBe(1);
  });

  test("sets disposed to true after disposal", () => {
    const stack = new DisposableStack();
    expect(stack.disposed).toBe(false);
    stack.dispose();
    expect(stack.disposed).toBe(true);
  });

  test("chains errors with SuppressedError", () => {
    const stack = new DisposableStack();
    stack.use({ [Symbol.dispose]() { throw "error a"; } });
    stack.use({ [Symbol.dispose]() { throw "error b"; } });
    let caught;
    try {
      stack.dispose();
    } catch (e) {
      caught = e;
    }
    // b disposed first (reverse order), then a
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error).toBe("error a");
    expect(caught.suppressed).toBe("error b");
  });

  test("is accessible via Symbol.dispose", () => {
    const stack = new DisposableStack();
    let disposed = false;
    stack.use({ [Symbol.dispose]() { disposed = true; } });
    // Call via Symbol.dispose
    stack[Symbol.dispose]();
    expect(disposed).toBe(true);
  });
});
