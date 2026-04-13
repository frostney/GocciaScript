describe("DisposableStack.prototype.use", () => {
  test("tracks a disposable resource and returns it", () => {
    const stack = new DisposableStack();
    const resource = {
      [Symbol.dispose]() {}
    };
    const returned = stack.use(resource);
    expect(returned).toBe(resource);
  });

  test("disposes tracked resources when dispose is called", () => {
    const stack = new DisposableStack();
    let disposed = false;
    stack.use({
      [Symbol.dispose]() { disposed = true; }
    });
    expect(disposed).toBe(false);
    stack.dispose();
    expect(disposed).toBe(true);
  });

  test("disposes resources in reverse order", () => {
    const stack = new DisposableStack();
    const order = [];
    stack.use({ [Symbol.dispose]() { order.push("a"); } });
    stack.use({ [Symbol.dispose]() { order.push("b"); } });
    stack.use({ [Symbol.dispose]() { order.push("c"); } });
    stack.dispose();
    expect(order).toEqual(["c", "b", "a"]);
  });

  test("use with null returns null", () => {
    const stack = new DisposableStack();
    const result = stack.use(null);
    expect(result).toBe(null);
    stack.dispose(); // should not throw
  });

  test("use with undefined returns undefined", () => {
    const stack = new DisposableStack();
    const result = stack.use(undefined);
    expect(result).toBe(undefined);
    stack.dispose();
  });

  test("throws TypeError for non-disposable value", () => {
    const stack = new DisposableStack();
    expect(() => stack.use(42)).toThrow(TypeError);
  });

  test("throws ReferenceError when stack is already disposed", () => {
    const stack = new DisposableStack();
    stack.dispose();
    expect(() => stack.use({ [Symbol.dispose]() {} })).toThrow(ReferenceError);
  });
});
