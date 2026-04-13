describe("DisposableStack.prototype.defer", () => {
  test("registers a cleanup callback", () => {
    const stack = new DisposableStack();
    let called = false;
    stack.defer(() => { called = true; });
    expect(called).toBe(false);
    stack.dispose();
    expect(called).toBe(true);
  });

  test("callbacks run in reverse order", () => {
    const stack = new DisposableStack();
    const order = [];
    stack.defer(() => order.push("first"));
    stack.defer(() => order.push("second"));
    stack.defer(() => order.push("third"));
    stack.dispose();
    expect(order).toEqual(["third", "second", "first"]);
  });

  test("throws TypeError if onDispose is not callable", () => {
    const stack = new DisposableStack();
    expect(() => stack.defer("not a function")).toThrow(TypeError);
  });

  test("returns undefined", () => {
    const stack = new DisposableStack();
    const result = stack.defer(() => {});
    expect(result).toBe(undefined);
  });
});
