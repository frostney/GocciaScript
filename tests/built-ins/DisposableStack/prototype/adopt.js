describe("DisposableStack.prototype.adopt", () => {
  test("registers a value with custom dispose callback", () => {
    const stack = new DisposableStack();
    let called = false;
    const resource = { name: "test" };
    stack.adopt(resource, () => { called = true; });
    stack.dispose();
    expect(called).toBe(true);
  });

  test("returns the adopted value", () => {
    const stack = new DisposableStack();
    const resource = { name: "test" };
    const returned = stack.adopt(resource, () => {});
    expect(returned).toBe(resource);
  });

  test("throws TypeError if onDispose is not callable", () => {
    const stack = new DisposableStack();
    expect(() => stack.adopt({}, "not a function")).toThrow(TypeError);
  });
});
