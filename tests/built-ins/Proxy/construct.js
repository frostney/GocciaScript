describe("Proxy construct trap", () => {
  test("intercepts new operator", () => {
    const target = (x) => x;
    const proxy = new Proxy(target, {
      construct: (t, args, newTarget) => {
        return { value: args[0] * 2 };
      },
    });
    const result = new proxy(5);
    expect(result.value).toBe(10);
  });

  test("receives correct target, args, and newTarget", () => {
    const target = () => {};
    let receivedTarget, receivedArgs, receivedNewTarget;
    const proxy = new Proxy(target, {
      construct: (t, args, newTarget) => {
        receivedTarget = t;
        receivedArgs = args;
        receivedNewTarget = newTarget;
        return {};
      },
    });
    new proxy(1, 2, 3);
    expect(receivedTarget).toBe(target);
    expect(receivedArgs[0]).toBe(1);
    expect(receivedArgs[1]).toBe(2);
    expect(receivedArgs[2]).toBe(3);
    expect(receivedNewTarget).toBe(proxy);
  });

  test("throws TypeError when trap returns non-object", () => {
    const target = () => {};
    const proxy = new Proxy(target, {
      construct: () => 42,
    });
    expect(() => new proxy()).toThrow(TypeError);
  });

  test("falls back to target when no construct trap", () => {
    const target = (x) => ({ doubled: x * 2 });
    const proxy = new Proxy(target, {});
    const result = new proxy(5);
    expect(result.doubled).toBe(10);
  });
});
