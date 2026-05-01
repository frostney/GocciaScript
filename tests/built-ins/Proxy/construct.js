describe("Proxy construct trap", () => {
  test("intercepts new operator", () => {
    class Target {}
    const proxy = new Proxy(Target, {
      construct: (t, args, newTarget) => {
        return { value: args[0] * 2 };
      },
    });
    const result = new proxy(5);
    expect(result.value).toBe(10);
  });

  test("receives correct target, args, and newTarget", () => {
    class Target {}
    let receivedTarget, receivedArgs, receivedNewTarget;
    const proxy = new Proxy(Target, {
      construct: (t, args, newTarget) => {
        receivedTarget = t;
        receivedArgs = args;
        receivedNewTarget = newTarget;
        return {};
      },
    });
    new proxy(1, 2, 3);
    expect(receivedTarget).toBe(Target);
    expect(receivedArgs[0]).toBe(1);
    expect(receivedArgs[1]).toBe(2);
    expect(receivedArgs[2]).toBe(3);
    expect(receivedNewTarget).toBe(proxy);
  });

  test("throws TypeError when trap returns non-object", () => {
    class Target {}
    const proxy = new Proxy(Target, {
      construct: () => 42,
    });
    expect(() => new proxy()).toThrow(TypeError);
  });

  test("throws TypeError when target is not constructable", () => {
    const target = () => {};
    const proxy = new Proxy(target, {
      construct: () => ({}),
    });
    expect(() => new proxy()).toThrow(TypeError);
  });

  test("allows constructable bound target", () => {
    class Target {}
    const bound = Target.bind(null);
    const proxy = new Proxy(bound, {
      construct: () => ({ ok: true }),
    });
    expect(new proxy().ok).toBe(true);
  });

  test("falls back to target when no construct trap", () => {
    class Target {
      constructor(x) {
        this.doubled = x * 2;
      }
    }
    const proxy = new Proxy(Target, {});
    const result = new proxy(5);
    expect(result.doubled).toBe(10);
  });
});
