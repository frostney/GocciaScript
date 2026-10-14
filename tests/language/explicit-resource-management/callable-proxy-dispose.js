/*---
description: A callable Proxy works as a Symbol.dispose method
features: [explicit-resource-management, Proxy]
---*/

// GetDisposeMethod reads Symbol.dispose with GetMethod, which asks only
// IsCallable (ES2026 §7.3.10 step 3), so a Proxy whose target is a function is
// a dispose method. (Node.js 24 rejects it; that is a V8 bug.)
describe("using with a callable Proxy as the dispose method", () => {
  test("the Proxy is called at block exit with the resource as this", () => {
    const calls = [];
    const resource = {
      [Symbol.dispose]: new Proxy(({ m() { calls.push(this === resource); } }).m, {}),
    };
    {
      using r = resource;
      expect(calls).toEqual([]);
    }
    expect(calls).toEqual([true]);
  });

  test("the Proxy's apply trap runs", () => {
    let trapped = false;
    {
      using r = { [Symbol.dispose]: new Proxy(() => {}, { apply: () => { trapped = true; } }) };
    }
    expect(trapped).toBe(true);
  });

  test("an error thrown by the Proxy propagates out of the block", () => {
    expect(() => {
      {
        using r = { [Symbol.dispose]: new Proxy(() => { throw new SyntaxError("dispose"); }, {}) };
      }
    }).toThrow(SyntaxError);
  });
});
