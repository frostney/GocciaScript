describe("await using declaration", () => {
  test("calls [Symbol.asyncDispose] at block exit", async () => {
    let disposed = false;
    {
      await using resource = {
        [Symbol.asyncDispose]() {
          disposed = true;
          return Promise.resolve();
        }
      };
      expect(disposed).toBe(false);
    }
    expect(disposed).toBe(true);
  });

  test("falls back to [Symbol.dispose] when no asyncDispose", async () => {
    let disposed = false;
    {
      await using resource = {
        [Symbol.dispose]() {
          disposed = true;
        }
      };
      expect(disposed).toBe(false);
    }
    expect(disposed).toBe(true);
  });

  test("disposes multiple async resources in reverse order", async () => {
    const order = [];
    {
      await using a = {
        [Symbol.asyncDispose]() {
          order.push("a");
          return Promise.resolve();
        }
      };
      await using b = {
        [Symbol.asyncDispose]() {
          order.push("b");
          return Promise.resolve();
        }
      };
    }
    expect(order).toEqual(["b", "a"]);
  });

  test("await using null is silently skipped", async () => {
    const order = [];
    // Schedule a microtask before the block to observe the async boundary
    Promise.resolve().then(() => order.push("microtask"));
    {
      await using resource = null;
    }
    // The block-exit await point drains microtasks, so the callback runs
    // during disposal — before "after" is pushed
    order.push("after");
    expect(order).toEqual(["microtask", "after"]);
  });

  test("await using undefined is silently skipped", async () => {
    const order = [];
    // Schedule a microtask before the block to observe the async boundary
    Promise.resolve().then(() => order.push("microtask"));
    {
      await using resource = undefined;
    }
    // The block-exit await point drains microtasks, so the callback runs
    // during disposal — before "after" is pushed
    order.push("after");
    expect(order).toEqual(["microtask", "after"]);
  });

  test("throws TypeError for non-disposable value", async () => {
    let caught;
    try {
      {
        await using resource = { name: "not disposable" };
      }
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof TypeError).toBe(true);
  });
});
