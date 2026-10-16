describe("AsyncDisposableStack.prototype.disposeAsync", () => {
  test("waits for a pending async disposer before resolving", () => {
    const log = [];
    let release;
    const blocker = new Promise((resolve) => { release = resolve; });
    const stack = new AsyncDisposableStack();

    stack.defer(() => blocker.then(() => { log.push("disposed"); }));
    const disposal = stack.disposeAsync().then(() => { log.push("resolved"); });

    return Promise.resolve()
      .then(() => {
        expect(log).toEqual([]);
        release();
      })
      .then(() => disposal)
      .then(() => {
        expect(log).toEqual(["disposed", "resolved"]);
      });
  });

  test("rejects when a pending async disposer rejects", () => {
    const log = [];
    let rejectLater;
    const blocker = new Promise((resolve, reject) => { rejectLater = reject; });
    const stack = new AsyncDisposableStack();

    stack.defer(() => blocker);
    const disposal = stack.disposeAsync().then(
      () => { throw "disposeAsync resolved"; },
      (error) => {
        expect(error).toBe("boom");
        log.push("rejected");
      }
    );

    return Promise.resolve()
      .then(() => {
        expect(log).toEqual([]);
        rejectLater("boom");
      })
      .then(() => disposal)
      .then(() => {
        expect(log).toEqual(["rejected"]);
      });
  });

  test("chains async disposal rejections with SuppressedError", () => {
    const stack = new AsyncDisposableStack();

    stack.defer(() => Promise.reject("error a"));
    stack.defer(() => Promise.reject("error b"));

    return stack.disposeAsync().then(
      () => { throw "disposeAsync resolved"; },
      (error) => {
        expect(error instanceof SuppressedError).toBe(true);
        expect(error.error).toBe("error a");
        expect(error.suppressed).toBe("error b");
      }
    );
  });
});

describe.runIf(typeof Goccia !== "undefined")("disposeAsync across a collection", () => {
  test("returns a usable promise when a synchronous disposer collects", async () => {
    const log = [];
    const stack = new AsyncDisposableStack();
    stack.defer(() => { log.push("second"); Goccia.gc(); });
    stack.defer(() => { log.push("first"); Goccia.gc(); });

    const disposal = stack.disposeAsync();
    expect(disposal instanceof Promise).toBe(true);
    await disposal;
    expect(log).toEqual(["first", "second"]);
  });

  test("awaits a disposer result whose then getter collects", async () => {
    const log = [];
    const stack = new AsyncDisposableStack();
    stack.defer(() => { log.push("earlier"); });
    stack.defer(() => ({
      get then() {
        Goccia.gc();
        return undefined;
      },
    }));

    await stack.disposeAsync();
    expect(log).toEqual(["earlier"]);
  });

  test("rejects with a disposer's error after a later disposer collects", async () => {
    const stack = new AsyncDisposableStack();
    stack.defer(() => { Goccia.gc(); });
    stack.defer(() => { throw new RangeError("kept"); });

    await expect(stack.disposeAsync()).rejects.toThrow("kept");
  });
});
