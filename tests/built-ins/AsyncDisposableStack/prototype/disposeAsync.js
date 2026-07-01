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
