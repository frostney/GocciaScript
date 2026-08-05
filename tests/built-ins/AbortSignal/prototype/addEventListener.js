/*---
description: AbortSignal abort event dispatch
features: [AbortSignal, AbortController, EventTarget, Event]
---*/

describe("AbortSignal abort event", () => {
  test("fires exactly once no matter how often abort is called", () => {
    const controller = new AbortController();
    let calls = 0;
    controller.signal.addEventListener("abort", () => {
      calls += 1;
    });

    controller.abort();
    controller.abort();
    expect(calls).toBe(1);
  });

  test("delivers an abort event targeted at the signal", () => {
    const controller = new AbortController();
    let received = null;
    controller.signal.addEventListener("abort", (event) => {
      received = event;
    });

    controller.abort();
    expect(received instanceof Event).toBe(true);
    expect(received.type).toBe("abort");
    expect(received.target).toBe(controller.signal);
    expect(received.currentTarget).toBe(null);
  });

  test("exposes the abort reason while the listener runs", () => {
    const controller = new AbortController();
    const reason = { kind: "cancelled" };
    let observed = null;
    controller.signal.addEventListener("abort", (event) => {
      observed = event.target.reason;
    });

    controller.abort(reason);
    expect(observed).toBe(reason);
  });

  test("never fires for a listener added after the abort", () => {
    const controller = new AbortController();
    controller.abort();

    let fired = false;
    controller.signal.addEventListener("abort", () => {
      fired = true;
    });
    expect(fired).toBe(false);
    expect(controller.signal.aborted).toBe(true);
  });

  test("never fires on an already-aborted AbortSignal.abort signal", () => {
    const signal = AbortSignal.abort();
    let fired = false;
    signal.addEventListener("abort", () => {
      fired = true;
    });
    expect(fired).toBe(false);
  });

  test("honors once and removeEventListener", () => {
    const removedController = new AbortController();
    let removedCalls = 0;
    const listener = () => {
      removedCalls += 1;
    };
    removedController.signal.addEventListener("abort", listener);
    removedController.signal.removeEventListener("abort", listener);
    removedController.abort();
    expect(removedCalls).toBe(0);

    const onceController = new AbortController();
    let onceCalls = 0;
    onceController.signal.addEventListener(
      "abort",
      () => {
        onceCalls += 1;
      },
      { once: true }
    );
    onceController.abort();
    expect(onceCalls).toBe(1);
  });

  test("does not fire for other event types", () => {
    const controller = new AbortController();
    let calls = 0;
    controller.signal.addEventListener("cancel", () => {
      calls += 1;
    });

    controller.abort();
    expect(calls).toBe(0);
  });
});
