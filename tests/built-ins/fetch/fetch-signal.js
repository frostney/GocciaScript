/*---
description: fetch cancellation with AbortSignal
features: [fetch, AbortController, AbortSignal, DOMException]
---*/

describe("fetch signal option", () => {
  test("rejects immediately with a pre-aborted signal reason", async () => {
    const reason = { kind: "cancelled" };
    let rejected;
    await fetch("http://0.0.0.0:1/", {
      signal: AbortSignal.abort(reason),
    }).catch((error) => {
      rejected = error;
    });
    expect(rejected).toBe(reason);
  });

  test("rejects with the reason when a pending request is aborted", async () => {
    const controller = new AbortController();
    const request = fetch("http://0.0.0.0:1/", {
      signal: controller.signal,
    });
    controller.abort("cancelled");

    let rejected;
    await request.catch((error) => {
      rejected = error;
    });
    expect(rejected).toBe("cancelled");
  });

  // WHATWG DOM §3.2 signal abort runs the abort algorithms (step 3) before it
  // fires the "abort" event (step 4). fetch registers its rejection as an abort
  // algorithm, so controller.abort() must settle the request synchronously
  // rather than leaving it for the next completion pump. See ADR 0104.
  test("rejects the request inside abort(), before the abort event", async () => {
    const controller = new AbortController();
    const request = fetch("http://0.0.0.0:1/", {
      signal: controller.signal,
    });
    request.catch(() => {});

    controller.abort("cancelled");

    // Race the request itself against a promise resolved afterwards. Racing an
    // already-settled promise queues its reaction first, so the request wins;
    // had the rejection waited for the next pump, the request would still be
    // pending here and "later-microtask" would win. (Adding a `.then` hop to
    // either arm makes this race non-decisive, so keep it hop-free.)
    const winner = await Promise.race([
      request,
      Promise.resolve("later-microtask"),
    ]).then(
      () => "later-microtask",
      () => "fetch-rejected"
    );

    expect(winner).toBe("fetch-rejected");
  });

  test("settles the request before an abort listener observes it", async () => {
    const controller = new AbortController();
    const order = [];
    const request = fetch("http://0.0.0.0:1/", {
      signal: controller.signal,
    });
    request.catch(() => order.push("rejection-reaction"));

    controller.signal.addEventListener("abort", () => {
      order.push("abort-event");
    });
    controller.abort("cancelled");

    await request.catch(() => {});
    // The abort event runs synchronously inside abort(); the promise reaction
    // is a microtask, so it necessarily lands afterwards.
    expect(order[0]).toBe("abort-event");
    expect(order).toContain("rejection-reaction");
  });

  test("uses TimeoutError for AbortSignal.timeout", async () => {
    let rejected;
    await fetch("http://0.0.0.0:1/", {
      signal: AbortSignal.timeout(0),
    }).catch((error) => {
      rejected = error;
    });

    expect(rejected instanceof DOMException).toBe(true);
    expect(rejected.name).toBe("TimeoutError");
    expect(rejected.code).toBe(23);
  });

  test("accepts null and rejects non-AbortSignal values", () => {
    const request = fetch("http://0.0.0.0:1/", { signal: null });
    request.catch(() => {});
    expect(typeof request.then).toBe("function");
    expect(() =>
      fetch("http://0.0.0.0:1/", { signal: {} })
    ).toThrow(TypeError);
  });
});
