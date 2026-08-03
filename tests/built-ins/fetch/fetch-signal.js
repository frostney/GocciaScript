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
