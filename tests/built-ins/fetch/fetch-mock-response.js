/*---
description: fetch Response and Headers behavior tested via mock callbacks
features: [fetch]
---*/

describe("fetch response handling via mock callbacks", () => {
  test("rejected promise calls catch handler with TypeError", async () => {
    const onReject = mock();
    await fetch("http://0.0.0.0:1/").catch(onReject);
    expect(onReject).toHaveBeenCalledTimes(1);
    expect(onReject.mock.calls[0][0].name).toBe("TypeError");
  });

  test("rejected promise error has a message", async () => {
    const onReject = mock();
    await fetch("http://0.0.0.0:1/").catch(onReject);
    const err = onReject.mock.calls[0][0];
    expect(typeof err.message).toBe("string");
    expect(err.message.length).toBeGreaterThan(0);
  });

  test("then handler is not called on network error", async () => {
    const onFulfill = mock();
    const onReject = mock();
    await fetch("http://0.0.0.0:1/").then(onFulfill, onReject);
    expect(onFulfill).toHaveBeenCalledTimes(0);
    expect(onReject).toHaveBeenCalledTimes(1);
  });

  test("catch handler receives single argument", async () => {
    const onReject = mock();
    await fetch("http://0.0.0.0:1/").catch(onReject);
    expect(onReject.mock.calls[0].length).toBe(1);
  });
});

describe("Headers mock integration", () => {
  test("forEach calls mock for each header", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2", "X-C": "3" });
    const fn = mock();
    h.forEach(fn);
    expect(fn).toHaveBeenCalledTimes(3);
  });

  test("forEach mock receives correct arguments", () => {
    const h = new Headers({ "X-Key": "val" });
    const fn = mock();
    h.forEach(fn);
    expect(fn).toHaveBeenCalledWith("val", "x-key", h);
  });

  test("spyOn can wrap forEach callback", () => {
    const h = new Headers({ A: "1", B: "2" });
    const handler = { cb: (v, k) => k + "=" + v };
    const spy = spyOn(handler, "cb");
    h.forEach(handler.cb);
    expect(spy).toHaveBeenCalledTimes(2);
  });
});

describe("Response body consumption mock tracking", () => {
  test("text() fulfillment handler is called once", async () => {
    const r = new Response();
    const onFulfill = mock();
    await r.text().then(onFulfill);
    expect(onFulfill).toHaveBeenCalledTimes(1);
    expect(onFulfill).toHaveBeenCalledWith("");
  });

  test("arrayBuffer() fulfillment handler receives ArrayBuffer", async () => {
    const r = new Response();
    const onFulfill = mock();
    await r.arrayBuffer().then(onFulfill);
    expect(onFulfill).toHaveBeenCalledTimes(1);
    expect(onFulfill.mock.calls[0][0]).toBeInstanceOf(ArrayBuffer);
  });

  test("json() rejection handler called for empty body", async () => {
    const r = new Response();
    const onReject = mock();
    await r.json().catch(onReject);
    expect(onReject).toHaveBeenCalledTimes(1);
  });
});
