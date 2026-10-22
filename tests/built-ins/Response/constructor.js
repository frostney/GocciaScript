/*---
description: Response constructor and property accessors
features: [fetch]
---*/

describe("Response constructor", () => {
  test("creates a Response instance", () => {
    const r = new Response();
    expect(r).toBeInstanceOf(Response);
  });

  test("has default status 200", () => {
    const r = new Response();
    expect(r.status).toBe(200);
  });

  test("ok is true for default status", () => {
    const r = new Response();
    expect(r.ok).toBe(true);
  });

  test("type is basic", () => {
    const r = new Response();
    expect(r.type).toBe("basic");
  });

  test("bodyUsed is initially false", () => {
    const r = new Response();
    expect(r.bodyUsed).toBe(false);
  });

  test("redirected is initially false", () => {
    const r = new Response();
    expect(r.redirected).toBe(false);
  });

  test("toString tag is Response", () => {
    const r = new Response();
    expect(Object.prototype.toString.call(r)).toBe("[object Response]");
  });

  test("accepts body as first argument", async () => {
    const r = new Response("hello");
    const text = await r.text();
    expect(text).toBe("hello");
  });

  test.runIf(typeof Goccia !== "undefined")("charges its native body to the memory limit", () => {
    const body = "x".repeat(32 * 1024);
    const before = Goccia.gc.bytesAllocated;
    const response = new Response(body);
    expect(response.status).toBe(200);
    expect(Goccia.gc.bytesAllocated - before).toBeGreaterThanOrEqual(body.length);
  });

  test.runIf(typeof Goccia !== "undefined")("releases the native body charge when collected", () => {
    const body = "x".repeat(64 * 1024);
    Goccia.gc();
    const before = Goccia.gc.bytesAllocated;
    let response = new Response(body);
    expect(response.status).toBe(200);
    const held = Goccia.gc.bytesAllocated;
    expect(held - before).toBeGreaterThanOrEqual(body.length);
    response = null;
    Goccia.gc();
    expect(Goccia.gc.bytesAllocated).toBeLessThan(before + body.length / 2);
  });

  test("accepts null body", async () => {
    const r = new Response(null);
    const text = await r.text();
    expect(text).toBe("");
  });

  test("accepts status in init", () => {
    const r = new Response(null, { status: 404 });
    expect(r.status).toBe(404);
    expect(r.ok).toBe(false);
  });

  test("accepts statusText in init", () => {
    const r = new Response(null, { status: 404, statusText: "Not Found" });
    expect(r.statusText).toBe("Not Found");
  });

  test("accepts headers as plain object in init", () => {
    const r = new Response(null, {
      headers: { "X-Custom": "value" },
    });
    expect(r.headers.get("x-custom")).toBe("value");
  });

  test("accepts headers as Headers instance in init", () => {
    const h = new Headers({ "Content-Type": "text/plain" });
    const r = new Response(null, { headers: h });
    expect(r.headers.get("content-type")).toBe("text/plain");
  });

  test("response headers are independent from init headers", () => {
    const h = new Headers({ "X-Test": "original" });
    const r = new Response(null, { headers: h });
    expect(r.headers.get("x-test")).toBe("original");
    // response.headers should be a separate object
    expect(r.headers).not.toBe(h);
  });

  test("throws RangeError for status below 200", () => {
    expect(() => new Response(null, { status: 100 })).toThrow(RangeError);
  });

  test("throws RangeError for status above 599", () => {
    expect(() => new Response(null, { status: 600 })).toThrow(RangeError);
  });

  test("body and init together", async () => {
    const r = new Response("ok", { status: 201, statusText: "Created" });
    expect(r.status).toBe(201);
    expect(r.statusText).toBe("Created");
    const text = await r.text();
    expect(text).toBe("ok");
  });
});

describe("Response init non-finite status", () => {
  test("status Infinity throws RangeError", () => {
    expect(() => new Response(null, { status: Infinity })).toThrow(RangeError);
  });

  test("status NaN throws RangeError", () => {
    expect(() => new Response(null, { status: NaN })).toThrow(RangeError);
  });
});
