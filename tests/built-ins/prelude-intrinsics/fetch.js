/*---
description: The fetch prelude's Blob, Headers, Response and fetch argument handling look up nothing a script can replace, beyond the Web IDL conversions of their arguments
features: [Blob, Headers, Response, fetch]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("Blob", async () => {
  const parts = ["a\r\nb", new Uint8Array([99]), new Uint8Array([100]).buffer, new DataView(new ArrayBuffer(1)), new Blob(["e"])];
  // Web IDL sequence<BlobPart>: the parts array's own iterator
  const b = expectClean(wire.construct("globalThis.Blob", parts, { type: "Text/Plain", endings: "native" }), ARRAY_ITERATION);
  wire.covers("Blob.prototype", b, [["size", [], 7], ["type", [], "text/plain"], ["slice", [1, -1, "X"]]]);
  expect(expectClean(await wire.invokeAsync("Blob.prototype.text", b))).toBe("a\nbcd\0e");
  expect(expectClean(await wire.invokeAsync("Blob.prototype.arrayBuffer", b)).byteLength).toBe(7);
  expect(expectClean(await wire.invokeAsync("Blob.prototype.bytes", b)).length).toBe(7);
});

test("Headers", () => {
  const h = expectClean(wire.construct("globalThis.Headers", [["A", " 1 "], ["set-cookie", "x"]]), ARRAY_ITERATION);
  expect(expectClean(wire.construct("globalThis.Headers", { b: "2" })).get("b")).toBe("2");
  expect(expectClean(wire.construct("globalThis.Headers", h)).get("a")).toBe("1");
  const seen = [];
  wire.covers("Headers.prototype", h, [
    ["append", ["a", "2"]], ["get", ["A"], "1, 2"], ["has", ["a"], true], ["set", ["c", "3"]], ["delete", ["c"]],
    ["getSetCookie", [], ["x"]], ["forEach", [(v, k) => { seen[seen.length] = k; }]],
    ["entries", []], ["keys", []], ["values", []], ["[Symbol.iterator]", []],
  ]);
  expect(seen).toEqual(["a", "set-cookie"]);
});

test("Response", async () => {
  const r = expectClean(wire.construct("globalThis.Response", "body", { status: 201, statusText: "C", headers: { x: "1" } }));
  wire.covers("Response.prototype", r, [
    ["status", [], 201], ["statusText", [], "C"], ["ok", [], true], ["headers", []], ["type", [], "basic"],
    ["url", [], ""], ["redirected", [], false], ["bodyUsed", [], false], ["body", []], ["clone", []],
  ]);
  expect(expectClean(await wire.invokeAsync("Response.prototype.text", r))).toBe("body");
  const make = (body) => new Response(body, { headers: { "content-type": "Text/X" } });
  expect(expectClean(await wire.invokeAsync("Response.prototype.json", new Response("[1]")))).toEqual([1]);
  expect(expectClean(await wire.invokeAsync("Response.prototype.arrayBuffer", make(new Uint8Array([1])))).byteLength).toBe(1);
  expect(expectClean(await wire.invokeAsync("Response.prototype.bytes", make(new URLSearchParams("a=1")))).length).toBe(3);
  expect(expectClean(await wire.invokeAsync("Response.prototype.blob", make("b"))).type).toBe("text/x");
  wire.covers("Response", Response, [
    ["error", []], ["json", [{ a: 1 }]], ["redirect", ["https://example.com/", 301]],
  ]);
  const owners = ["Blob.prototype", "Headers.prototype", "Response", "Response.prototype"];
  expect(owners.flatMap((o) => wire.uncovered(o))).toEqual([]);
});

test("fetch argument handling", () => {
  const signal = new AbortController().signal;
  const r = wire.invoke("globalThis.fetch", undefined, "https://example.com/", { method: "get", headers: new Headers({ a: "1" }), redirect: "manual", signal });
  // without a network grant fetch throws PermissionDenied once its arguments are read; the native that makes the
  // error reads the global binding (Goccia.VM NewPermissionDenied), which is not the prelude's
  expect(r.hits.filter((h) => h !== "globalThis.PermissionDenied")).toEqual([]);
});
