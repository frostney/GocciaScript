/*---
description: The host prelude's performance, PermissionDenied, Temporal.PlainMonthDay.compare and goccia:ast parse look up nothing a script can replace
features: [Goccia, performance, Temporal, experimental-ast]
---*/

import { parse } from "goccia:ast";
import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("performance", () => {
  wire.covers("Performance.prototype", performance, [["now", []], ["toJSON", []], ["timeOrigin", []]]);
  const r = wire.construct("globalThis.Performance");
  expect(r.threw).toBe(true);
  expect(r.hits).toEqual([]);
  expect(wire.uncovered("Performance.prototype")).toEqual([]);
});

test("PermissionDenied", () => {
  const e = expectClean(wire.construct("globalThis.PermissionDenied", "no", { cause: 1 }));
  expect([e.name, e.message, e.cause]).toEqual(["PermissionDenied", "no", 1]);
  expect(expectClean(wire.invoke("globalThis.PermissionDenied", undefined, "x")).message).toBe("x");
});

test("Temporal.PlainMonthDay.compare", () => {
  expect(expectClean(wire.invoke("Temporal.PlainMonthDay.compare", Temporal.PlainMonthDay, "02-29", { monthCode: "M03", day: 1 }))).toBe(-1);
  expect(expectClean(wire.invoke("Temporal.PlainMonthDay.compare", Temporal.PlainMonthDay, "12-02", "12-01"))).toBe(1);
});

test("goccia:ast parse", () => {
  const r = expectClean(wire.watch(() => parse("const a = [1]; // c", { tokens: true, expressions: true })));
  expect(r.source).toBe("const a = [1]; // c");
  expect(r.comments.length).toBe(1);
  expect(r.tokens.length > 0).toBe(true);
});
