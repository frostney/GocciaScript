/*---
description: The console prelude renders values and keeps its counters and timers without looking up anything a script can replace
features: [console]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

// what rendering reads from the values themselves: an error's name, message and stack, which these inherit
const ERROR_FIELDS = ["Error.prototype.name", "Error.prototype.message", "TypeError.prototype.name", "TypeError.prototype.message"];

test("every console method", () => {
  class Point { constructor() { this.x = 1; } }
  const err = new TypeError("bad", { cause: new Error("inner") });
  const values = [
    "s", 1, -0, 2n, Symbol("y"), null, undefined, [1, [2, [3]]], new Map([[1, { a: "q'\"`" }]]), new Set([1]),
    () => {}, Point, new Point(), err, new AggregateError([1], "agg"), { nested: { deeper: { deepest: {} } } },
  ];
  wire.covers("console", console, [
    ["log", values, undefined, ERROR_FIELDS], ["warn", ["w", { a: 1 }]], ["error", ["e"]], ["info", ["i"]],
    ["debug", ["d"]], ["dir", [{ a: [1] }]], ["assert", [false, "x", 1]], ["count", ["c"]], ["count", ["c"]],
    ["countReset", ["c"]], ["time", ["t"]], ["timeLog", ["t"]], ["timeEnd", ["t"]], ["group", ["g"]],
    ["groupCollapsed", ["g\nh"]], ["log", ["in a group\nsecond line"]], ["groupEnd", []], ["groupEnd", []],
    ["trace", ["t"]], ["table", [[1, 2]]], ["clear", []],
  ]);
  expect(wire.uncovered("console")).toEqual([]);
});
