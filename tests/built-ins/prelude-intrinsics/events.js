/*---
description: The events prelude's Event, EventTarget, AbortController and AbortSignal look up nothing a script can replace
features: [EventTarget, AbortController]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("Event", () => {
  const e = expectClean(wire.construct("globalThis.Event", "x", { bubbles: true, cancelable: true }));
  wire.covers("Event.prototype", e, [
    ["type", [], "x"], ["target", [], null], ["srcElement", [], null], ["currentTarget", [], null], ["eventPhase", [], 0],
    ["bubbles", [], true], ["cancelable", [], true], ["composed", [], false], ["defaultPrevented", [], false],
    ["returnValue", [], true], ["cancelBubble", [], false], ["isTrusted", [], false], ["timeStamp", []],
    ["composedPath", [], []], ["stopPropagation", []], ["stopImmediatePropagation", []], ["preventDefault", []],
  ]);
});

test("EventTarget", () => {
  const t = expectClean(wire.construct("globalThis.EventTarget"));
  const seen = [];
  const listener = () => { seen[seen.length] = "a"; };
  const once = { handleEvent() { seen[seen.length] = "once a"; } };
  const controller = new AbortController();
  wire.covers("EventTarget.prototype", t, [
    ["addEventListener", ["a", listener]], ["addEventListener", ["a", listener]],
    ["addEventListener", ["a", once, { once: true, passive: true, signal: controller.signal }]],
    ["dispatchEvent", [new Event("a")], true], ["dispatchEvent", [new Event("a")], true],
    ["removeEventListener", ["a", listener]], ["dispatchEvent", [new Event("a")], true],
  ]);
  expect(seen).toEqual(["a", "once a", "a"]);
});

test("AbortController and AbortSignal", () => {
  const c = expectClean(wire.construct("globalThis.AbortController"));
  const signal = c.signal;
  const fired = [];
  signal.addEventListener("abort", () => { fired[fired.length] = "listener"; });
  wire.covers("AbortSignal.prototype", signal, [
    ["onabort", [], null], ["aborted", [], false], ["reason", [], undefined], ["throwIfAborted", []],
  ]);
  const setOnabort = Object.getOwnPropertyDescriptor(AbortSignal.prototype, "onabort").set;
  expectClean(wire.watch(() => wire.call(setOnabort, signal, () => { fired[fired.length] = "onabort"; })));
  const any = expectClean(wire.invoke("AbortSignal.any", AbortSignal, [signal]), ARRAY_ITERATION);
  wire.covers("AbortController.prototype", c, [["signal", [], signal], ["abort", ["why"]]]);
  expect(fired).toEqual(["listener", "onabort"]);
  expect(any.reason).toBe("why");
  expectClean(wire.watch(() => wire.call(setOnabort, signal, null)));
  expect(expectClean(wire.invoke("AbortSignal.abort", AbortSignal, "r")).reason).toBe("r");
  expect(expectClean(wire.invoke("AbortSignal.timeout", AbortSignal, 10)).aborted).toBe(false);
  const owners = ["Event.prototype", "EventTarget.prototype", "AbortSignal", "AbortSignal.prototype", "AbortController.prototype"];
  expect(owners.flatMap((o) => wire.uncovered(o))).toEqual([]);
});
