/*---
description: The web prelude's TextEncoder, TextDecoder, DOMException, structuredClone, atob and btoa look up nothing a script can replace, beyond what their specifications read
features: [TextEncoder, TextDecoder, structuredClone, atob, btoa, DOMException]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("TextEncoder and TextDecoder", () => {
  const enc = expectClean(wire.construct("globalThis.TextEncoder"));
  wire.covers("TextEncoder.prototype", enc, [
    ["encoding", [], "utf-8"], ["encode", ["hé\u{1F600}"]], ["encodeInto", ["hi", new Uint8Array(4)], { read: 2, written: 2 }],
  ]);
  const dec = expectClean(wire.construct("globalThis.TextDecoder", " UTF-8 ", { fatal: false }));
  const bytes = new Uint8Array([0xEF, 0xBB, 0xBF, 0x68, 0xE2, 0x82]);
  wire.covers("TextDecoder.prototype", dec, [
    ["encoding", [], "utf-8"], ["fatal", [], false], ["ignoreBOM", [], false],
    ["decode", [bytes, { stream: true }], "h"], ["decode", [new Uint8Array([0xAC]).buffer], "€"],
    ["decode", [new DataView(new Uint8Array([0x61]).buffer)], "a"],
  ]);
  expect(["TextEncoder.prototype", "TextDecoder.prototype"].flatMap((o) => wire.uncovered(o))).toEqual([]);
});

test("atob, btoa and DOMException", () => {
  expect(expectClean(wire.invoke("globalThis.atob", undefined, " aGk= "))).toBe("hi");
  expect(expectClean(wire.invoke("globalThis.btoa", undefined, "hi"))).toBe("aGk=");
  const bad = wire.invoke("globalThis.atob", undefined, "*");
  expect(bad.threw).toBe(true);
  expect(bad.hits).toEqual([]);
  const e = expectClean(wire.construct("globalThis.DOMException", "m", "NotFoundError"));
  wire.covers("DOMException.prototype", e, [["name", [], "NotFoundError"], ["code", [], 8]]);
  expect(e.message).toBe("m");
});

test("structuredClone", () => {
  const source = {
    n: new Number(1), s: new String("s"), b: new Boolean(true), big: Object(1n), d: new Date(0), re: /a/gi,
    ab: new ArrayBuffer(2), ta: new Uint8Array([1, 2]), dv: new DataView(new ArrayBuffer(1)),
    m: new Map([[1, { x: 1 }]]), set: new Set([1]), arr: [1, , 3], err: new RangeError("r"),
    dom: new DOMException("d", "AbortError"), nested: { deep: [{}] },
  };
  source.self = source;
  // an error's name and message are read through Get (HTML StructuredSerializeInternal, error case)
  const r = expectClean(wire.invoke("globalThis.structuredClone", undefined, source),
    ["RangeError.prototype.name", "RangeError.prototype.message"]);
  expect(r.self).toBe(r);
  expect(r.m.get(1)).toEqual({ x: 1 });
  expect(r.re.flags).toBe("gi");
  const refused = wire.invoke("globalThis.structuredClone", undefined, new Headers());
  expect(refused.threw).toBe(true);
  // the error names the value's kind, its Symbol.toStringTag (Object.prototype.toString reads it)
  expect(refused.hits).toEqual(["Headers.prototype[Symbol.toStringTag]"]);
});
