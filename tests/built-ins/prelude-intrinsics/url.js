/*---
description: The url prelude's URL and URLSearchParams look up nothing a script can replace, beyond the Web IDL conversions of their arguments
features: [URL, URLSearchParams]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("URL", () => {
  const u = expectClean(wire.construct("globalThis.URL", "../b?x=1#h", "https://user:pw@example.com:8080/a/c"));
  const setter = (k) => Object.getOwnPropertyDescriptor(URL.prototype, k).set;
  const set = (k, v) => { const f = setter(k); expectClean(wire.watch(() => wire.call(f, u, v))); };
  wire.covers("URL.prototype", u, [
    ["href", [], "https://user:pw@example.com:8080/b?x=1#h"], ["origin", [], "https://example.com:8080"],
    ["protocol", [], "https:"], ["username", [], "user"], ["password", [], "pw"], ["host", [], "example.com:8080"],
    ["hostname", [], "example.com"], ["port", [], "8080"], ["pathname", [], "/b"], ["search", [], "?x=1"],
    ["searchParams", []], ["hash", [], "#h"], ["toString", []], ["toJSON", []],
  ]);
  for (const [k, v] of [["protocol", "HTTP:"], ["port", " 81 "], ["search", "?y=2"], ["hash", "#z"], ["pathname", "/p"],
    ["host", "example.org"], ["hostname", "example.net"], ["username", "u"], ["password", "p"], ["href", "https://x.test/"]]) set(k, v);
  expect(u.href).toBe("https://x.test/");
  expect(expectClean(wire.invoke("URL.canParse", URL, "https://a.test/"))).toBe(true);
  expect(expectClean(wire.invoke("URL.parse", URL, "nope"))).toBe(null);
  expect(wire.uncovered("URL.prototype")).toEqual([]);
});

test("URLSearchParams", () => {
  const p = expectClean(wire.construct("globalThis.URLSearchParams", "?a=1&b=2&a=3"));
  expect(expectClean(wire.construct("globalThis.URLSearchParams", [["x", "1"]]), ARRAY_ITERATION).get("x")).toBe("1");
  expect(expectClean(wire.construct("globalThis.URLSearchParams", { y: "2" })).get("y")).toBe("2");
  expect(expectClean(wire.construct("globalThis.URLSearchParams", p)).get("b")).toBe("2");
  const seen = [];
  wire.covers("URLSearchParams.prototype", p, [
    ["append", ["c", "4"]], ["get", ["a"], "1"], ["getAll", ["a"], ["1", "3"]], ["has", ["a", "3"], true],
    ["has", ["z"], false], ["set", ["a", "9"]], ["delete", ["c"]], ["delete", ["b", "x"]], ["sort", []],
    ["toString", [], "a=9&b=2"], ["forEach", [(v, k) => { seen[seen.length] = k; }]], ["size", [], 2],
    ["keys", []], ["values", []], ["entries", []], ["[Symbol.iterator]", []],
  ]);
  expect(seen).toEqual(["a", "b"]);
  const it = p.entries(), next = Object.getPrototypeOf(it).next;
  expect(expectClean(wire.watch(() => wire.call(next, it)))).toEqual({ value: ["a", "9"], done: false });
  expect(wire.uncovered("URLSearchParams.prototype")).toEqual([]);
});
