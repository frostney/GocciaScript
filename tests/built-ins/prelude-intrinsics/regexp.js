/*---
description: The regexp prelude's RegExp and String methods look up nothing a script can replace beyond the RegExp protocol ECMA-262 reads
features: [RegExp, RegExp.escape, String.prototype.matchAll, String.prototype.replaceAll]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

// what ECMA-262's RegExp algorithms read from a RegExp object (22.2.7.1 RegExpExec, 22.2.6.4 get flags, the Symbol
// methods, and SpeciesConstructor for split and matchAll): the patterns below inherit them from RegExp.prototype
const FLAGS = ["hasIndices", "global", "ignoreCase", "multiline", "dotAll", "unicode", "unicodeSets", "sticky"];
const PROTOCOL = [
  "RegExp.prototype.exec", "RegExp.prototype.flags getter", ...FLAGS.map((f) => `RegExp.prototype.${f} getter`),
  "RegExp.prototype[Symbol.match]", "RegExp.prototype[Symbol.matchAll]", "RegExp.prototype[Symbol.replace]",
  "RegExp.prototype[Symbol.search]", "RegExp.prototype[Symbol.split]", "RegExp.prototype.constructor",
  "RegExp[Symbol.species] getter", "RegExp.prototype.source getter",
];
const call = (path, thisArg, ...args) => expectClean(wire.invoke(path, thisArg, ...args), PROTOCOL);

test("the RegExp.prototype methods", () => {
  expect(call("RegExp.prototype[Symbol.replace]", /(?<y>\d{4})-(\d\d)/g, "on 2020-01 and 2021-02", "$<y>/$2 ($&) $` $'")).toContain("2020/01");
  expect(call("RegExp.prototype[Symbol.replace]", /b/g, "abcb", (m, i) => `[${m}${i}]`)).toBe("a[b1]c[b3]");
  expect(call("RegExp.prototype[Symbol.split]", /,\s*/u, "a, b,c", 2)).toEqual(["a", "b"]);
  expect(call("RegExp.prototype[Symbol.match]", /a/g, "banana")).toEqual(["a", "a", "a"]);
  expect(call("RegExp.prototype[Symbol.search]", /n/y, "nana")).toBe(0);
  const it = call("RegExp.prototype[Symbol.matchAll]", /a(n)?/g, "banana");
  const first = call("%RegExpStringIteratorPrototype%.next", it);
  expect(first.value[0]).toBe("an");
  expect(call("RegExp.prototype.test", /x/i, "aXb")).toBe(true);
  expect(call("RegExp.prototype.toString", /a\/b/gimsuyd)).toBe("/a\\/b/dgimsuy");
  expect(call("RegExp.prototype.flags", /a/dgimsuy)).toBe("dgimsuy");
  expect(call("RegExp.prototype.compile", /x/, "y", "g").source).toBe("y");
  expect(call("RegExp.escape", RegExp, "a.b*c")).toBe("\\x61\\.b\\*c");
});

test("the RegExp constructor", () => {
  const r = expectClean(wire.construct("globalThis.RegExp", /a/g, "i"), PROTOCOL);
  expect(r.flags).toBe("i");
  const symbolLike = { [Symbol.match]: true, source: "s", flags: "g" };
  expect(expectClean(wire.construct("globalThis.RegExp", symbolLike), PROTOCOL).source).toBe("s");
});

test("the String methods that take a pattern", () => {
  expect(call("String.prototype.replace", "a-b-c", /-/g, "+")).toBe("a+b+c");
  expect(call("String.prototype.replaceAll", "a-b-c", /-/g, "+")).toBe("a+b+c");
  expect(call("String.prototype.replace", "a-b", "-", "$&$&")).toBe("a--b");
  expect(call("String.prototype.replaceAll", "a-b-c", "-", (m, i) => `${i}`)).toBe("a1b3c");
  expect(call("String.prototype.split", "a1b2c", /\d/)).toEqual(["a", "b", "c"]);
  expect(call("String.prototype.split", "a,b", ",")).toEqual(["a", "b"]);
  expect(call("String.prototype.match", "abc", /b/).index).toBe(1);
  expect(call("String.prototype.search", "abc", "c")).toBe(2);
  expect(call("String.prototype.matchAll", "a1a2", /a(\d)/g).next().value[1]).toBe("1");
  expect(call("String.prototype.trimLeft", "  a")).toBe("a");
  expect(call("String.prototype.trimRight", "a  ")).toBe("a");
});

test("the native RegExp.prototype methods and getters", () => {
  wire.covers("RegExp.prototype", /a/dgimsy, [
    ["exec", ["xa"]], ["source", [], "a"], ...FLAGS.map((f) => [f, [], f !== "unicode" && f !== "unicodeSets"]),
  ].map(([name, args, expected]) => [name, args, expected, PROTOCOL]));
});

test("every RegExp built-in the prelude defines has a case", () => {
  expect(["RegExp.prototype", "%RegExpStringIteratorPrototype%"].flatMap((o) => wire.uncovered(o))).toEqual([]);
});
