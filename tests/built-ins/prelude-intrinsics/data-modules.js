/*---
description: The json5, jsonl, delimited, datatree and semver preludes parse and serialize without looking up anything a script can replace
features: [Goccia]
---*/

import * as JSON5 from "goccia:json5";
import * as JSONL from "goccia:jsonl";
import * as CSV from "goccia:csv";
import * as TSV from "goccia:tsv";
import * as YAML from "goccia:yaml";
import * as TOML from "goccia:toml";
import * as semver from "goccia:semver";
import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());
const watched = (f) => expectClean(wire.watch(f));

test("goccia:json5", () => {
  const text = "// c\n{a: 0x1F, b: [1, 'two', +Infinity, NaN], 'c d': .5e1, \u{1D49C}: null, /* x */ e: \"\\u0041\\x41\",}";
  expect(watched(() => JSON5.parse(text, (k, v) => v))).toEqual({ a: 31, b: [1, "two", Infinity, NaN], "c d": 5, "\u{1D49C}": null, e: "AA" });
  const boxed = new Number(2);
  // a Number object serializes as ToNumber(value), which calls its valueOf (JSON.stringify's SerializeJSONProperty)
  const r = expectClean(wire.watch(() => JSON5.stringify({ a: [1, "it's \"q\"", boxed], "b c": { d: true } }, null, 2)), ["Number.prototype.valueOf"]);
  expect(r).toContain("'b c'");
  expect(watched(() => JSON5.stringify({ a: 1, b: 2 }, { replacer: ["a"], quote: "'", space: "\t" }))).toBe("{\n\ta: 1,\n}");
  const bad = wire.watch(() => JSON5.parse("{a:"));
  expect(bad.threw).toBe(true);
  expect(bad.hits).toEqual([]);
});

test("goccia:jsonl", () => {
  expect(watched(() => JSONL.parse("﻿{\"a\":1}\r\n\n[2]\n"))).toEqual([{ a: 1 }, [2]]);
  const bytes = new TextEncoder().encode("1\n2");
  expect(watched(() => JSONL.parse(bytes))).toEqual([1, 2]);
  expect(watched(() => JSONL.parseChunk("1\n{\"a\":", 0, 9))).toEqual({ values: [1], read: 1, done: false, error: null });
});

test("goccia:csv and goccia:tsv", () => {
  expect(watched(() => CSV.parse("a,b\n1,\"x,\"\"y\"\"\"\n", { skipEmptyLines: true }))).toEqual([{ a: "1", b: "x,\"y\"" }]);
  expect(watched(() => CSV.parse("a,b\n1,2", undefined, (k, v) => v + "!"))).toEqual([{ a: "1!", b: "2!" }]);
  expect(watched(() => CSV.parse("1,2", { headers: false }, (j, v) => j + v))).toEqual([["01", "12"]]);
  expect(watched(() => CSV.parseChunk("a\n1\n2", undefined, 0, 4).values)).toEqual([{ a: "1" }]);
  expect(watched(() => CSV.stringify([{ a: "x,y", b: 1 }]))).toBe("a,b\n\"x,y\",1");
  expect(watched(() => CSV.stringify([[1, "q\""]], { headers: false }, (j, v) => v))).toBe("1,\"q\"\"\"");
  expect(watched(() => TSV.parse("a\tb\n1\\t\t2"))).toEqual([{ a: "1\t", b: "2" }]);
  expect(watched(() => TSV.stringify([{ a: "x\ty" }]))).toBe("a\nx\\ty");
});

test("goccia:yaml and goccia:toml", () => {
  expect(watched(() => YAML.parse("a: &x [1, 2]\nb: *x\nc: !custom v\n"))).toMatchObject({ a: [1, 2], b: [1, 2] });
  expect(watched(() => YAML.parseDocuments("a: 1\n---\nb: 2\n").length)).toBe(2);
  expect(watched(() => TOML.parse("a = 1\n[t]\nb = 'x'\n"))).toEqual({ a: 1, t: { b: "x" } });
});

test("goccia:semver", () => {
  expect(watched(() => semver.parse("1.2.3-beta.1").prerelease)).toEqual(["beta", 1]);
  expect(watched(() => semver.satisfies("1.2.3", "^1.0.0"))).toBe(true);
  expect(watched(() => new semver.Range(">=1.0.0 <2.0.0").set.length)).toBe(1);
  expect(watched(() => semver.sort(["1.10.0", "1.2.0"]))).toEqual(["1.2.0", "1.10.0"]);
  expect(watched(() => semver.compare(new semver.SemVer("1.0.0"), "1.0.1"))).toBe(-1);
});
