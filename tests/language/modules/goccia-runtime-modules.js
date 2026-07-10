/*---
description: >
  Loader runtime APIs are available through named-only goccia:* modules instead
  of ambient data-format globals or Goccia.semver.
features: [modules, runtime-modules]
---*/

import {
  parse as parseCSV,
  parseChunk as parseCSVChunk,
  stringify as stringifyCSV,
} from "goccia:csv";
import * as csv from "goccia:csv";
import { parse as parseJSON5, stringify as stringifyJSON5 } from "goccia:json5";
import * as json5 from "goccia:json5";
import {
  parse as parseJSONL,
  parseChunk as parseJSONLChunk,
} from "goccia:jsonl";
import * as jsonl from "goccia:jsonl";
import { parse as parseTOML } from "goccia:toml";
import * as toml from "goccia:toml";
import {
  parse as parseTSV,
  parseChunk as parseTSVChunk,
  stringify as stringifyTSV,
} from "goccia:tsv";
import * as tsv from "goccia:tsv";
import {
  parse as parseYAML,
  parseDocuments as parseYAMLDocuments,
} from "goccia:yaml";
import * as yaml from "goccia:yaml";
import {
  SEMVER_SPEC_VERSION,
  SemVer,
  satisfies as semverSatisfies,
  toComparators,
  valid as validSemver,
  validRange,
} from "goccia:semver";
import * as semver from "goccia:semver";

const expectNamedOnlyNamespace = (namespace, names) => {
  expect("default" in namespace).toBe(false);
  expect(namespace.default).toBe(undefined);
  expect(Object.keys(namespace)).toEqual(names);
};

describe("goccia runtime modules", () => {
  test("data-format modules expose named exports only", () => {
    expectNamedOnlyNamespace(csv, ["parse", "parseChunk", "stringify"]);
    expectNamedOnlyNamespace(json5, ["parse", "stringify"]);
    expectNamedOnlyNamespace(jsonl, ["parse", "parseChunk"]);
    expectNamedOnlyNamespace(toml, ["parse"]);
    expectNamedOnlyNamespace(tsv, ["parse", "parseChunk", "stringify"]);
    expectNamedOnlyNamespace(yaml, ["parse", "parseDocuments"]);
  });

  test("data-format named imports are functional", () => {
    expect(parseCSV("a,b\n1,2")[0].a).toBe("1");
    expect(parseCSVChunk("a,b\n1,2\n", {}).values[0].b).toBe("2");
    expect(stringifyCSV([{ a: "1", b: "2" }])).toBe("a,b\n1,2");

    expect(parseJSON5("{a:1,}").a).toBe(1);
    expect(stringifyJSON5({ a: 1 })).toBe("{a:1}");

    expect(parseJSONL('{"a":1}\n{"a":2}')[1].a).toBe(2);
    expect(parseJSONLChunk('{"a":1}\n').values[0].a).toBe(1);

    expect(parseTOML("a = 1").a).toBe(1);

    expect(parseTSV("a\tb\n1\t2")[0].b).toBe("2");
    expect(parseTSVChunk("a\tb\n1\t2\n", {}).values[0].a).toBe("1");
    expect(stringifyTSV([{ a: "1", b: "2" }])).toBe("a\tb\n1\t2");

    expect(parseYAML("a: 1").a).toBe(1);
    expect(parseYAMLDocuments("---\na: 1\n---\na: 2").length).toBe(2);
  });

  test("namespace imports call the same APIs as named imports", () => {
    expect(csv.parse).toBe(parseCSV);
    expect(json5.parse).toBe(parseJSON5);
    expect(jsonl.parse).toBe(parseJSONL);
    expect(toml.parse).toBe(parseTOML);
    expect(tsv.parse).toBe(parseTSV);
    expect(yaml.parse).toBe(parseYAML);
  });

  test("semver is exposed through the goccia:semver module", () => {
    expect("default" in semver).toBe(false);
    expect(semver.default).toBe(undefined);
    expect(SEMVER_SPEC_VERSION).toBe("2.0.0");
    expect(validSemver("v1.2.3")).toBe("1.2.3");
    expect(semverSatisfies("1.5.0", "^1.2.3")).toBe(true);
    expect(validRange("^1.2.3") !== null).toBe(true);
    expect(toComparators("1.x").length).toBeGreaterThan(0);

    const version = new SemVer("1.2.3-alpha.1+build.5");
    expect(version instanceof SemVer).toBe(true);
    expect(version.version).toBe("1.2.3-alpha.1");

    expect(semver.valid).toBe(validSemver);
    expect(semver.SemVer).toBe(SemVer);
    expect(typeof semver.Comparator).toBe("function");
    expect(typeof semver.Range).toBe("function");
    expect(typeof semver.functions).toBe("object");
    expect(typeof semver.ranges).toBe("object");
    expect(typeof semver.classes).toBe("object");
  });
});
