#!/usr/bin/env bun
import { TESTRUNNER } from "./test-cli/binaries";

// Each accepted root must restore the entry and its imported module records
// into a fresh engine, without precompiling or replaying either module.
const modules = Bun.spawnSync([
  TESTRUNNER, "-P", "tests/language/modules/heap-snapshot-round-trip.js",
  "--heap-snapshots", "--silent", "--no-progress", "--output=json",
], { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
if (modules.exitCode !== 0 || JSON.parse(modules.stdout.toString()).ok !== true)
  throw new Error(`Module-record round trip failed: ${modules.stderr}`);
if (!/Heap snapshots: observations=2 objects=2 passed=2 refused=0 failed=0/.test(
  modules.stderr.toString(),
))
  throw new Error(`Unexpected module-record round-trip counts: ${modules.stderr}`);

const body = `
test("observed objects", () => {
  const shared = { value: 1 };
  const object = { a: shared, b: shared, list: [1, , undefined, -0] };
  object.self = object;
  expect(object).toBe(object);
  expect(object).toBe(object);
  object.final = 42;
  expect(new Map()).toBeInstanceOf(Map);
  expect(new ArrayBuffer(1)).toBeInstanceOf(ArrayBuffer);
  expect(1).toBe(1);
});
`;

for (const moduleOnly of [false, true]) {
  const source = moduleOnly
    ? 'import { test, expect } from "goccia:test";\n' + body
    : body;
  const result = Bun.spawnSync([
    TESTRUNNER, "-P", "--heap-snapshots", "--jobs=2", "--silent",
    "--no-progress", "--output=json",
    ...(moduleOnly ? ["--no-vitest-compat", "--source-type=module"] : []), "-",
  ], {
    stdin: new TextEncoder().encode(source), stdout: "pipe", stderr: "pipe",
    timeout: 120_000,
  });
  const report = JSON.parse(result.stdout.toString());
  const diagnostics = result.stderr.toString();
  if (result.exitCode !== 0 || report.ok !== true || report.jobCount !== 1)
    throw new Error(`Round-trip CLI failed: ${result.exitCode} ${diagnostics}`);
  if (!/Heap snapshots: observations=4 objects=3 passed=1 refused=2 failed=0/.test(diagnostics))
    throw new Error(`Unexpected round-trip counts: ${diagnostics}`);
  const refusals = diagnostics.split(/\r?\n/)
    .filter((line) => line.startsWith("Heap snapshot refusal:"));
  const expectedRefusals = [
    "Heap snapshot refusal: 1 no snapshot codec for TGocciaArrayBufferValue",
    "Heap snapshot refusal: 1 no snapshot codec for TGocciaMapValue",
  ];
  if (JSON.stringify(refusals) !== JSON.stringify(expectedRefusals))
    throw new Error(`Unexpected refusal counts or order: ${diagnostics}`);
}
const collected = Bun.spawnSync([
  TESTRUNNER, "-P", "tests/built-ins/Promise/try.js",
  "tests/built-ins/Function/constructor/scope.js",
  "tests/language/classes/static-prototype-field-throws.js", "--heap-snapshots",
  "--silent", "--no-progress", "--output=json",
], { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
if (collected.exitCode !== 0 || JSON.parse(collected.stdout.toString()).ok !== true)
  throw new Error(`GC, dynamic-function or class-template round trip failed: ${collected.stderr}`);
console.log("Heap snapshot CLI: module records, global and module-only expectations passed");
