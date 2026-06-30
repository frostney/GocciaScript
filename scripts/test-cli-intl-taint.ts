#!/usr/bin/env bun
/**
 * test-cli-intl-taint.ts
 *
 * BareLoader-level regression for ECMA-402 robustness under lazy Intl
 * materialization. The first-party test runner materializes Intl eagerly (its
 * harness touches Intl before any test runs), so it cannot reproduce the
 * "materialize Intl after user code taints Object.prototype" path. The bare
 * loader keeps Intl lazy, so it can — analogous to running lexer/parser
 * rejection checks against the bare loader rather than the test runner.
 *
 * The script taints Object.prototype option accessors and the Intl.NumberFormat
 * / Intl.DateTimeFormat globals, then exercises Intl construction and
 * Number/Array/Date toLocaleString. On success it throws a sentinel; any
 * regression throws a different error first, so the sentinel never reaches the
 * output.
 */

import { $ } from "bun";
import { BARE } from "./test-cli/binaries";

const SENTINEL = "INTL_TAINT_OK";

// Each statement throws if Intl reads/writes through a tainted Object.prototype
// accessor (construction) or reads the replaceable Intl.NumberFormat /
// Intl.DateTimeFormat global (toLocaleString) instead of the intrinsic.
const script = `
// Construction must not [[Get]]/[[Set]] through Object.prototype. Intl is first
// touched here, so the namespace materializes after this taint.
for (const name of ["type", "style", "numeric", "localeMatcher"]) {
  Object.defineProperty(Object.prototype, name, {
    configurable: true,
    get() { throw new Error("Object.prototype." + name + " getter ran"); },
  });
}
new Intl.PluralRules().select(9);
new Intl.RelativeTimeFormat().format(1, "day");
new Intl.ListFormat([], undefined).resolvedOptions();
// DisplayNames is where the null-prototype bootstrap fix lives. Construct it
// directly; the options bag is null-prototype because reading a user object's
// inherited tainted accessors is correct ECMA-402 GetOption behaviour, not the
// internal-bootstrap path under test.
new Intl.DisplayNames("en", Object.assign(Object.create(null), { type: "language" })).of("de");
for (const name of ["type", "style", "numeric", "localeMatcher"]) delete Object.prototype[name];

// toLocaleString must construct the intrinsic, not the replaceable global.
Object.defineProperty(Intl, "NumberFormat", {
  configurable: true,
  get() { throw new Error("Intl.NumberFormat global read"); },
});
Object.defineProperty(Intl, "DateTimeFormat", {
  configurable: true,
  get() { throw new Error("Intl.DateTimeFormat global read"); },
});
(0).toLocaleString();
[1, 2].toLocaleString();
new Date(0).toLocaleString();
new Date(0).toLocaleDateString();
new Date(0).toLocaleTimeString();

throw new Error("${SENTINEL}");
`;

const tmp = `${import.meta.dir}/../build/.intl-taint-cli.js`;
await Bun.write(tmp, script);

let failures = 0;
for (const mode of [[], ["--mode=bytecode"]]) {
  const label = mode.length ? "bytecode" : "interpreter";
  console.log(`Intl taint robustness (${label})...`);
  const res = await $`${BARE} ${tmp} ${mode} 2>&1`.nothrow();
  const out = res.text();
  if (!out.includes(SENTINEL)) {
    console.error(`  FAIL [${label}]: a tainted Intl access threw before the sentinel:\n${out}`);
    failures++;
  }
}

await $`rm -f ${tmp}`.nothrow();

if (failures > 0) {
  throw new Error(`Intl taint robustness failed in ${failures} mode(s)`);
}
console.log("Intl taint robustness OK");
