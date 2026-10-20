#!/usr/bin/env bun
/** Source/bytecode differential coverage through the bundler and runner.
 * Invoked by test-cli-apps.ts in PR and platform CI; also runnable alone.
 */
import { readFileSync, writeFileSync, mkdirSync, unlinkSync, truncateSync } from "fs";
import { join, resolve } from "path";
import { BUNDLER, RUNNER } from "./test-cli/binaries";
import { normalizeLineEndings } from "./test-cli/assertions";
import { clean, mkdtemp } from "./test-cli/tmpdir";

const smolFlags = [
  "--compat-var",
  "--compat-function",
  "--compat-traditional-for-loop",
  "--compat-while-loops",
  "--compat-for-in-loop",
  "--compat-loose-equality",
  "--compat-asi",
  "--compat-arguments-object",
  "--compat-label",
];

function invoke(binary: string, args: string[], succeeds = true): string {
  const result = Bun.spawnSync([binary, ...args], {
    stdout: "pipe",
    stderr: "pipe",
    timeout: 60_000,
  });
  const output = normalizeLineEndings(
    result.stdout.toString() + result.stderr.toString(),
  );
  if ((result.exitCode === 0) !== succeeds || result.signalCode)
    throw new Error(`${binary} ${args.join(" ")}: exit ${result.exitCode}\n${output}`);
  return output;
}

function resultLines(output: string): string {
  return output.split("\n").filter((line) => line.startsWith("AOT:")).join("\n");
}

function equal(actual: string, expected: string, context: string): void {
  if (actual !== expected)
    throw new Error(`${context}\nExpected: ${expected}\nActual: ${actual}`);
}

function compile(path: string, flags: string[] = []): string {
  const artifact = path.replace(/\.[^.\\/]+$/, ".gbc");
  invoke(BUNDLER, [path, `--output=${artifact}`, ...flags]);
  return artifact;
}

export function verifyAotModules(): void {
  const tmp = mkdtemp("goccia-aot-");
  try {
    const flags = ["--compat-function", "--compat-var"];
    const sources: Record<string, string> = {
      "counter.mjs": `
        export let value = 2;
        export function increment() { value += 1; }
        export const { left, right: renamed } = { left: 4, right: 5 };
        if (true) { var hoisted = 6; }
        export { hoisted };
      `,
      "cycle-a.mjs": `
        import { callEarly } from "./cycle-b.mjs";
        export function early() { return 7; }
        export const observed = callEarly();
      `,
      "cycle-b.mjs": `
        import { early } from "./cycle-a.mjs";
        export function callEarly() { return early(); }
      `,
      "barrel.mjs": `
        export * from "./counter.mjs";
        export { observed as cycle } from "./cycle-a.mjs";
        export default function named() { return 8; }
        export const replace = () => { named = () => 9; };
      `,
      // Deliberately never compiled: a mixed graph must retain source fallback.
      "fallback.mjs": 'export const fallback = 10;',
      "entry.mjs": `
        import current, { value, increment, left, renamed, hoisted, cycle, replace } from "./barrel.mjs";
        import * as counter from "./counter.mjs";
        import { fallback } from "./fallback.mjs";
        increment();
        const before = current();
        replace();
        console.log("AOT:" + JSON.stringify([value, counter.value, left, renamed, hoisted, cycle, before, current(), fallback]));
        42;
      `,
    };
    for (const [name, text] of Object.entries(sources))
      writeFileSync(join(tmp, name), text);
    const entry = join(tmp, "entry.mjs");
    const expected = "AOT:[3,3,4,5,6,7,8,9,10]";
    equal(resultLines(invoke(RUNNER, [entry, ...flags])), expected, "source graph");
    for (const name of Object.keys(sources))
      if (name !== "fallback.mjs") compile(join(tmp, name), flags);
    equal(
      resultLines(invoke(RUNNER, [entry, "--prefer-bytecode", ...flags])),
      expected,
      "mixed precompiled graph",
    );
    // Remove every compiled dependency source: accidental reparsing must fail.
    for (const name of Object.keys(sources))
      if (name !== "fallback.mjs") unlinkSync(join(tmp, name));
    const compiledEntry = join(tmp, "entry.gbc");
    const artifactRun = invoke(RUNNER, [compiledEntry, "--prefer-bytecode", "--print", ...flags]);
    equal(resultLines(artifactRun), expected, "bytecode-only graph with source fallback");
    if (!artifactRun.split("\n").includes("42"))
      throw new Error(`Compiled module entry lost completion value:\n${artifactRun}`);

    const rootCycleDir = join(tmp, "root-cycle");
    mkdirSync(rootCycleDir);
    const rootCycleEntry = join(rootCycleDir, "entry.mjs");
    const rootCycleDependency = join(rootCycleDir, "dependency.mjs");
    writeFileSync(rootCycleEntry, `
      import { readEntry } from "./dependency.mjs";
      export function entryValue() { return 17; }
      globalThis.entryRuns = (globalThis.entryRuns ?? 0) + 1;
      console.log("AOT:" + readEntry() + ":" + globalThis.entryRuns);
    `);
    writeFileSync(rootCycleDependency, `
      import { entryValue } from "./entry.mjs";
      export function readEntry() { return entryValue(); }
    `);
    const rootCycleExpected = resultLines(invoke(RUNNER, [rootCycleEntry, ...flags]));
    equal(rootCycleExpected, "AOT:17:1", "source entry cycle");
    const rootCycleArtifact = compile(rootCycleEntry, flags);
    compile(rootCycleDependency, flags);
    unlinkSync(rootCycleEntry);
    unlinkSync(rootCycleDependency);
    equal(
      resultLines(invoke(RUNNER, [rootCycleArtifact, "--prefer-bytecode", ...flags])),
      rootCycleExpected,
      "compiled entry cycle evaluates the root once",
    );

    const extensionlessDir = join(tmp, "extensionless");
    mkdirSync(extensionlessDir);
    mkdirSync(join(extensionlessDir, "directory"));
    const extensionlessEntry = join(extensionlessDir, "entry.mjs");
    writeFileSync(join(extensionlessDir, "library.mjs"), `
      globalThis.libraryRuns = (globalThis.libraryRuns ?? 0) + 1;
      export const value = 19;
      export const address = import.meta.url;
    `);
    writeFileSync(join(extensionlessDir, "directory", "index.mjs"), "export const value = 23;");
    writeFileSync(extensionlessEntry, `
      import { value as file, address } from "./library";
      import { value as explicit } from "./library.mjs";
      import { value as directory } from "./directory";
      console.log("AOT:" + (file + directory) + ":" + explicit + ":" + globalThis.libraryRuns);
      console.log("AOT:" + address);
    `);
    const extensionlessExpected = resultLines(invoke(RUNNER, [extensionlessEntry]));
    equal(extensionlessExpected.split("\n")[0], "AOT:42:19:1", "source extensionless and directory imports");
    for (const source of [
      join(extensionlessDir, "library.mjs"),
      join(extensionlessDir, "directory", "index.mjs"),
    ]) {
      compile(source);
      unlinkSync(source);
    }
    equal(
      resultLines(invoke(RUNNER, [extensionlessEntry, "--prefer-bytecode"])),
      extensionlessExpected,
      "source-free extensionless and directory companions",
    );
    const extensionlessDenied = invoke(RUNNER, [
      extensionlessEntry, "--prefer-bytecode",
      `--deny-read=${join(extensionlessDir, "library.gbc")}`,
    ], false);
    if (!extensionlessDenied.toLowerCase().includes("permission"))
      throw new Error(`Extensionless metadata reads must honor explicit denies:\n${extensionlessDenied}`);
    const sandboxCompanions = resultLines(invoke(RUNNER, [
      extensionlessEntry, `--copy=${extensionlessDir}=/`, "--prefer-bytecode",
    ]));
    equal(sandboxCompanions.split("\n")[0], "AOT:42:19:1", "sandbox companion resolution");
    if (!sandboxCompanions.split("\n")[1]?.endsWith("/library.mjs"))
      throw new Error(`Sandbox companion lost its logical address:\n${sandboxCompanions}`);

    const packageDir = join(tmp, "packages");
    const esmPackage = join(packageDir, "node_modules", "aot-esm");
    const cjsPackage = join(packageDir, "node_modules", "aot-commonjs");
    mkdirSync(esmPackage, { recursive: true });
    mkdirSync(cjsPackage, { recursive: true });
    writeFileSync(join(esmPackage, "package.json"), JSON.stringify({ main: "index.mjs" }));
    writeFileSync(join(cjsPackage, "package.json"), JSON.stringify({ main: "index.js" }));
    writeFileSync(join(esmPackage, "index.mjs"), "export const value = 42;");
    writeFileSync(join(cjsPackage, "index.js"), 'console.log("AOT:forbidden-body"); module.exports = { value: 42 };');
    const packageEntry = join(packageDir, "entry.mjs");
    writeFileSync(packageEntry, 'import { value } from "aot-esm"; console.log("AOT:" + value);');
    equal(resultLines(invoke(RUNNER, [packageEntry, "--allow-import=node_modules"])),
      "AOT:42", "source package main");
    compile(join(esmPackage, "index.mjs"));
    unlinkSync(join(esmPackage, "index.mjs"));
    compile(join(cjsPackage, "index.js"), ["--source-type=module"]);
    equal(resultLines(invoke(RUNNER, [
      packageEntry, "--allow-import=node_modules", "--prefer-bytecode",
    ])), "AOT:42", "source-free package main companion");
    writeFileSync(packageEntry, 'import { value } from "aot-commonjs"; console.log(value);');
    const commonJS = invoke(RUNNER, [
      packageEntry, "--allow-import=node_modules", "--prefer-bytecode",
    ], false);
    if (!commonJS.includes("CommonJS") || commonJS.includes("AOT:forbidden-body"))
      throw new Error(`A companion must not bypass CommonJS package policy:\n${commonJS}`);

    const asyncDir = join(tmp, "async");
    mkdirSync(asyncDir);
    writeFileSync(join(asyncDir, "awaited.mjs"), `
      export const value = await Promise.resolve(12);
    `);
    writeFileSync(join(asyncDir, "deferred.mjs"), `
      import { value } from "./awaited.mjs";
      export const answer = value + 1;
    `);
    writeFileSync(join(asyncDir, "entry.mjs"), `
      import { value } from "./awaited.mjs";
      import defer * as deferred from "./deferred.mjs";
      console.log("AOT:" + (deferred.answer + value - 12));
    `);
    const asyncEntry = join(asyncDir, "entry.mjs");
    const asyncExpected = resultLines(invoke(RUNNER, [asyncEntry]));
    equal(asyncExpected, "AOT:13", "source deferred/TLA graph");
    for (const name of ["awaited", "deferred", "entry"]) {
      const source = join(asyncDir, `${name}.mjs`);
      compile(source);
      unlinkSync(source);
    }
    equal(
      resultLines(invoke(RUNNER, [join(asyncDir, "entry.gbc"), "--prefer-bytecode"])),
      asyncExpected,
      "compiled deferred/TLA graph",
    );

    // Present-but-invalid companions must not silently select source.
    writeFileSync(join(tmp, "reject.mjs"), 'console.log("AOT:body-ran"); export const value = 1;');
    writeFileSync(join(tmp, "reject.gbc"), "invalid artifact");
    writeFileSync(join(tmp, "reject-entry.mjs"), 'import { value } from "./reject.mjs"; console.log(value);');
    const corrupt = invoke(RUNNER, [join(tmp, "reject-entry.mjs"), "--prefer-bytecode"], false);
    if (corrupt.includes("AOT:body-ran"))
      throw new Error("Invalid companion unexpectedly fell back to source");
    equal(
      resultLines(invoke(RUNNER, [join(tmp, "reject-entry.mjs")])),
      "AOT:body-ran",
      "ordinary loading still chooses source",
    );
    const oversized = join(tmp, "reject.gbc");
    // Sparse, so the regression doesn't allocate a 64 MiB fixture in the host.
    truncateSync(oversized, 64 * 1024 * 1024 + 1);
    const tooLarge = invoke(RUNNER, [join(tmp, "reject-entry.mjs"), "--prefer-bytecode"], false);
    if (!tooLarge.toLowerCase().includes("limit"))
      throw new Error(`Oversized companion must be refused before reading:\n${tooLarge}`);
    compile(join(tmp, "reject.mjs"));
    const denied = invoke(RUNNER, [
      join(tmp, "reject-entry.mjs"), "--prefer-bytecode", `--deny-read=${oversized}`,
    ], false);
    if (!denied.toLowerCase().includes("permission"))
      throw new Error(`Companion reads must honor explicit denies:\n${denied}`);

    const profileSource = join(tmp, "profile.js");
    writeFileSync(profileSource, `
      var readThis = function () {
        var target = {};
        Object.defineProperty(target, "fixed", { value: 12, writable: false });
        target.fixed = 99;
        return [this === globalThis, target.fixed];
      };
      console.log("AOT:" + JSON.stringify(readThis()));
      console.log("AOT:" + JSON.stringify(readThis.toString()));
    `);
    const profileFlags = ["--compat-var", "--compat-function", "--compat-non-strict-mode"];
    const profileArtifact = compile(profileSource, profileFlags);
    equal(
      resultLines(invoke(RUNNER, [profileArtifact, ...profileFlags])),
      resultLines(invoke(RUNNER, [profileSource, ...profileFlags])),
      "non-strict profile and StrictCode roundtrip",
    );
    const mismatch = invoke(RUNNER, [profileArtifact], false);
    if (!mismatch.toLowerCase().includes("profile") || resultLines(mismatch))
      throw new Error(`Profile mismatch must reject before execution:\n${mismatch}`);
    const goalMismatch = invoke(RUNNER, [profileArtifact, "--source-type=module", ...profileFlags], false);
    if (!goalMismatch.toLowerCase().includes("profile"))
      throw new Error(`Explicit source goal mismatch was not diagnosed:\n${goalMismatch}`);

    const derivedSource = join(tmp, "derived.js");
    writeFileSync(derivedSource, `
      class Base { constructor() { this.value = 40; } }
      class Derived extends Base {
        constructor() { super(); this.value += 2; }
      }
      console.log("AOT:" + new Derived().value);
    `);
    const derivedExpected = resultLines(invoke(RUNNER, [derivedSource]));
    equal(derivedExpected, "AOT:42", "derived constructor oracle");
    equal(
      resultLines(invoke(RUNNER, [compile(derivedSource)])),
      derivedExpected,
      "derived-this state survives serialization",
    );

    const sourceTarget = join(tmp, "source-target.mjs");
    writeFileSync(sourceTarget, 'globalThis.sourceTargetRan = true; export const answer = 42;');
    const sourceImporter = join(tmp, "source-importer.mjs");
    writeFileSync(sourceImporter, `
      import source target from "./source-target.mjs";
      console.log("AOT:" + Object.prototype.toString.call(target) + ":" + String(globalThis.sourceTargetRan));
    `);
    const sourceFlags = ["--experimental-js-module-source"];
    compile(sourceTarget, sourceFlags);
    const sourceArtifact = compile(sourceImporter, sourceFlags);
    const sourceExpected = resultLines(invoke(RUNNER, [sourceImporter, ...sourceFlags]));
    equal(sourceExpected, "AOT:[object ModuleSource]:undefined", "source-phase import oracle");
    unlinkSync(sourceImporter);
    equal(
      resultLines(invoke(RUNNER, [sourceArtifact, "--prefer-bytecode", ...sourceFlags])),
      sourceExpected,
      "compiled importer keeps source-phase semantics",
    );

    const largeSource = join(tmp, "large.mjs");
    writeFileSync(largeSource, Array.from({ length: 1000 }, (_, index) =>
      `export const function${index} = (value) => { const shifted = value + ${index}; return shifted * 3 + 1; };`,
    ).join("\n"));
    const largeEntry = join(tmp, "large-entry.mjs");
    writeFileSync(largeEntry, `
      import * as library from "./large.mjs";
      console.log("AOT:" + JSON.stringify([Object.keys(library).length, library.function0(2), library.function999(2)]));
    `);
    const largeExpected = resultLines(invoke(RUNNER, [largeEntry]));
    equal(largeExpected, "AOT:[1000,7,3004]", "large source module");
    compile(largeSource);
    const largeArtifact = compile(largeEntry);
    unlinkSync(largeSource);
    unlinkSync(largeEntry);
    equal(
      resultLines(invoke(RUNNER, [largeArtifact, "--prefer-bytecode"])),
      largeExpected,
      "large compiled module",
    );

    // The real parser which exposed dropped BigInt constant slots, plus nested
    // templates, classes, large constants and observable parse/stringify work.
    const smolSource = join(tmp, "smol-toml.mjs");
    const fixture = resolve("fixtures/bytecode/smol-toml/index.js");
    writeFileSync(smolSource, readFileSync(fixture, "utf8"));
    const smolEntry = join(tmp, "smol-entry.mjs");
    writeFileSync(smolEntry, `
      import { parse, stringify } from "./smol-toml.mjs";
      const document = parse('title = "AOT"\\ncount = 42\\n[owner]\\nname = "Ada"\\nactive = true\\nvalues = [1, 2, 3]\\n');
      console.log("AOT:" + JSON.stringify(document));
      console.log("AOT:" + JSON.stringify(parse(stringify(document))));
      console.log("AOT:" + String(parse("value = 9223372036854775807", { integersAsBigInt: true }).value));
    `);
    const smolExpected = resultLines(invoke(RUNNER, [smolEntry, ...smolFlags]));
    const expectedDocument = '{"title":"AOT","count":42,"owner":{"name":"Ada","active":true,"values":[1,2,3]}}';
    equal(smolExpected, [
      `AOT:${expectedDocument}`,
      `AOT:${expectedDocument}`,
      "AOT:9223372036854775807",
    ].join("\n"), "smol-toml source oracle");
    compile(smolSource, smolFlags);
    const smolArtifact = compile(smolEntry, smolFlags);
    unlinkSync(smolSource);
    unlinkSync(smolEntry);
    equal(
      resultLines(invoke(RUNNER, [smolArtifact, "--prefer-bytecode", ...smolFlags])),
      smolExpected,
      "smol-toml source/bytecode differential",
    );
  } finally {
    clean(tmp);
  }
}

if (import.meta.main) {
  verifyAotModules();
  console.log("AOT module differential tests passed.");
}
