#!/usr/bin/env bun
/** Byte identity and private-name isolation through the public bundler CLI. */
import { mkdirSync, readFileSync, unlinkSync, writeFileSync } from "fs";
import { join } from "path";
import { BUNDLER, RUNNER } from "./test-cli/binaries";
import { normalizeLineEndings } from "./test-cli/assertions";
import { clean, mkdtemp } from "./test-cli/tmpdir";

function invoke(binary: string, args: string[]): string {
  const result = Bun.spawnSync([binary, ...args], {
    stdout: "pipe", stderr: "pipe", timeout: 60_000,
  });
  const output = normalizeLineEndings(result.stdout.toString() + result.stderr.toString());
  if (result.exitCode !== 0 || result.signalCode)
    throw new Error(`${binary} ${args.join(" ")}: exit ${result.exitCode}\n${output}`);
  return output;
}

export function verifyBundlerReproducibility(bundler = BUNDLER): void {
  const tmp = mkdtemp("goccia-reproducible-");
  const failures: string[] = [];
  try {
    // Separate families make a regression in either allocator visible even
    // when the other family happens to get the same worker assignments.
    const families = {
      private: "class C { #value = 1; get() { return this.#value; } } console.log(new C().get());",
      computed: 'class C { ["field"] = 42; } console.log(new C().field);',
      eval: 'function outer() { eval("var x = 1;"); return () => x; } console.log(outer()());',
    };
    for (const [family, source] of Object.entries(families)) {
      const directory = join(tmp, family);
      mkdirSync(directory);
      const files = Array.from({ length: 16 }, (_, index) => join(directory, `m${index.toString().padStart(2, "0")}.js`));
      for (const file of files) writeFileSync(file, source);
      const flags = ["--compat-function", "--compat-var", "--compat-non-strict-mode"];
      let baseline: Buffer[] | undefined;
      for (const [round, jobs] of [1, 1, 4, 4, 4].entries()) {
        // Last round supplies explicit files in reverse order. Source paths,
        // contents and options stay fixed; scheduling and history may change.
        invoke(bundler, [...(round === 4 ? files.toReversed() : [directory]), `--jobs=${jobs}`, ...flags]);
        const artifacts = files.map((file) => readFileSync(file.replace(/\.js$/, ".gbc")));
        if (artifacts.some((bytes) => bytes.length === 0)) throw new Error(`${family}: empty artifact`);
        if (!baseline) baseline = artifacts;
        else {
          const expected = baseline;
          const different = artifacts.filter((bytes, index) => !bytes.equals(expected[index] ?? Buffer.alloc(0))).length;
          console.log(`  ${family}, round ${round}, jobs=${jobs}: ${different}/${files.length} artifacts differ`);
          if (different) failures.push(`${family}, round ${round}, jobs=${jobs}: ${different}/${files.length} differ`);
        }
      }
    }
    if (failures.length) throw new Error(`Bytecode is not reproducible:\n${failures.join("\n")}`);
  } finally {
    clean(tmp);
  }
}

export function verifyBundlerPrivateIsolation(bundler = BUNDLER, runner = RUNNER): void {
  const tmp = mkdtemp("goccia-private-identity-");
  try {
    // Each module is compiled in a separate process, so their first class
    // receives the same deterministic label. Runtime storage must be fresh.
    writeFileSync(join(tmp, "base.mjs"), `
      export class Base { #value = 1; getBase() { return this.#value; } setBase(n) { this.#value = n; } }
    `);
    writeFileSync(join(tmp, "derived.mjs"), `
      import { Base } from "./base.mjs";
      export class Derived extends Base { #value = 2; getDerived() { return this.#value; } }
    `);
    writeFileSync(join(tmp, "entry.mjs"), `
      import { Derived } from "./derived.mjs";
      const value = new Derived();
      console.log("PRIVATE:" + [value.getBase(), value.getDerived()].join());
      value.setBase(3);
      console.log("PRIVATE:" + [value.getBase(), value.getDerived()].join());
      const make = (Base, n) => class extends Base { #x = n; get() { return this.#x; } };
      const A = make(class {}, 1);
      const B = make(A, 2);
      const child = new B();
      console.log("PRIVATE:" + [A.prototype.get.call(child), child.get()].join());
    `);
    for (const name of ["base", "derived", "entry"]) {
      const file = join(tmp, `${name}.mjs`);
      invoke(bundler, [file]);
      unlinkSync(file);
    }
    const output = invoke(runner, [join(tmp, "entry.gbc"), "--prefer-bytecode"]);
    const actual = output.split("\n").filter((line) => line.startsWith("PRIVATE:")).join("\n");
    const expected = "PRIVATE:1,2\nPRIVATE:3,2\nPRIVATE:1,2";
    if (actual !== expected) throw new Error(`Private storage isolation\nExpected: ${expected}\nActual: ${actual}`);
  } finally {
    clean(tmp);
  }
}

if (import.meta.main) {
  const failures: unknown[] = [];
  for (const verify of [verifyBundlerReproducibility, verifyBundlerPrivateIsolation]) {
    try { verify(); } catch (error) { failures.push(error); console.error(error); }
  }
  if (failures.length) process.exit(1);
}
