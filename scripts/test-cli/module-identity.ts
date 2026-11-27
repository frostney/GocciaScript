import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, symlinkSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { BUNDLER, RUNNER, TESTRUNNER } from "./binaries";
import { clean, mkdtemp } from "./tmpdir";

export function verifyModuleIdentity() {
  console.log("Filesystem module aliases share a module record...");
  const tmp = mkdtemp("goccia-module-identity-");
  const project = join(tmp, "project");
  const real = join(project, "real");
  const directoryLink = process.platform === "win32" ? "junction" : "dir";

  function write(path: string, lines: string[]) {
    writeFileSync(join(project, path), lines.join("\n") + "\n");
  }

  function run(binary: string, ...args: string[]) {
    const proc = Bun.spawnSync([resolve(binary), ...args], { cwd: project, stdout: "pipe", stderr: "pipe" });
    const stdout = proc.stdout.toString();
    assert.equal(proc.exitCode, 0, `${binary} ${args.join(" ")}\n${stdout}${proc.stderr}`);
    return stdout.replaceAll("\r\n", "\n");
  }

  try {
    mkdirSync(join(real, "inner"), { recursive: true });
    mkdirSync(join(project, "node_modules"));
    symlinkSync(real, join(project, "linked"), directoryLink);
    symlinkSync(real, join(project, "node_modules", "identity-package"), directoryLink);
    write("real/package.json", ['{"type":"module","exports":"./dep.mjs"}']);
    write("import-map.json", ['{"imports":{"mapped":"./linked/dep.mjs"}}']);
    write("real/dep.mjs", [
      "export const identity = {};",
      'console.log("dependency evaluated");',
    ]);
    write("real/data.json", ['{"answer":42}']);
    write("real/invalid.mjs", ["export const = ;"]);

    write("main.mjs", [
      'import * as direct from "./real/dep.mjs";',
      'import * as linked from "./linked/dep.mjs";',
      'import * as dotted from "./real//./inner/../dep.mjs";',
      'import * as mapped from "mapped";',
      'import * as pkg from "identity-package";',
      'import data from "./real/data.json";',
      'import linkedData from "./linked/data.json";',
      'import typed from "./real/data.json" with { type: "json" };',
      'import linkedTyped from "./linked/data.json" with { type: "json" };',
      'const dynamic = await import("./linked/dep.mjs");',
      'const deferred = await import.defer("./real/dep.mjs");',
      'const linkedDeferred = await import.defer("./linked/dep.mjs");',
      'const source = await import.source("./real/dep.mjs");',
      'const linkedSource = await import.source("./linked/dep.mjs");',
      'const parseError = (path) => import(path).catch(error => error);',
      'const firstError = await parseError("./real/invalid.mjs");',
      'const linkedError = await parseError("./linked/invalid.mjs");',
      'console.log("module records match:", [linked, dotted, mapped, pkg, dynamic].every(ns => ns === direct));',
      'console.log("typed records match:", data === linkedData && typed === linkedTyped && data !== typed);',
      'console.log("phase records match:", source === linkedSource && deferred === linkedDeferred);',
      'console.log("failed records match:", firstError instanceof SyntaxError && firstError === linkedError);',
    ]);
    const output = run(
      RUNNER, "main.mjs", "--allow-read=.", "--allow-import=node_modules",
      "--import-map=import-map.json", "--experimental-js-module-source",
    );
    for (const label of ["module", "typed", "phase", "failed"]) {
      assert.ok(output.includes(`${label} records match: true\n`), output);
    }
    assert.equal(output.match(/^dependency evaluated$/gm)?.length, 1, output);

    const caseInsensitive = existsSync(join(real, "DEP.mjs"));
    if (!caseInsensitive) write("real/DEP.mjs", ["export const identity = {};"]);
    write("case.mjs", [
      'import * as upper from "./real/DEP.mjs";',
      'import * as lower from "./real/dep.mjs";',
      'console.log("case aliases match:", upper === lower);',
    ]);
    assert.ok(run(RUNNER, "case.mjs").includes(`case aliases match: ${caseInsensitive}\n`));

    if (process.platform !== "win32") {
      write("real/relative.mjs", [
        'export { identity } from "./dep.mjs";',
        'export const load = () => import("./dep.mjs");',
      ]);
      symlinkSync(join(real, "relative.mjs"), join(project, "file-link.mjs"));
      write("file-main.mjs", [
        'import * as alias from "./file-link.mjs";',
        'import * as direct from "./real/relative.mjs";',
        'console.log("file aliases match:", alias === direct && (await alias.load()).identity === direct.identity);',
      ]);
      assert.ok(run(RUNNER, "file-main.mjs").includes("file aliases match: true\n"));
    }

    write("real/self.mjs", [
      'import "../linked/self.mjs";',
      'console.log("entry evaluated");',
    ]);
    run(BUNDLER, "real/self.mjs");
    for (const entry of ["real/self.mjs", "linked/self.mjs", "real/self.gbc", "linked/self.gbc"]) {
      const result = run(RUNNER, entry);
      assert.equal(result.match(/^entry evaluated$/gm)?.length, 1, result);
    }
    write("real/source-map.jsx", ["export const element = <div />;"]);
    run(BUNDLER, "linked/source-map.jsx", "--source-map");
    const sourceMap = JSON.parse(readFileSync(join(real, "source-map.map"), "utf8"));
    assert.ok(sourceMap.sources.some((source: string) => source.endsWith("source-map.jsx")));

    write("real/entry-test.mjs", [
      'import "../linked/entry-test.mjs";',
      'globalThis.entryRuns = (globalThis.entryRuns ?? 0) + 1;',
      'test("evaluates once", () => expect(globalThis.entryRuns).toBe(1));',
    ]);
    run(TESTRUNNER, "-P", "real/entry-test.mjs", "--coverage", "--coverage-format=lcov", "--coverage-output=coverage.lcov");
    const coverage = readFileSync(join(project, "coverage.lcov"), "utf8");
    assert.equal(coverage.match(/^SF:.*entry-test\.mjs\r?$/gm)?.length, 1, coverage);

    write("real/host.mjs", [
      "globalThis.hostRuns = (globalThis.hostRuns ?? 0) + 1;",
      "export const hostValue = {};",
    ]);
    write("host-main.mjs", [
      'import { hostValue as linked } from "./linked/host.mjs";',
      'console.log("host and guest share a record:", linked === hostValue, globalThis.hostRuns);',
    ]);
    assert.ok(run(RUNNER, "host-main.mjs", "--globals", "real/host.mjs").includes("host and guest share a record: true 1\n"));

    const outside = join(tmp, "outside");
    mkdirSync(outside);
    writeFileSync(join(outside, "disallowed.mjs"), 'export default "outside";\n');
    symlinkSync(outside, join(project, "outside-link"), directoryLink);
    write("permissions.mjs", [
      'import "./real/dep.mjs";',
      'const cached = ["./linked", "dep.mjs"].join("/");',
      'const checked = await import(cached).catch(error => error.name);',
      'const outside = await import("./outside-link/disallowed.mjs").catch(error => error.name);',
      'console.log("read checks:", checked, outside);',
    ]);
    const checked = run(RUNNER, "permissions.mjs");
    assert.ok(checked.includes("read checks: PermissionDenied PermissionDenied\n"), checked);

    const overlay = JSON.stringify(join(outside, "disallowed.mjs"));
    write("virtual.mjs", [
      `import value from ${overlay};`,
      `import source from ${overlay} with { type: "text" };`,
      'console.log("virtual source:", value, source.includes("registered fixture"));',
    ]);
    const outputWithOverlay = run(
      RUNNER, "virtual.mjs", "--module", `${join(outside, "disallowed.mjs")}=export default "registered fixture";`,
    );
    assert.ok(outputWithOverlay.includes("virtual source: registered fixture true\n"), outputWithOverlay);
  } finally {
    clean(tmp);
  }
}
