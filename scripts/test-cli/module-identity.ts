import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, renameSync, symlinkSync, writeFileSync } from "node:fs";
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
    const output = `${proc.stdout}${proc.stderr}`.replaceAll("\r\n", "\n");
    assert.equal(proc.exitCode, 0, `${binary} ${args.join(" ")}\n${output}`);
    return output;
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

    // Names that differ only in non-ASCII case reach one record on a case-insensitive volume, as ASCII ones do.
    write("real/\u00c4rger.mjs", ['console.log("non-ASCII case evaluated");', "export const identity = {};"]);
    const unicodeCaseInsensitive = existsSync(join(real, "\u00e4rger.mjs"));
    write("unicode-case.mjs", [
      'import * as upper from "./real/\u00c4rger.mjs";',
      `import * as lower from "./real/${unicodeCaseInsensitive ? "\u00e4" : "\u00c4"}rger.mjs";`,
      'console.log("non-ASCII case aliases match:", upper === lower);',
    ]);
    const unicodeCaseOutput = run(RUNNER, "unicode-case.mjs");
    assert.ok(unicodeCaseOutput.includes("non-ASCII case aliases match: true\n"), unicodeCaseOutput);
    assert.equal(unicodeCaseOutput.match(/^non-ASCII case evaluated$/gm)?.length, 1, unicodeCaseOutput);

    // import.meta names the real file, whichever spelling loaded the module first, and resolves from its directory.
    write("real/meta.mjs", [
      "export const url = import.meta.url;",
      'export const resolved = import.meta.resolve("./sibling.mjs");',
    ]);
    const metaOf = (first: string, second: string) => {
      write("meta-order.mjs", [
        `import * as first from "./${first}/meta.mjs";`,
        `import * as second from "./${second}/meta.mjs";`,
        'console.log("meta:", first === second, first.url, first.resolved);',
      ]);
      const line = run(RUNNER, "meta-order.mjs").split("\n").find(text => text.startsWith("meta: "));
      assert.ok(line, `no import.meta line for ${first} then ${second}`);
      return line;
    };
    const linkFirst = metaOf("linked", "real");
    assert.equal(linkFirst, metaOf("real", "linked"));
    const [, sameRecord, metaUrl, metaResolved] = linkFirst.split(" ");
    assert.equal(sameRecord, "true", linkFirst);
    assert.match(metaUrl, /^file:\/\/.*\/real\/meta\.mjs$/, linkFirst);
    assert.equal(metaResolved, metaUrl.replace(/meta\.mjs$/, "sibling.mjs"), linkFirst);

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

      // A symlinked file resolves its imports, import.meta and its Script entry's imports from the real file's
      // directory, as Node does: each directory has its own sib.mjs.
      mkdirSync(join(project, "fa"));
      mkdirSync(join(project, "fb"));
      write("fa/sib.mjs", ['export const who = "link directory";']);
      write("fb/sib.mjs", ['export const who = "real directory";']);
      write("fb/lib.mjs", [
        'import { who } from "./sib.mjs";',
        'const dynamic = await import("./sib.mjs");',
        'const realm = await new ShadowRealm().importValue("./sib.mjs", "who");',
        'console.log("lib imports:", who, "|", dynamic.who, "|", realm);',
        'console.log("lib meta:", import.meta.url, import.meta.resolve("./sib.mjs"));',
      ]);
      symlinkSync(join(project, "fb", "lib.mjs"), join(project, "fa", "lib.mjs"));
      write("lib-main.mjs", ['import "./fa/lib.mjs";']);
      write("virtual-lib-main.mjs", ['import "./fa/vlib.mjs";']);
      const libOutput = run(RUNNER, "lib-main.mjs", "--unsafe-shadowrealm");
      assert.ok(libOutput.includes("lib imports: real directory | real directory | real directory\n"), libOutput);
      assert.match(libOutput, /^lib meta: file:\/\/\S*\/fb\/lib\.mjs file:\/\/\S*\/fb\/sib\.mjs$/m, libOutput);

      // A virtual module whose address is a symlink to another file resolves all three from its own address.
      write("fb/vsib.mjs", ['export const who = "disk target";']);
      write("fb/vlib.mjs", ['export const who = "disk module";']);
      symlinkSync(join(project, "fb", "vlib.mjs"), join(project, "fa", "vlib.mjs"));
      const virtualLib = join(project, "fa", "vlib.mjs");
      const virtualOutput = run(
        RUNNER, "virtual-lib-main.mjs", "--unsafe-shadowrealm",
        "--module", `${virtualLib}=${[
          'import { who } from "./vsib.mjs";',
          'const dynamic = await import("./vsib.mjs");',
          'const realm = await new ShadowRealm().importValue("./vsib.mjs", "who");',
          'console.log("virtual imports:", who, "|", dynamic.who, "|", realm);',
        ].join(" ")}`,
        "--module", `${join(project, "fa", "vsib.mjs")}=export const who = "configured";`,
      );
      assert.ok(virtualOutput.includes("virtual imports: configured | configured | configured\n"), virtualOutput);

      write("fb/script.js", [
        'import { who } from "./sib.mjs";',
        'console.log("script static:", who);',
        'import("./sib.mjs").then(m => console.log("script dynamic:", m.who));',
      ]);
      symlinkSync(join(project, "fb", "script.js"), join(project, "fa", "script.js"));
      const scriptOutput = run(RUNNER, "fa/script.js", "--source-type=script", "--allow-read=.");
      assert.ok(scriptOutput.includes("script static: real directory\n"), scriptOutput);
      assert.ok(scriptOutput.includes("script dynamic: real directory\n"), scriptOutput);

      // A Script compiled to bytecode whose source is gone resolves from the real bytecode file's directory.
      run(BUNDLER, "fb/script.js", "--source-type=script");
      renameSync(join(project, "fb", "script.gbc"), join(project, "fb", "standalone.gbc"));
      symlinkSync(join(project, "fb", "standalone.gbc"), join(project, "fa", "standalone.gbc"));
      const standaloneOutput = run(RUNNER, "fa/standalone.gbc", "--allow-read=.");
      assert.ok(standaloneOutput.includes("script static: real directory\n"), standaloneOutput);
      assert.ok(standaloneOutput.includes("script dynamic: real directory\n"), standaloneOutput);

      write("real/asset.payload", ['{"answer":42}']);
      for (const alias of ["asset.json", "asset-alias.json", "asset.txt"]) {
        symlinkSync(join(real, "asset.payload"), join(project, alias));
      }
      write("asset-main.mjs", [
        'import * as json from "./asset.json";',
        'import * as alias from "./asset-alias.json";',
        'import text from "./asset.txt";',
        'console.log("asset aliases match:", json === alias && json.answer === 42 && JSON.parse(text).answer === 42);',
      ]);
      const assetOutput = run(RUNNER, "asset-main.mjs");
      assert.ok(assetOutput.includes("asset aliases match: true\n"), assetOutput);

      run(BUNDLER, "file-link.mjs");
      write("artifact-main.mjs", [
        'import * as compiled from "./file-link.gbc";',
        'import * as direct from "./real/relative.mjs";',
        'console.log("artifact and source match:", compiled === direct && (await compiled.load()).identity === direct.identity);',
      ]);
      const artifactOutput = run(RUNNER, "artifact-main.mjs");
      assert.ok(artifactOutput.includes("artifact and source match: true\n"), artifactOutput);

      write("real/artifact-self.mjs", [
        'import "./artifact-self.mjs";',
        'console.log("file-linked entry evaluated");',
      ]);
      symlinkSync(join(real, "artifact-self.mjs"), join(project, "artifact-self-link.mjs"));
      run(BUNDLER, "artifact-self-link.mjs");
      const artifactEntryOutput = run(RUNNER, "artifact-self-link.gbc");
      assert.equal(artifactEntryOutput.match(/^file-linked entry evaluated$/gm)?.length, 1, artifactEntryOutput);

      write("real/payload.mjs", ['export const label = "bytecode payload";']);
      run(BUNDLER, "real/payload.mjs");
      renameSync(join(real, "payload.gbc"), join(real, "payload.bin"));
      symlinkSync(join(real, "payload.bin"), join(project, "payload-link.gbc"));
      write("artifact-link-main.mjs", [
        'import { label } from "./payload-link.gbc";',
        'console.log(label);',
      ]);
      const artifactLinkOutput = run(RUNNER, "artifact-link-main.mjs");
      assert.match(artifactLinkOutput, /^bytecode payload$/m);
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

    const snapshotPath = join(real, "snapshot-state.mjs");
    write("real/snapshot-state.mjs", ["export const read = () => 'disk';"]);
    write("overlay-snapshot.mjs", [
      `import { read } from ${JSON.stringify(snapshotPath)};`,
      'test("round trips a registered module closure", () => {',
      '  expect(read()).toBe("overlay");',
      '  const observed = { read };',
      '  expect(observed).toBe(observed);',
      '});',
    ]);
    const snapshotOutput = run(
      TESTRUNNER, "-P", "overlay-snapshot.mjs", "--heap-snapshots", "--no-progress",
      "--module", `${snapshotPath}=export const read = () => 'overlay';`,
    );
    assert.match(snapshotOutput, /Heap snapshots: observations=1 objects=1 passed=1 refused=0 failed=0/);
  } finally {
    clean(tmp);
  }
}
