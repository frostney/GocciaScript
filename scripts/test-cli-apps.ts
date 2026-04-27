#!/usr/bin/env bun
/**
 * test-cli-apps.ts
 *
 * App-specific features: GocciaScriptLoader (JSON output, --global/--globals,
 * coverage, source maps), GocciaBundler (compile, roundtrip, stdin, directory,
 * .gbc rejection, source maps), GocciaBenchmarkRunner (file, stdin, bytecode),
 * GocciaREPL (banner, evaluation, ASI, error recovery, bytecode).
 */

import { $ } from "bun";
import {
  mkdtempSync,
  writeFileSync,
  readFileSync,
  existsSync,
  rmSync,
  mkdirSync,
} from "fs";
import { join, resolve } from "path";
import { tmpdir } from "os";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;
const REPL = `./build/GocciaREPL${ext}`;
const TESTRUNNER = `./build/GocciaTestRunner${ext}`;
const BUNDLER = `./build/GocciaBundler${ext}`;
const BENCHRUNNER = `./build/GocciaBenchmarkRunner${ext}`;

const makeTmp = () => mkdtempSync(join(tmpdir(), "goccia-apps-"));
const clean = (d: string) => rmSync(d, { recursive: true, force: true });

function assertValidSourceMap(path: string): void {
  const raw = readFileSync(path, "utf-8");
  const map = JSON.parse(raw);
  if (map.version !== 3) throw new Error(`Source map version should be 3, got ${map.version}`);
  if (!Array.isArray(map.sources) || map.sources.length === 0) throw new Error("Source map should have non-empty sources");
  if (typeof map.mappings !== "string" || map.mappings.length === 0) throw new Error("Source map should have non-empty mappings");
}

// ============================================================================
// GocciaScriptLoader
// ============================================================================

// -- JSON output (interpreted + bytecode) ---------------------------------------

console.log("Loader: JSON output (interpreted)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("console.log('hi'); 2 + 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== true) throw new Error(`JSON ok should be true, got ${json.ok}`);
  if (json.value !== 4) throw new Error(`JSON value should be 4, got ${json.value}`);
  if (!json.output?.includes("hi")) throw new Error(`JSON output should contain "hi"`);
}

console.log("Loader: JSON output (bytecode)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("console.log('hi'); 2 + 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== true) throw new Error(`Bytecode JSON ok should be true, got ${json.ok}`);
  if (json.value !== 4) throw new Error(`Bytecode JSON value should be 4, got ${json.value}`);
  if (!json.output?.includes("hi")) throw new Error(`Bytecode JSON output should contain "hi"`);
}

// -- --global / --globals -------------------------------------------------------

console.log("Loader: --global flag...");
{
  const proc = Bun.spawnSync([LOADER, "--global", "x=10", "--global", "y=20", "--output=json"], {
    stdin: new TextEncoder().encode("x + y;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.value !== 30) throw new Error(`--global x+y should be 30, got ${json.value}`);
}

console.log("Loader: --globals file...");
{
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const proc = Bun.spawnSync([LOADER, `--globals=${globalsPath}`, "--output=json", "--mode=bytecode"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
    if (json.value !== "goccia") throw new Error(`--globals should set name to "goccia", got ${json.value}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --global overrides --globals file...");
{
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const proc = Bun.spawnSync([LOADER, `--globals=${globalsPath}`, "--global", "name=override", "--output=json"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
    if (json.value !== "override") throw new Error(`--global should override --globals, got ${json.value}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --globals from JS module...");
{
  const tmp = makeTmp();
  try {
    const moduleJsPath = join(tmp, "module.js");
    writeFileSync(moduleJsPath, 'export const name = "module-value";\n');
    const proc = Bun.spawnSync([LOADER, `--globals=${moduleJsPath}`, "--output=json"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
    if (json.value !== "module-value") throw new Error(`--globals JS module should set name, got ${json.value}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --global cannot override built-in...");
{
  const res = await $`echo '1;' | ${LOADER} --global console=1 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Overriding built-in should fail");
  if (!res.text().includes("Cannot override built-in global")) throw new Error("Should mention 'Cannot override built-in global'");
}

// -- Coverage -------------------------------------------------------------------

console.log("Loader: coverage summary...");
{
  const out = await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage 2>&1`.text();
  if (!out.includes("Coverage Summary:")) throw new Error(`Expected "Coverage Summary:", got: ${out}`);
}

console.log("Loader: coverage --output=json not corrupted...");
{
  const proc = Bun.spawnSync([LOADER, "--coverage", "--output=json"], {
    stdin: new TextEncoder().encode("const x = 1 + 2;\nx;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const firstLine = proc.stdout.toString().trimStart().split("\n")[0];
  if (!firstLine.startsWith("{")) throw new Error(`Coverage --output=json first line should start with "{", got: ${firstLine}`);
}

{
  const tmp = makeTmp();
  try {
    console.log("Loader: coverage LCOV...");
    const lcovPath = join(tmp, "coverage.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-format=lcov --coverage-output=${lcovPath}`.quiet();
    if (!existsSync(lcovPath)) throw new Error("LCOV file should exist");
    const lcov = readFileSync(lcovPath, "utf-8");
    if (!lcov.includes("SF:")) throw new Error('LCOV should contain "SF:"');
    if (!lcov.includes("DA:")) throw new Error('LCOV should contain "DA:"');

    console.log("Loader: coverage JSON...");
    const jsonCovPath = join(tmp, "coverage.json");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-format=json --coverage-output=${jsonCovPath}`.quiet();
    if (!existsSync(jsonCovPath)) throw new Error("JSON coverage file should exist");
    const jsonCov = readFileSync(jsonCovPath, "utf-8");
    if (!jsonCov.includes('"path":')) throw new Error('JSON coverage should contain "path":');

    console.log("Loader: coverage order-independent flags...");
    const orderPath = join(tmp, "order.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-output=${orderPath} --coverage-format=lcov`.quiet();
    if (!existsSync(orderPath)) throw new Error("Order-independent LCOV should exist");
    if (!readFileSync(orderPath, "utf-8").includes("SF:")) throw new Error("Order-independent LCOV should contain SF:");

    console.log("Loader: coverage bytecode...");
    const bcLcovPath = join(tmp, "bc-coverage.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --mode=bytecode --coverage-format=lcov --coverage-output=${bcLcovPath}`.quiet();
    if (!existsSync(bcLcovPath)) throw new Error("Bytecode LCOV should exist");
    if (!readFileSync(bcLcovPath, "utf-8").includes("DA:")) throw new Error("Bytecode LCOV should contain DA:");

    console.log("Loader: branch coverage via TestRunner...");
    const branchLcovPath = join(tmp, "branch.lcov");
    await $`${TESTRUNNER} --coverage --coverage-format=lcov --coverage-output=${branchLcovPath} --no-progress tests/language/statements/if/if-else-statements.js`.quiet();
    const branchLcov = readFileSync(branchLcovPath, "utf-8");
    if (!branchLcov.includes("BRDA:")) throw new Error('Branch LCOV should contain "BRDA:"');
    if (!branchLcov.includes("BRF:")) throw new Error('Branch LCOV should contain "BRF:"');
    if (!branchLcov.includes("BRH:")) throw new Error('Branch LCOV should contain "BRH:"');

    const branchJsonPath = join(tmp, "branch.json");
    await $`${TESTRUNNER} --coverage --coverage-format=json --coverage-output=${branchJsonPath} --no-progress tests/language/statements/if/if-else-statements.js`.quiet();
    const branchJson = readFileSync(branchJsonPath, "utf-8");
    if (!branchJson.includes('"branchMap":')) throw new Error('Branch JSON should contain "branchMap":');
    if (!branchJson.includes('"b":')) throw new Error('Branch JSON should contain "b":');

    console.log("Loader: JSX coverage source-map translation...");
    const jsxPath = join(tmp, "coverage-test.jsx");
    writeFileSync(
      jsxPath,
      [
        "const createElement = (t, p, ...c) => ({ t, p, c });",
        "const Greet = (props) => {",
        '  const msg = props.name ? props.name : "world";',
        "  return <div>{msg}</div>;",
        "};",
        'Greet({ name: "hi" });',
        "Greet({});",
        "",
      ].join("\n"),
    );

    const jsxLcovPath = join(tmp, "jsx-coverage.lcov");
    await $`${LOADER} --coverage --coverage-format=lcov --coverage-output=${jsxLcovPath} ${jsxPath}`.quiet();
    if (!readFileSync(jsxLcovPath, "utf-8").includes("BRDA:3,")) throw new Error("JSX LCOV should have branch on line 3");

    const jsxJsonPath = join(tmp, "jsx-coverage.json");
    await $`${LOADER} --coverage --coverage-format=json --coverage-output=${jsxJsonPath} ${jsxPath}`.quiet();
    if (!readFileSync(jsxJsonPath, "utf-8").includes('"line":3')) throw new Error('JSX JSON should have "line":3');
  } finally {
    clean(tmp);
  }
}

// -- Source maps (Loader) -------------------------------------------------------

{
  const tmp = makeTmp();
  try {
    const jsxSource = [
      "const createElement = (t, p, ...c) => ({ t, p, c });",
      'const el = <div id="test">hello</div>;',
      "el;",
      "",
    ].join("\n");

    console.log("Loader: source map bytecode...");
    const jsxPath = join(tmp, "test.jsx");
    writeFileSync(jsxPath, jsxSource);
    await $`${LOADER} --source-map --mode=bytecode ${jsxPath}`.quiet();
    const mapPath = jsxPath.replace(/\.jsx$/, ".jsx.map");
    if (!existsSync(mapPath)) throw new Error("Source map should exist");
    assertValidSourceMap(mapPath);

    console.log("Loader: source map custom path...");
    const customMapPath = join(tmp, "custom.map");
    await $`${LOADER} --source-map=${customMapPath} --mode=bytecode ${jsxPath}`.quiet();
    if (!existsSync(customMapPath)) throw new Error("Custom source map should exist");
    assertValidSourceMap(customMapPath);

    console.log("Loader: source map interpreted...");
    const interpJsxPath = join(tmp, "interp.jsx");
    writeFileSync(interpJsxPath, jsxSource);
    await $`${LOADER} --source-map ${interpJsxPath}`.quiet();
    const interpMapPath = interpJsxPath.replace(/\.jsx$/, ".jsx.map");
    if (!existsSync(interpMapPath)) throw new Error("Interpreted source map should exist");
    assertValidSourceMap(interpMapPath);

    console.log("Loader: no --source-map -> no .map...");
    const noMapJsxPath = join(tmp, "nomap.jsx");
    writeFileSync(noMapJsxPath, jsxSource);
    await $`${LOADER} ${noMapJsxPath}`.quiet();
    const noMapPath = noMapJsxPath.replace(/\.jsx$/, ".jsx.map");
    if (existsSync(noMapPath)) throw new Error("No .map file should exist without --source-map");

    console.log("Loader: stdin --source-map rejection...");
    const stdinRes = await $`echo 'const x = 1;' | ${LOADER} --source-map 2>&1`.nothrow();
    const stdinOut = stdinRes.text().toLowerCase();
    if (!stdinOut.includes("error") && !stdinOut.includes("cannot") && !stdinOut.includes("require")) {
      throw new Error(`Stdin --source-map should produce an error, got: ${stdinRes.text()}`);
    }
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaBundler
// ============================================================================

{
  const tmp = makeTmp();
  try {
    console.log("Bundler: single file compile + roundtrip...");
    const singleFile = join(tmp, "single.js");
    writeFileSync(singleFile, "const x = 2 + 2;\nx;\n");
    const singleOut = await $`${BUNDLER} ${singleFile} 2>&1`.text();
    const singleGbc = singleFile.replace(/\.js$/, ".gbc");
    if (!existsSync(singleGbc)) throw new Error(".gbc should exist");
    if (!singleOut.includes("Compiled to")) throw new Error('Output should contain "Compiled to"');

    // Roundtrip
    const roundtripOut = await $`${LOADER} ${singleGbc} 2>&1`.text();
    if (!roundtripOut.includes("Result: 4")) throw new Error(`Roundtrip should produce Result: 4, got: ${roundtripOut}`);

    console.log("Bundler: custom --output path...");
    const customOut = join(tmp, "custom.gbc");
    const customSrc = join(tmp, "custom.js");
    writeFileSync(customSrc, "const y = 3 + 3;\ny;\n");
    await $`${BUNDLER} ${customSrc} --output=${customOut}`.quiet();
    if (!existsSync(customOut)) throw new Error("Custom --output .gbc should exist");

    console.log("Bundler: stdin compile with --output...");
    const stdinOut = join(tmp, "stdin.gbc");
    await $`echo 'const z = 5 + 5; z;' | ${BUNDLER} --output=${stdinOut}`.quiet();
    if (!existsSync(stdinOut)) throw new Error("Stdin --output .gbc should exist");
    const stdinRoundtrip = await $`${LOADER} ${stdinOut} 2>&1`.text();
    if (!stdinRoundtrip.includes("Result: 10")) throw new Error(`Stdin roundtrip should produce Result: 10, got: ${stdinRoundtrip}`);

    console.log("Bundler: stdin without --output should fail...");
    const stdinNoOutput = await $`echo '1 + 1;' | ${BUNDLER} 2>&1`.nothrow();
    if (stdinNoOutput.exitCode === 0) throw new Error("Stdin without --output should exit non-zero");

    console.log("Bundler: directory compile...");
    const dirSrc = join(tmp, "dir-src");
    mkdirSync(dirSrc);
    writeFileSync(join(dirSrc, "a.js"), "1 + 1;\n");
    writeFileSync(join(dirSrc, "b.js"), "2 + 2;\n");
    await $`${BUNDLER} ${dirSrc}`.quiet();
    if (!existsSync(join(dirSrc, "a.gbc"))) throw new Error("Directory compile should create a.gbc");
    if (!existsSync(join(dirSrc, "b.gbc"))) throw new Error("Directory compile should create b.gbc");

    console.log("Bundler: multiple files...");
    const multiA = join(tmp, "multi-a.js");
    const multiB = join(tmp, "multi-b.js");
    writeFileSync(multiA, "10 + 10;\n");
    writeFileSync(multiB, "20 + 20;\n");
    await $`${BUNDLER} ${multiA} ${multiB}`.quiet();
    if (!existsSync(join(tmp, "multi-a.gbc"))) throw new Error("multi-a.gbc should exist");
    if (!existsSync(join(tmp, "multi-b.gbc"))) throw new Error("multi-b.gbc should exist");

    console.log("Bundler: .gbc rejection...");
    const gbcInput = join(tmp, "reject.gbc");
    writeFileSync(gbcInput, "not real bytecode");
    const gbcReject = await $`${BUNDLER} ${gbcInput} 2>&1`.nothrow();
    if (gbcReject.exitCode === 0) throw new Error(".gbc input should be rejected");

    // -- Bundler source maps --

    const jsxSource = [
      "const createElement = (t, p, ...c) => ({ t, p, c });",
      'const el = <div id="test">hello</div>;',
      "el;",
      "",
    ].join("\n");

    console.log("Bundler: --source-map flag...");
    const smSrc = join(tmp, "sm.jsx");
    writeFileSync(smSrc, jsxSource);
    await $`${BUNDLER} ${smSrc} --source-map`.quiet();
    const smMap = join(tmp, "sm.jsx.map");
    if (!existsSync(join(tmp, "sm.gbc"))) throw new Error("--source-map: .gbc should exist");
    if (!existsSync(smMap)) throw new Error("--source-map: .map should exist");
    assertValidSourceMap(smMap);

    console.log("Bundler: --source-map=<custom path>...");
    const smCustomSrc = join(tmp, "sm-custom.jsx");
    const smCustomMap = join(tmp, "custom-output.map");
    writeFileSync(smCustomSrc, jsxSource);
    await $`${BUNDLER} ${smCustomSrc} --source-map=${smCustomMap}`.quiet();
    if (!existsSync(smCustomMap)) throw new Error("Custom map should exist");
    assertValidSourceMap(smCustomMap);

    console.log("Bundler: no --source-map -> no .map...");
    const noSmSrc = join(tmp, "no-sm.jsx");
    writeFileSync(noSmSrc, jsxSource);
    await $`${BUNDLER} ${noSmSrc}`.quiet();
    if (existsSync(join(tmp, "no-sm.jsx.map"))) throw new Error("No .map file should exist without --source-map");

    console.log("Bundler: stdin --source-map --output...");
    const stdinSmOut = join(tmp, "stdin-sm.gbc");
    await $`echo ${jsxSource} | ${BUNDLER} --source-map --output=${stdinSmOut}`.quiet();
    const stdinSmMap = stdinSmOut + ".map";
    if (!existsSync(stdinSmOut)) throw new Error("Stdin --source-map: .gbc should exist");
    if (!existsSync(stdinSmMap)) throw new Error("Stdin --source-map: .gbc.map should exist");
    assertValidSourceMap(stdinSmMap);
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaBenchmarkRunner
// ============================================================================

{
  const tmp = makeTmp();
  const benchEnv = {
    ...process.env,
    GOCCIA_BENCH_CALIBRATION_MS: "50",
    GOCCIA_BENCH_ROUNDS: "3",
  } as Record<string, string>;

  try {
    const stdinSource = 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n';

    console.log("BenchmarkRunner: file benchmark (interpreted)...");
    const fileOut = join(tmp, "file-interp.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--no-progress", "--format=json", `--output=${fileOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`File benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileJson = readFileSync(fileOut, "utf-8");
    if (!fileJson.includes('"file":')) throw new Error('File JSON should contain "file":');
    if (!fileJson.includes('"totalBenchmarks":')) throw new Error('File JSON should contain "totalBenchmarks":');

    console.log("BenchmarkRunner: file benchmark (bytecode)...");
    const fileBcOut = join(tmp, "file-bc.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--no-progress", "--format=json", `--output=${fileBcOut}`, "--mode=bytecode"],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode file benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileBcJson = readFileSync(fileBcOut, "utf-8");
    if (!fileBcJson.includes('"file":')) throw new Error('Bytecode file JSON should contain "file":');

    console.log("BenchmarkRunner: file benchmark JSON output...");
    if (!fileJson.includes('"totalBenchmarks":')) throw new Error('JSON should contain totalBenchmarks');

    console.log("BenchmarkRunner: stdin benchmark (interpreted)...");
    const stdinOutPath = join(tmp, "stdin-interp.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${stdinOutPath}`],
        {
          stdin: new TextEncoder().encode(stdinSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Stdin benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const stdinJson = readFileSync(stdinOutPath, "utf-8");
    if (!stdinJson.includes('"name": "sum"')) throw new Error('Stdin JSON should contain "name": "sum"');
    if (!stdinJson.includes('"totalBenchmarks": 1')) throw new Error('Stdin JSON should contain totalBenchmarks: 1');

    console.log("BenchmarkRunner: stdin benchmark (bytecode)...");
    const stdinBcOutPath = join(tmp, "stdin-bc.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${stdinBcOutPath}`, "--mode=bytecode"],
        {
          stdin: new TextEncoder().encode(stdinSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode stdin benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const stdinBcJson = readFileSync(stdinBcOutPath, "utf-8");
    if (!stdinBcJson.includes('"name": "sum"')) throw new Error('Bytecode stdin JSON should contain "name": "sum"');
    if (!stdinBcJson.includes('"totalBenchmarks": 1')) throw new Error('Bytecode stdin JSON should contain totalBenchmarks: 1');
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaREPL
// ============================================================================

console.log("REPL: banner (interpreted)...");
{
  const out = await $`echo '' | ${REPL} 2>&1`.text();
  if (!out.includes("Goccia REPL")) throw new Error(`Banner should contain "Goccia REPL", got: ${out.slice(0, 200)}`);
  if (!out.includes("(interpreted)")) throw new Error(`Banner should contain "(interpreted)", got: ${out.slice(0, 200)}`);
}

console.log("REPL: banner (bytecode)...");
{
  const out = await $`echo '' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("(bytecode)")) throw new Error(`Bytecode banner should contain "(bytecode)", got: ${out.slice(0, 200)}`);
}

console.log("REPL: expression evaluation...");
{
  const out = await $`echo '2 + 2;' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Expression 2+2 should produce 4, got: ${out}`);
}

console.log("REPL: ASI mode...");
{
  const out = await $`printf 'const x = 5\nx\n' | ${REPL} --asi 2>&1`.text();
  if (!out.includes("5")) throw new Error(`ASI mode should produce 5, got: ${out}`);
}

console.log("REPL: error recovery...");
{
  const out = await $`printf 'const x = ;\n2 + 2;\n' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`After error, second expression should produce 4, got: ${out}`);
}

console.log("REPL: bytecode evaluation...");
{
  const out = await $`echo '2 + 2;' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Bytecode 2+2 should produce 4, got: ${out}`);
}

// ============================================================================
// --allowed-host flag
// ============================================================================

console.log("Loader: --allowed-host blocks unlisted host...");
{
  const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail");
  if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
}

console.log("Loader: no --allowed-host blocks all fetch...");
{
  const res = await $`echo 'fetch("http://example.com");' | ${LOADER} 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch without --allowed-host should fail");
  if (!res.text().includes("allowed hosts")) throw new Error(`Error should mention allowed hosts, got: ${res.text()}`);
}

console.log("Loader: --allowed-host multiple hosts...");
{
  // Both hosts in the list; blocked.test is not
  const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com --allowed-host=other.com 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail with multiple --allowed-host");
  if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
}

console.log("Loader: HTTPS fetch smoke with --allowed-host...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--asi", "--allowed-host=www.gstatic.com"], {
    stdin: new TextEncoder().encode(
      'const response = await fetch("https://www.gstatic.com/generate_204", { method: "HEAD" });\nresponse.status;\n',
    ),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`HTTPS fetch should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== true) throw new Error(`HTTPS fetch JSON ok should be true, got ${json.ok}`);
  if (json.value !== 204) throw new Error(`HTTPS fetch status should be 204, got ${json.value}`);
}

console.log("\nAll test-cli-apps.ts tests passed.");
