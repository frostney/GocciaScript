#!/usr/bin/env bun
/**
 * test-cli-permissions.ts
 *
 * The ADR 0122 command-line surface across the Goccia binaries: the
 * --allow-<cap> / --deny-<cap> grammar, the removed flags and config keys and
 * their replacements, what each binary honors, the deny-by-default profile,
 * config permissions blocks and the trust they need, and the --max-* limits
 * with units.
 *
 * Every run gets a private HOME (and no XDG_CONFIG_HOME or APPDATA), so the
 * per-user trust store is never the developer's own; trust tests also name a
 * store explicitly with --trust-store.
 */

import { cpSync, existsSync, mkdirSync, readFileSync, realpathSync, rmSync, symlinkSync, writeFileSync } from "fs";
import { join, resolve } from "path";
import {
  BARE,
  BENCHRUNNER,
  BUNDLER,
  LOADER,
  REPL,
  SANDBOXRUNNER,
  TEST262RUNNER,
  TESTRUNNER,
  WASMTESTRUNNER,
} from "./test-cli/binaries";
import { makeTmpFactory, clean } from "./test-cli/tmpdir";

const makeTmp = makeTmpFactory("goccia-permissions-");
const isWindows = process.platform === "win32";

type RunResult = { exitCode: number; stdout: string; stderr: string; combined: string };

const privateHome = makeTmp();
const isolatedEnv: Record<string, string> = {};
for (const [key, value] of Object.entries(process.env))
  if (value !== undefined && key !== "XDG_CONFIG_HOME" && key !== "APPDATA") isolatedEnv[key] = value;
isolatedEnv.HOME = privateHome;
isolatedEnv.USERPROFILE = privateHome;

function run(
  binary: string,
  args: string[],
  options: { cwd?: string; stdin?: string } = {},
): RunResult {
  const proc = Bun.spawnSync([resolve(binary), ...args], {
    cwd: options.cwd,
    env: isolatedEnv,
    stdin: options.stdin === undefined ? "ignore" : Buffer.from(options.stdin),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 60_000,
  });
  const stdout = proc.stdout.toString();
  const stderr = proc.stderr.toString();
  return { exitCode: proc.exitCode ?? -1, stdout, stderr, combined: stdout + stderr };
}

async function runAsync(binary: string, args: string[], cwd?: string): Promise<RunResult> {
  const proc = Bun.spawn([resolve(binary), ...args], {
    cwd,
    env: isolatedEnv,
    stdin: "ignore",
    stdout: "pipe",
    stderr: "pipe",
  });
  const [stdout, stderr, exitCode] = await Promise.all([
    new Response(proc.stdout).text(),
    new Response(proc.stderr).text(),
    proc.exited,
  ]);
  return { exitCode, stdout, stderr, combined: stdout + stderr };
}

function expectExit(result: RunResult, code: number, label: string): void {
  if (result.exitCode !== code)
    throw new Error(`${label}: expected exit ${code}, got ${result.exitCode}\n${result.combined}`);
}

function expectIncludes(text: string, expected: string, label: string): void {
  if (!text.includes(expected)) throw new Error(`${label}: expected output to contain ${JSON.stringify(expected)}, got:\n${text}`);
}

function expectExcludes(text: string, unexpected: string, label: string): void {
  if (text.includes(unexpected)) throw new Error(`${label}: output should not contain ${JSON.stringify(unexpected)}, got:\n${text}`);
}

const REMOVED = "was removed in GocciaScript 0.14.0";

// Binaries built on the shared application, with a positional argument that
// lets each reach option validation without doing any work first.
const ENGINE_BINARIES: { name: string; binary: string; args: string[] }[] = [
  { name: "GocciaScriptLoader", binary: LOADER, args: ["missing.js"] },
  { name: "GocciaTestRunner", binary: TESTRUNNER, args: ["missing.js"] },
  { name: "GocciaBenchmarkRunner", binary: BENCHRUNNER, args: ["missing.js"] },
  { name: "GocciaBundler", binary: BUNDLER, args: ["missing.js"] },
  { name: "GocciaREPL", binary: REPL, args: [] },
  { name: "GocciaSandboxRunner", binary: SANDBOXRUNNER, args: ["/missing.js"] },
];

// -- Removed flags --------------------------------------------------------------

console.log("Removed flags exit 2 and name their replacement...");
{
  const removedFlags: [string, string][] = [
    ["--allowed-host=example.com", "--allowed-host was removed in GocciaScript 0.14.0; use --allow-net=<host>[,<host>...] instead"],
    ["--fetch-deny-private-ranges", "--fetch-deny-private-ranges was removed in GocciaScript 0.14.0; private ranges are denied by default; allow them with --allow-net=private, or refuse them outright with --deny-net=private"],
    ["--fetch-max-response-bytes=10", "--fetch-max-response-bytes was removed in GocciaScript 0.14.0; use --max-fetch-bytes instead (units: 1MiB)"],
    ["--unsafe-ffi", "--unsafe-ffi was removed in GocciaScript 0.14.0; use --allow-ffi[=<library>,...] instead"],
    ["--allow-node-modules", "--allow-node-modules was removed in GocciaScript 0.14.0; use --allow-import=node_modules[=<dir>] instead"],
    ["--allow-node-modules=./vendor", "--allow-node-modules was removed in GocciaScript 0.14.0; use --allow-import=node_modules[=<dir>] instead"],
    ["--no-host-filesystem", "--no-host-filesystem was removed in GocciaScript 0.14.0; host reads are denied by default; use --deny-read to also refuse imports from the project"],
    ["--stack-size=100", "--stack-size was removed in GocciaScript 0.14.0; use --max-stack instead"],
  ];
  for (const { name, binary, args } of ENGINE_BINARIES) {
    for (const [flag, message] of removedFlags) {
      const result = run(binary, [flag, ...args]);
      expectExit(result, 2, `${name} ${flag}`);
      expectIncludes(result.stderr, `Error: ${message}`, `${name} ${flag}`);
    }
  }

  const sandboxFlags: [string, string][] = [
    ["--fs-quota-bytes=1024", "--fs-quota-bytes was removed in GocciaScript 0.14.0; use --max-fs-bytes instead (units: 16MiB)"],
    ["--fs-node-limit=10", "--fs-node-limit was removed in GocciaScript 0.14.0; use --max-fs-nodes instead"],
  ];
  for (const [flag, message] of sandboxFlags) {
    const result = run(SANDBOXRUNNER, [flag, "/missing.js"]);
    expectExit(result, 2, `GocciaSandboxRunner ${flag}`);
    expectIncludes(result.stderr, `Error: ${message}`, `GocciaSandboxRunner ${flag}`);
  }

  const bare = run(BARE, ["--stack-size=100", "missing.js"]);
  expectExit(bare, 2, "GocciaScriptLoaderBare --stack-size");
  expectIncludes(bare.stderr, "Error: --stack-size was removed in GocciaScript 0.14.0; use --max-stack instead", "Bare --stack-size");

  const test262 = run(TEST262RUNNER, ["--timeout-ms=50"]);
  expectExit(test262, 2, "GocciaTest262Runner --timeout-ms");
  // An invalid value is exit 1 there as everywhere else; only usage errors
  // exit 2.
  for (const [flag, message] of [
    ["--timeout=5x", "Invalid value for --timeout: 5x"],
    ["--max-memory=64MB", '"MB" is ambiguous'],
    ["--jobs=many", "--jobs requires a non-negative integer"],
  ] as const) {
    const invalid = run(TEST262RUNNER, [flag]);
    expectExit(invalid, 1, `GocciaTest262Runner ${flag}`);
    expectIncludes(invalid.stderr, message, `GocciaTest262Runner ${flag}`);
  }
  expectIncludes(test262.stderr, "Error: --timeout-ms was removed in GocciaScript 0.14.0; use --timeout instead (units: 20s)", "Test262 --timeout-ms");

  // Removed flags stay out of --help.
  const help = run(LOADER, ["--help"]);
  expectExit(help, 0, "Loader --help");
  for (const [flag] of removedFlags) expectExcludes(help.stdout, flag.split("=")[0] + " ", "Loader --help");
}

// -- Removed and command-line-only config keys ----------------------------------

console.log("Removed and command-line-only config keys exit 2...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "main.js"), "1;\n");
    const cases: [string, string][] = [
      ['{"allowed-hosts": ["example.com"]}', `"allowed-hosts" ${REMOVED}; use "permissions": { "allow-net": [...] } instead`],
      ['{"unsafe-ffi": true}', `"unsafe-ffi" ${REMOVED}; use "permissions": { "allow-ffi": true } instead`],
      ['{"allow-node-modules": true}', `"allow-node-modules" ${REMOVED}; use "permissions": { "allow-import": ["node_modules"] } instead`],
      ['{"no-host-filesystem": true}', `"no-host-filesystem" ${REMOVED}; use "permissions": { "deny-read": true } instead`],
      ['{"fetch-deny-private-ranges": true}', `"fetch-deny-private-ranges" ${REMOVED}; private ranges are denied by default; allow them with "permissions": { "allow-net": ["private"] }, or refuse them outright with "permissions": { "deny-net": ["private"] }`],
      ['{"fetch-max-response-bytes": 10}', `"fetch-max-response-bytes" ${REMOVED}; use "max-fetch-bytes" instead`],
      ['{"stack-size": 100}', `"stack-size" ${REMOVED}; use "max-stack" instead`],
      ['{"allow-net": ["example.com"]}', `"allow-net" can only be given on the command line; declare it in the config's "permissions" object instead`],
      ['{"deny-read": true}', `"deny-read" can only be given on the command line`],
    ];
    const configPath = join(tmp, "goccia.json");
    for (const [config, message] of cases) {
      writeFileSync(configPath, config + "\n");
      for (const binary of [LOADER, TESTRUNNER]) {
        const result = run(binary, ["main.js"], { cwd: tmp });
        expectExit(result, 2, `${binary} config ${config}`);
        expectIncludes(result.combined, `${configPath}: ${message}`, `${binary} config ${config}`);
      }
    }

    // A per-file config below the root config is checked too.
    writeFileSync(configPath, "{}\n");
    mkdirSync(join(tmp, "sub"));
    writeFileSync(join(tmp, "sub", "goccia.json"), '{"unsafe-ffi": true}\n');
    writeFileSync(join(tmp, "sub", "a.js"), "1;\n");
    const nested = run(LOADER, [join("sub", "a.js")], { cwd: tmp });
    expectIncludes(nested.combined, `${join(tmp, "sub", "goccia.json")}: "unsafe-ffi" ${REMOVED}`, "per-file removed key");

    // A config flag must be exactly true or false.
    writeFileSync(configPath, '{"compat-asi": "yes"}\n');
    const flagValue = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(flagValue, 1, "config flag value");
    expectIncludes(flagValue.combined, `${configPath}: "compat-asi" must be true or false, got "yes"`, "config flag value");

    // A flag in config is exactly a boolean; a scalar option takes one value.
    const valueCases: [string, string][] = [
      ['{"compat-var": null}', '"compat-var" must be true or false, got null'],
      ['{"compat-var": "true"}', '"compat-var" must be true or false, got "true"'],
      ['{"max-stack": [1]}', '"max-stack" must be a single value, not an array'],
      ['{"max-memory": 100000000000000000000}', `Invalid value for "max-memory" in ${configPath}: 100000000000000000000 (value is too large)`],
      ['{"max-memory": 1e20}', `Invalid value for "max-memory" in ${configPath}: 1e20 (`],
      ['{"extends": {"path": "base.json"}}', `${configPath}: "extends" must be a path`],
      ['{"extends": 1}', `${configPath}: "extends" must be a path`],
      ['{"max-memory": "64MB"}', `Invalid value for "max-memory" in ${configPath}: 64MB ("MB" is ambiguous`],
      ['{"permissions": {"allow-net": [""]}}', `${configPath}: "permissions.allow-net" has an empty scope`],
    ];
    for (const [config, message] of valueCases) {
      writeFileSync(configPath, config + "\n");
      const result = run(LOADER, ["main.js"], { cwd: tmp });
      expectExit(result, 1, `config ${config}`);
      expectIncludes(result.combined, message, `config ${config}`);
    }
    writeFileSync(join(tmp, "goccia.toml"), 'compat-var = "true"\n');
    const tomlString = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(tomlString, 1, "TOML string flag");
    rmSync(join(tmp, "goccia.toml"));
    writeFileSync(join(tmp, "goccia.toml"), '[extends]\npath = "base.toml"\n');
    const tomlExtends = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(tomlExtends, 1, "TOML extends table");
    expectIncludes(tomlExtends.combined, '"extends" must be a path', "TOML extends table");
    rmSync(join(tmp, "goccia.toml"));
    writeFileSync(join(tmp, "goccia.json5"), "{ extends: 1 }\n");
    const json5Extends = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(json5Extends, 1, "JSON5 extends number");
    expectIncludes(json5Extends.combined, '"extends" must be a path', "JSON5 extends number");
    rmSync(join(tmp, "goccia.json5"));

    // An unknown permission is a usage error naming the valid keys.
    writeFileSync(configPath, '{"permissions": {"deny-nett": true}}\n');
    const typo = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(typo, 2, "unknown permission");
    expectIncludes(typo.combined, `${configPath}: unknown permission "deny-nett" (valid: allow-read, allow-net, allow-ffi, allow-import, deny-read, deny-net, deny-ffi, deny-import)`, "unknown permission");
  } finally {
    clean(tmp);
  }
}

// -- Flag values -----------------------------------------------------------------

console.log("Boolean flags reject values; scope lists validate...");
{
  for (const flag of ["--compat-asi=false", "--compat-asi=", "--deterministic=true"]) {
    const result = run(LOADER, [flag, "missing.js"]);
    expectExit(result, 2, flag);
    expectIncludes(result.stderr, "does not take a value", flag);
  }
  const cases: [string[], string][] = [
    [["--allow-net="], '--allow-net= has an empty scope list; omit "=" to allow every public host'],
    [["--allow-read=a,,b"], "Empty scope in --allow-read=a,,b"],
    [["--allow-import"], "--allow-import needs a scope: node_modules[=<dir>] or a provider such as github"],
    [["--allow-net=http://x"], 'Invalid scope for --allow-net: "http://x" (use host, host:port, *.domain, an IP, a CIDR range, or private)'],
    [["--timeout=5h"], "Invalid value for --timeout: 5h (use a duration such as 500ms, 5s, or 2m, or plain milliseconds)"],
    [["--max-memory=64MB"], 'Invalid value for --max-memory: 64MB ("MB" is ambiguous; use KiB, MiB, or GiB, or a plain byte count)'],
    [["--max-stack=-1"], "Invalid value for --max-stack: -1 (use a non-negative whole number)"],
  ];
  for (const [args, message] of cases) {
    const result = run(LOADER, [...args, "missing.js"]);
    expectExit(result, 1, args.join(" "));
    expectIncludes(result.combined, message, args.join(" "));
  }
}

// -- What each binary honors -----------------------------------------------------

console.log("Unsupported capabilities and limits are rejected on the command line...");
{
  const rejected: [string, string[], string][] = [
    [BUNDLER, ["--allow-read", "missing.js"], "GocciaBundler cannot grant read; it supports no capability flags. Remove --allow-read."],
    [BUNDLER, ["--timeout=5s", "missing.js"], "GocciaBundler does not support --timeout. Remove it."],
    [BUNDLER, ["--max-memory=64MiB", "missing.js"], "GocciaBundler does not support --max-memory. Remove it."],
    [SANDBOXRUNNER, ["--allow-read", "/missing.js"], "GocciaSandboxRunner cannot grant read; it supports net. Remove --allow-read."],
    [SANDBOXRUNNER, ["--allow-ffi", "/missing.js"], "GocciaSandboxRunner cannot grant ffi; it supports net. Remove --allow-ffi."],
    [BARE, ["--allow-net=example.com", "missing.js"], "GocciaScriptLoaderBare cannot grant net; it supports no capability flags. Remove --allow-net."],
    [TEST262RUNNER, ["--allow-read"], "GocciaTest262Runner cannot grant read; it supports no capability flags. Remove --allow-read."],
  ];
  for (const [binary, args, message] of rejected) {
    const result = run(binary, args);
    expectExit(result, 2, `${binary} ${args.join(" ")}`);
    expectIncludes(result.stderr, `Error: ${message}`, `${binary} ${args.join(" ")}`);
  }

  // A deny is always accepted.
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "main.js"), "const x = 1;\n");
    const bundle = run(BUNDLER, ["--deny-read", "main.js"], { cwd: tmp });
    expectExit(bundle, 0, "Bundler --deny-read");
    const bare = run(BARE, ["--deny-net", "main.js"], { cwd: tmp });
    expectExit(bare, 0, "Bare --deny-net");
  } finally {
    clean(tmp);
  }

  // Help advertises only what the binary honors.
  const sandboxHelp = run(SANDBOXRUNNER, ["--help"]).stdout;
  expectIncludes(sandboxHelp, "--allow-net", "SandboxRunner --help");
  expectExcludes(sandboxHelp, "--allow-read", "SandboxRunner --help");
  expectIncludes(sandboxHelp, "--max-fs-bytes", "SandboxRunner --help");
  const bundlerHelp = run(BUNDLER, ["--help"]).stdout;
  expectExcludes(bundlerHelp, "--allow-", "Bundler --help");
  expectExcludes(bundlerHelp, "--timeout", "Bundler --help");
  const loaderHelp = run(LOADER, ["--help"]).stdout;
  for (const flag of ["--allow-read", "--allow-net", "--allow-ffi", "--allow-import", "--deny-read", "--max-stack", "--max-fetch-bytes"])
    expectIncludes(loaderHelp, flag, "Loader --help");
}

console.log("Binaries with their own parser follow the same grammar...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "main.js"), "print(1);\n");
    // Malformed deny flags fail exactly as on the shared-application binaries.
    const malformed: [string, string, number][] = [
      ["--deny-import", "--deny-import needs a scope: node_modules[=<dir>] or a provider such as github", 1],
      ["--deny-net=", "--deny-net= has an empty scope list", 1],
      ["--deny-read=a,,b", "Empty scope in --deny-read=a,,b", 1],
      ["--deny-net=http://x", 'Invalid scope for --deny-net: "http://x"', 1],
    ];
    for (const [flag, message, code] of malformed) {
      for (const [binary, args] of [[BARE, [flag, "main.js"]], [TEST262RUNNER, [flag]], [LOADER, [flag, "main.js"]]] as const) {
        const result = run(binary, [...args], { cwd: tmp });
        expectExit(result, code, `${binary} ${flag}`);
        expectIncludes(result.combined, message, `${binary} ${flag}`);
      }
    }
    // A well-formed deny is accepted.
    expectExit(run(BARE, ["--deny-net=example.com", "main.js"], { cwd: tmp }), 0, "Bare --deny-net=example.com");

    // Boolean flags reject a value.
    const bareFlag = run(BARE, ["--compat-var=false", "main.js"], { cwd: tmp });
    expectExit(bareFlag, 2, "Bare --compat-var=false");
    expectIncludes(bareFlag.stderr, "--compat-var does not take a value", "Bare --compat-var=false");
    const test262Flag = run(TEST262RUNNER, ["--verbose=false"], { cwd: tmp });
    expectExit(test262Flag, 2, "Test262 --verbose=false");
    expectIncludes(test262Flag.stderr, "--verbose does not take a value", "Test262 --verbose=false");
    const bareShort = run(BARE, ["-A", "main.js"], { cwd: tmp });
    expectExit(bareShort, 1, "Bare -A");
    expectIncludes(bareShort.stderr, "Unknown option: -A", "Bare -A");

    // Grammar comes before support, as in the shared parser: a malformed
    // --allow-* is an invalid value (1) everywhere, a well-formed unsupported
    // one a usage error (2).
    for (const [flag, message] of [
      ["--allow-net=", "--allow-net= has an empty scope list"],
      ["--allow-read=a,,b", "Empty scope in --allow-read=a,,b"],
      ["--allow-import", "--allow-import needs a scope"],
    ] as const) {
      for (const [binary, args] of [[BARE, [flag, "main.js"]], [TEST262RUNNER, [flag]], [BUNDLER, [flag, "main.js"]], [LOADER, [flag, "main.js"]]] as const) {
        const result = run(binary, [...args], { cwd: tmp });
        expectExit(result, 1, `${binary} ${flag}`);
        expectIncludes(result.combined, message, `${binary} ${flag}`);
      }
    }
    for (const [binary, args] of [[BARE, ["--allow-net=example.com", "main.js"]], [TEST262RUNNER, ["--allow-net=example.com"]], [BUNDLER, ["--allow-net=example.com", "main.js"]]] as const) {
      const result = run(binary, [...args], { cwd: tmp });
      expectExit(result, 2, `${binary} well-formed unsupported --allow-net`);
      expectIncludes(result.stderr, "cannot grant net", `${binary} well-formed unsupported --allow-net`);
    }

    // Limits a binary does not apply are rejected like the bundler's.
    const unsupportedLimits: [string, string[], string][] = [
      [BARE, ["--max-fetch-bytes=1MiB", "main.js"], "GocciaScriptLoaderBare does not support --max-fetch-bytes. Remove it."],
      [TEST262RUNNER, ["--max-stack=100"], "GocciaTest262Runner does not support --max-stack. Remove it."],
      [TEST262RUNNER, ["--max-instructions=100"], "GocciaTest262Runner does not support --max-instructions. Remove it."],
      [TEST262RUNNER, ["--max-fetch-bytes=1MiB"], "GocciaTest262Runner does not support --max-fetch-bytes. Remove it."],
    ];
    for (const [binary, args, message] of unsupportedLimits) {
      const result = run(binary, args, { cwd: tmp });
      expectExit(result, 2, `${binary} ${args[0]}`);
      expectIncludes(result.stderr, `Error: ${message}`, `${binary} ${args[0]}`);
    }

    // The WASM runner rejects options cleanly instead of crashing.
    const wasmHelp = run(WASMTESTRUNNER, ["--help"], { cwd: tmp });
    expectExit(wasmHelp, 0, "WasmTestRunner --help");
    expectIncludes(wasmHelp.stdout, "Usage: GocciaWasmTestRunner [-P] <manifest-file>", "WasmTestRunner --help");
    const wasmFlag = run(WASMTESTRUNNER, ["--allow-read"], { cwd: tmp });
    expectExit(wasmFlag, 2, "WasmTestRunner --allow-read");
    expectIncludes(wasmFlag.stderr, "Unknown option: --allow-read", "WasmTestRunner --allow-read");
    expectIncludes(wasmHelp.stdout, "-P", "WasmTestRunner --help lists -P");
    const wasmNoManifest = run(WASMTESTRUNNER, ["-P"], { cwd: tmp });
    expectExit(wasmNoManifest, 2, "WasmTestRunner -P without a manifest");
    const wasmMissing = run(WASMTESTRUNNER, [join(tmp, "missing.txt")], { cwd: tmp });
    expectExit(wasmMissing, 2, "WasmTestRunner missing manifest");
    expectIncludes(wasmMissing.stderr, "manifest not found", "WasmTestRunner missing manifest");

    // Its unsupported-request warning names the config once.
    mkdirSync(join(tmp, "wasm"));
    writeFileSync(join(tmp, "wasm", "goccia.json"), '{"permissions": {"allow-import": ["node_modules"]}}\n');
    writeFileSync(join(tmp, "wasm", "t.js"), 'test("t", () => {});\n');
    writeFileSync(join(tmp, "manifest.txt"), join(tmp, "wasm", "t.js") + "\n");
    const wasmWarn = run(WASMTESTRUNNER, [join(tmp, "manifest.txt")], { cwd: tmp });
    // -P accepts config requests (none here needs it), and extra
    // positional arguments are ignored with a warning, as before.
    const wasmTolerant = run(WASMTESTRUNNER, ["-P", join(tmp, "manifest.txt"), "extra-argument"], { cwd: tmp });
    expectExit(wasmTolerant, 0, "WasmTestRunner -P with an extra argument");
    expectIncludes(wasmTolerant.stdout, "SUMMARY files=1", "WasmTestRunner -P runs the manifest");
    expectIncludes(wasmTolerant.stderr, "Warning: ignoring extra argument: extra-argument", "WasmTestRunner extra argument");
    const configPath = join(tmp, "wasm", "goccia.json");
    expectIncludes(wasmWarn.stderr, `WARN ${configPath} :: requests allow-import, which GocciaWasmTestRunner cannot grant; ignoring it`, "WasmTestRunner warning");
    expectExcludes(wasmWarn.stderr, `:: Warning: ${configPath}`, "WasmTestRunner warning names the config once");
  } finally {
    clean(tmp);
  }
}

console.log("Config requests a binary cannot honor are warnings...");
{
  const tmp = makeTmp();
  try {
    const configPath = join(tmp, "goccia.json");
    writeFileSync(configPath, '{"permissions": {"allow-read": ["."], "allow-net": ["example.com"]}}\n');
    writeFileSync(join(tmp, "main.js"), "const x = 1;\n");

    const bundle = run(BUNDLER, ["main.js"], { cwd: tmp });
    expectExit(bundle, 0, "Bundler with a declaring config");
    expectIncludes(bundle.stderr, `Warning: ${configPath} requests allow-read, which GocciaBundler cannot grant; ignoring it`, "Bundler warning");

    // The sandbox honors net only: a config asking for read alone warns and
    // needs no trust, and one asking for net needs trust.
    writeFileSync(join(tmp, "entry.js"), "1 + 1;\n");
    const sandboxArgs = [`--config=${configPath}`, "--seed", `${join(tmp, "entry.js")}=/entry.js`, "/entry.js"];
    writeFileSync(configPath, '{"permissions": {"allow-read": ["."]}}\n');
    const sandbox = run(SANDBOXRUNNER, sandboxArgs, { cwd: tmp });
    expectExit(sandbox, 0, "SandboxRunner with allow-read config");
    expectIncludes(sandbox.stderr, `Warning: ${configPath} requests allow-read, which GocciaSandboxRunner cannot grant; ignoring it`, "SandboxRunner warning");

    writeFileSync(configPath, '{"permissions": {"allow-read": ["."], "allow-net": ["example.com"]}}\n');
    const untrusted = run(SANDBOXRUNNER, sandboxArgs, { cwd: tmp });
    expectExit(untrusted, 2, "SandboxRunner with allow-net config");
    expectIncludes(untrusted.stderr, "goccia.json (never trusted)\n    allow-net: example.com", "SandboxRunner allow-net needs trust");
    const accepted = run(SANDBOXRUNNER, ["-P", ...sandboxArgs], { cwd: tmp });
    expectExit(accepted, 0, "SandboxRunner -P");
    expectExcludes(accepted.stderr, "allow-net", "SandboxRunner honors net");
  } finally {
    clean(tmp);
  }
}

// -- Defaults ---------------------------------------------------------------------

console.log("Nothing beyond the project's module graph is granted by default...");
{
  const tmp = makeTmp();
  try {
    const project = join(tmp, "project");
    const outside = join(tmp, "outside");
    mkdirSync(join(project, "lib"), { recursive: true });
    mkdirSync(outside);
    writeFileSync(join(project, "lib", "value.js"), "export const value = 7;\n");
    writeFileSync(join(outside, "secret.js"), 'export const secret = "OUTSIDE";\n');
    writeFileSync(join(outside, "data.txt"), "OUTSIDE-TEXT\n");

    const staticImport = join(project, "static.mjs");
    writeFileSync(staticImport, 'import { value } from "./lib/value.js";\nconsole.log("value", value);\n');
    const inside = run(LOADER, [staticImport]);
    expectExit(inside, 0, "static import inside the project");
    expectIncludes(inside.stdout, "value 7", "static import inside the project");

    const outsideImport = join(project, "outside.mjs");
    writeFileSync(outsideImport, 'import { secret } from "../outside/secret.js";\nconsole.log(secret);\n');
    const refused = run(LOADER, [outsideImport]);
    expectExit(refused, 1, "static import outside the project");
    expectIncludes(refused.combined, "PermissionDenied: read: ../outside/secret.js", "static import outside the project");
    expectExcludes(refused.combined, "OUTSIDE", "static import outside the project");

    const bytesImport = join(project, "bytes.mjs");
    writeFileSync(bytesImport, 'import data from "../outside/data.txt" with { type: "text" };\nconsole.log(data);\n');
    expectIncludes(run(LOADER, [bytesImport]).combined, "PermissionDenied: read: ../outside/data.txt", "text import outside the project");

    const computed = join(project, "computed.mjs");
    writeFileSync(computed, 'const name = "./lib/value.js";\nconst m = await import(name);\nconsole.log(m.value);\n');
    expectIncludes(run(LOADER, [computed]).combined, "PermissionDenied: read: ./lib/value.js", "computed import");

    // CLI scopes resolve against the working directory.
    const granted = run(LOADER, ["--allow-read=outside", join("project", "outside.mjs")], { cwd: tmp });
    expectExit(granted, 0, "--allow-read=outside");
    expectIncludes(granted.stdout, "OUTSIDE", "--allow-read=outside");
    const grantedComputed = run(LOADER, ["--allow-read=project/lib", join("project", "computed.mjs")], { cwd: tmp });
    expectIncludes(grantedComputed.stdout, "7", "--allow-read for a computed import");

    // A deny wins over an allow and removes the module-graph exemption.
    const denied = run(LOADER, ["--allow-read", "--deny-read=outside", join("project", "outside.mjs")], { cwd: tmp });
    expectExit(denied, 1, "--deny-read scope");
    expectIncludes(denied.combined, "PermissionDenied: read: ../outside/secret.js", "--deny-read scope");
    const deniedProject = run(LOADER, ["--deny-read", staticImport]);
    expectExit(deniedProject, 1, "--deny-read");
    expectExcludes(deniedProject.stdout, "value 7", "--deny-read refuses project imports");

    // FFI, fetch, and node_modules are off.
    const probes = join(project, "probes.js");
    writeFileSync(probes, [
      'console.log("ffi", typeof FFI);',
      'try { fetch("http://example.com/"); } catch (e) { console.log("fetch", e.name, e.message); }',
      "",
    ].join("\n"));
    const probe = run(LOADER, [probes]);
    expectIncludes(probe.stdout, "ffi undefined", "FFI is off");
    expectIncludes(probe.stdout, "fetch PermissionDenied net: example.com", "fetch is off");

    mkdirSync(join(project, "node_modules", "pkg"), { recursive: true });
    writeFileSync(join(project, "node_modules", "pkg", "package.json"), '{"name": "pkg", "main": "index.js"}\n');
    writeFileSync(join(project, "node_modules", "pkg", "index.js"), "export default 42;\n");
    const bare = join(project, "bare.mjs");
    writeFileSync(bare, 'import pkg from "pkg";\nconsole.log("pkg", pkg);\n');
    const sealed = run(LOADER, [bare]);
    expectExit(sealed, 1, "bare specifier sealed");
    expectExcludes(sealed.stdout, "pkg 42", "bare specifier sealed");
    const opened = run(LOADER, ["--allow-import=node_modules", bare]);
    expectExit(opened, 0, "--allow-import=node_modules");
    expectIncludes(opened.stdout, "pkg 42", "--allow-import=node_modules");
    const ceiling = run(LOADER, ["--allow-import=node_modules=project", join("project", "bare.mjs")], { cwd: tmp });
    expectIncludes(ceiling.stdout, "pkg 42", "--allow-import=node_modules=<dir> relative to cwd");
  } finally {
    clean(tmp);
  }
}

console.log("Private network ranges need an explicit address or private...");
{
  const tmp = makeTmp();
  try {
    // Port 1 on loopback refuses the connection, so an allowed request fails
    // with a network TypeError, while a refused one throws PermissionDenied
    // (synchronously for the URL's own host, as a rejection once a name
    // resolves) before any connection is attempted.
    const script = (url: string) =>
      `try { fetch(${JSON.stringify(url)}).then(() => console.log("reached"), (e) => console.log(e.name, e.message)); } catch (e) { console.log(e.name, e.message); }\n`;
    writeFileSync(join(tmp, "ip.js"), script("http://127.0.0.1:1/"));
    writeFileSync(join(tmp, "name.js"), script("http://localhost:1/"));

    const noGrant = run(LOADER, [join(tmp, "ip.js")]);
    expectIncludes(noGrant.stdout, "PermissionDenied net: 127.0.0.1:1", "no grant");

    const unscoped = run(LOADER, ["--allow-net", join(tmp, "ip.js")]);
    expectIncludes(unscoped.stdout, "PermissionDenied net: 127.0.0.1:1", "unscoped allow excludes private");

    const explicitIp = run(LOADER, ["--allow-net=127.0.0.1", join(tmp, "ip.js")]);
    expectExcludes(explicitIp.stdout, "PermissionDenied", "explicit IP reaches loopback");
    expectIncludes(explicitIp.stdout, "TypeError", "explicit IP reaches loopback");

    const hostName = run(LOADER, ["--allow-net=localhost", join(tmp, "name.js")]);
    expectIncludes(hostName.stdout, "PermissionDenied net: localhost:1", "host name resolving privately");

    const hostNamePrivate = run(LOADER, ["--allow-net=localhost,private", join(tmp, "name.js")]);
    expectExcludes(hostNamePrivate.stdout, "PermissionDenied", "private lifts the refusal");

    const privateOnly = run(LOADER, ["--allow-net=private", join(tmp, "ip.js")]);
    expectExcludes(privateOnly.stdout, "PermissionDenied", "private alone");

    const denyPrivate = run(LOADER, ["--allow-net=127.0.0.1", "--deny-net=private", join(tmp, "ip.js")]);
    expectIncludes(denyPrivate.stdout, "PermissionDenied net: 127.0.0.1:1", "deny private wins");
  } finally {
    clean(tmp);
  }
}

// -- Config permissions ------------------------------------------------------------

console.log("A malformed permissions value fails instead of being ignored...");
{
  const tmp = makeTmp();
  try {
    const project = join(tmp, "project");
    mkdirSync(join(tmp, "outside"), { recursive: true });
    mkdirSync(project);
    writeFileSync(join(tmp, "outside", "secret.js"), 'export const secret = "OUTSIDE";\n');
    writeFileSync(join(project, "main.mjs"), 'import { secret } from "../outside/secret.js";\nconsole.log(secret);\n');
    const configPath = join(project, "goccia.json");
    const cases: [string, string][] = [
      ['{"permissions": {"allow-read": ["../outside"], "deny-read": {"path": "../outside"}}}', '"permissions.deny-read" must be true, false, or an array of strings'],
      ['{"permissions": {"allow-read": ["../outside"], "deny-read": null}}', '"permissions.deny-read" must be true, false, or an array of strings'],
      ['{"permissions": {"allow-read": ["../outside"], "deny-read": [["../outside"]]}}', '"permissions.deny-read" must be true, false, or an array of strings'],
      ['{"permissions": {"allow-read": ["../outside"], "deny-nett": null}}', 'unknown permission "deny-nett"'],
    ];
    for (const [config, message] of cases) {
      writeFileSync(configPath, config + "\n");
      const result = run(LOADER, [join(project, "main.mjs")]);
      expectExit(result, 2, `malformed ${config}`);
      expectIncludes(result.stderr, `${configPath}: ${message}`, `malformed ${config}`);
      expectExcludes(result.stdout, "OUTSIDE", `malformed ${config}`);
    }
    rmSync(configPath);
    writeFileSync(join(project, "goccia.toml"), '[permissions]\nallow-read = ["../outside"]\ndeny-read = { a = 1 }\n');
    const toml = run(LOADER, [join(project, "main.mjs")]);
    expectExit(toml, 2, "malformed TOML deny-read");
    expectExcludes(toml.stdout, "OUTSIDE", "malformed TOML deny-read");
  } finally {
    clean(tmp);
  }
}

console.log("A per-file config usage error stops the run before any file executes...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "a"));
    mkdirSync(join(tmp, "b"));
    writeFileSync(join(tmp, "a", "s.js"), 'console.log("A-RAN");\n');
    writeFileSync(join(tmp, "b", "s.js"), 'console.log("B-RAN");\n');
    writeFileSync(join(tmp, "b", "goccia.json"), '{"unsafe-ffi": true}\n');
    for (const args of [[], ["--jobs=2"], ["--mode=bytecode"]]) {
      const loader = run(LOADER, [join("a", "s.js"), join("b", "s.js"), ...args], { cwd: tmp });
      expectExit(loader, 2, `Loader multi-file ${args.join(" ")}`);
      expectIncludes(loader.stderr, `Error: ${join(tmp, "b", "goccia.json")}: "unsafe-ffi" ${REMOVED}`, "Loader multi-file");
      expectExcludes(loader.stdout, "A-RAN", "Loader multi-file runs nothing");
    }
    writeFileSync(join(tmp, "a", "t.js"), 'test("a", () => { console.log("A-RAN"); });\n');
    writeFileSync(join(tmp, "b", "t.js"), 'test("b", () => {});\n');
    for (const args of [[], ["--jobs=2"]]) {
      const tests = run(TESTRUNNER, [join("a", "t.js"), join("b", "t.js"), "--no-progress", ...args], { cwd: tmp });
      expectExit(tests, 2, `TestRunner multi-file ${args.join(" ")}`);
      expectIncludes(tests.stderr, `"unsafe-ffi" ${REMOVED}`, "TestRunner multi-file");
      expectExcludes(tests.combined, "A-RAN", "TestRunner multi-file runs nothing");
    }
    const bench = run(BENCHRUNNER, [join("b", "s.js"), "--no-progress"], { cwd: tmp });
    expectExit(bench, 2, "BenchmarkRunner per-file usage error");
    // An invalid value in a per-file config is still an exit-1 error, reported
    // before anything runs.
    writeFileSync(join(tmp, "b", "goccia.json"), '{"max-memory": "64MB"}\n');
    const badValue = run(LOADER, [join("a", "s.js"), join("b", "s.js")], { cwd: tmp });
    expectExit(badValue, 1, "per-file invalid value");
    expectIncludes(badValue.combined, '"MB" is ambiguous', "per-file invalid value");
    expectExcludes(badValue.stdout, "A-RAN", "per-file invalid value runs nothing");
  } finally {
    clean(tmp);
  }
}

console.log("A net deny's suggestion names the deny, not a grant...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "f.js"), 'fetch("http://example.com/");\n');
    writeFileSync(join(tmp, "ip.js"), 'fetch("http://127.0.0.1:1/");\n');
    const cases: [string, string[], string][] = [
      ["f.js", ["--allow-net", "--deny-net=example.com"], "refused by the net deny example.com"],
      ["f.js", ["--allow-net=example.com", "--deny-net=example.com"], "refused by the net deny example.com"],
      ["f.js", ["--allow-net", "--deny-net"], "a net deny refuses every host"],
      ["ip.js", ["--allow-net=127.0.0.1", "--deny-net=127.0.0.0/8"], "refused by the net deny 127.0.0.0/8"],
      ["ip.js", ["--allow-net=127.0.0.1", "--deny-net=private"], "refused by the net deny private"],
    ];
    for (const [file, args, suggestion] of cases) {
      const result = run(LOADER, [...args, file], { cwd: tmp });
      expectExit(result, 1, `${args.join(" ")}`);
      expectIncludes(result.combined, `Suggestion: ${suggestion}`, `${args.join(" ")}`);
      expectExcludes(result.combined, "grant it with --allow-net", `${args.join(" ")}`);
    }
    // A config deny is named as the config entry it came from.
    writeFileSync(join(tmp, "goccia.json"), '{"permissions": {"deny-net": ["example.com"]}}\n');
    const configDeny = run(LOADER, ["--allow-net", "f.js"], { cwd: tmp });
    expectIncludes(configDeny.combined, "Suggestion: refused by the net deny example.com", "config deny-net");
  } finally {
    clean(tmp);
  }
}

console.log("A config's permissions govern only files in its own directory tree...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "a"));
    mkdirSync(join(tmp, "c"));
    writeFileSync(join(tmp, "outside.js"), 'export const x = "OUTSIDE";\n');
    writeFileSync(join(tmp, "a", "goccia.json"), '{"permissions": {"allow-read": [".."]}, "unsafe-function-constructor": true}\n');
    const probe = [
      'try { const m = await import("../" + "outside.js"); console.log("read", m.x); } catch (e) { console.log("read", e.name); }',
      'try { new Function("return 1")(); console.log("fn allowed"); } catch (e) { console.log("fn", e.name); }',
      "",
    ].join("\n");
    writeFileSync(join(tmp, "a", "x.mjs"), probe);
    writeFileSync(join(tmp, "c", "l.mjs"), probe);
    for (const args of [[], ["--mode=bytecode"]]) {
      const result = run(LOADER, ["-P", join("a", "x.mjs"), join("c", "l.mjs"), ...args], { cwd: tmp });
      // Each file's output precedes its own "Running script" line.
      const [first, second] = result.stdout.split("Running script");
      expectIncludes(first, "read OUTSIDE", "a/x.mjs uses a's grants");
      expectIncludes(first, "fn allowed", "a/x.mjs uses a's unsafe key");
      expectIncludes(second ?? "", "read PermissionDenied", "c/l.mjs gets no grants from a");
      expectExcludes(second ?? "", "fn allowed", "c/l.mjs gets no unsafe keys from a");
    }
    // An explicit --config governs every input.
    const explicit = run(LOADER, ["-P", `--config=${join(tmp, "a", "goccia.json")}`, join("c", "l.mjs")], { cwd: tmp });
    expectIncludes(explicit.stdout, "read OUTSIDE", "explicit --config governs every input");
  } finally {
    clean(tmp);
  }
}

console.log("Config permissions resolve against the declaring file...");
{
  const tmp = makeTmp();
  try {
    const project = join(tmp, "project");
    mkdirSync(join(tmp, "shared"), { recursive: true });
    mkdirSync(project);
    writeFileSync(join(tmp, "shared", "data.js"), 'export const data = "SHARED";\n');
    writeFileSync(join(project, "goccia.json"), '{"permissions": {"allow-read": ["../shared"]}}\n');
    writeFileSync(join(project, "main.mjs"), 'import { data } from "../shared/data.js";\nconsole.log(data);\n');
    // Run from an unrelated directory: the scope still means ../shared from
    // the config file. -P accepts the request for the run.
    const result = run(LOADER, ["-P", join(project, "main.mjs")], { cwd: join(tmp, "shared") });
    expectExit(result, 0, "config allow-read");
    expectIncludes(result.stdout, "SHARED", "config allow-read");

    // A command-line deny subtracts from the config's allow.
    const denied = run(LOADER, ["-P", "--deny-read=shared", join(project, "main.mjs")], { cwd: tmp });
    expectExit(denied, 1, "CLI deny over config allow");

    // TOML and JSON5 declare the same block.
    writeFileSync(join(project, "goccia.json"), "{}\n");
    writeFileSync(join(project, "goccia.toml"), '[permissions]\nallow-read = ["../shared"]\n');
    expectIncludes(run(LOADER, ["-P", join(project, "main.mjs")]).stdout, "SHARED", "TOML permissions");
  } finally {
    clean(tmp);
  }
}

// -- Limits and units ---------------------------------------------------------------

console.log("Limits take units on the command line and in config...");
{
  const tmp = makeTmp();
  try {
    const spin =
      "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
    writeFileSync(join(tmp, "spin.js"), spin);
    const cli = run(LOADER, ["--timeout=50ms", join(tmp, "spin.js")]);
    expectExit(cli, 1, "--timeout=50ms");
    expectIncludes(cli.combined, "timed out", "--timeout=50ms");

    const configDir = join(tmp, "configured");
    mkdirSync(configDir);
    writeFileSync(join(configDir, "spin.js"), spin);
    writeFileSync(join(configDir, "goccia.json"), '{"timeout": "50ms"}\n');
    const config = run(LOADER, [join(configDir, "spin.js")]);
    expectExit(config, 1, 'config "timeout": "50ms"');
    expectIncludes(config.combined, "timed out", 'config "timeout": "50ms"');

    writeFileSync(join(configDir, "goccia.json"), '{"max-memory": "64MB"}\n');
    writeFileSync(join(configDir, "one.js"), "1;\n");
    const badMemory = run(LOADER, [join(configDir, "one.js")]);
    expectExit(badMemory, 1, "config max-memory 64MB");
    expectIncludes(badMemory.combined, '"MB" is ambiguous', "config max-memory 64MB");

    const stack = run(LOADER, ["--max-stack=100"], {
      stdin: "let n = 0; const f = () => { n++; f(); }; try { f(); } catch (e) { console.log(n); }\n",
    });
    expectIncludes(stack.stdout, "100", "--max-stack=100");

    // The REPL applies --timeout to each input.
    const repl = run(REPL, ["--timeout=100ms"], { stdin: spin + "1 + 1\n" });
    expectIncludes(repl.combined, "timed out", "REPL --timeout");
    expectIncludes(repl.combined, "2", "REPL continues after a timeout");
  } finally {
    clean(tmp);
  }
}

console.log("Object values for flags and limits are rejected, not ignored...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "main.js"), 'console.log("RAN");\n');
    const configPath = join(tmp, "goccia.json");
    const cases: [string, string, string][] = [
      ["goccia.json", '{"max-memory": {"a": 1}}', '"max-memory" must be a single value, not null or an object'],
      ["goccia.json", '{"timeout": {}}', '"timeout" must be a single value, not null or an object'],
      ["goccia.json", '{"compat-asi": {"x": 1}}', '"compat-asi" must be true or false, got an object'],
      ["goccia.json5", '{ "max-memory": { a: 1 } }', '"max-memory" must be a single value, not null or an object'],
      ["goccia.toml", '[max-memory]\na = 1\n', '"max-memory" must be a single value, not null or an object'],
    ];
    for (const [name, config, message] of cases) {
      for (const other of ["goccia.json", "goccia.json5", "goccia.toml"]) rmSync(join(tmp, other), { force: true });
      writeFileSync(join(tmp, name), config);
      const result = run(LOADER, ["main.js"], { cwd: tmp });
      expectExit(result, 1, `${name} ${config}`);
      expectIncludes(result.combined, message, `${name} ${config}`);
      expectExcludes(result.stdout, "RAN", `${name} ${config}`);
    }
    for (const other of ["goccia.json5", "goccia.toml"]) rmSync(join(tmp, other), { force: true });

    // A per-file config, and a base reached through extends, are checked too.
    writeFileSync(configPath, "{}\n");
    mkdirSync(join(tmp, "sub"));
    writeFileSync(join(tmp, "sub", "main.js"), 'console.log("RAN");\n');
    writeFileSync(join(tmp, "base.json"), '{"max-stack": {"n": 1}}\n');
    writeFileSync(join(tmp, "sub", "goccia.json"), '{"extends": "../base.json"}\n');
    const inherited = run(LOADER, [join("sub", "main.js")], { cwd: tmp });
    expectExit(inherited, 1, "extends base with an object limit");
    expectIncludes(inherited.combined, `${join(tmp, "base.json")}: "max-stack" must be a single value, not null or an object`, "extends base");
    const tests = run(TESTRUNNER, [join("sub", "main.js"), "--no-progress"], { cwd: tmp });
    expectExit(tests, 1, "TestRunner per-file object limit");

    // Nested sections that take objects keep working.
    writeFileSync(join(tmp, "sub", "goccia.json"), '{"modules": {"virtual:x": {"content": "export default 1;"}}, "permissions": {}}\n');
    writeFileSync(join(tmp, "sub", "main.js"), 'import x from "virtual:x";\nconsole.log("RAN", x);\n');
    const nested = run(LOADER, [join("sub", "main.js"), "--source-type=module"], { cwd: tmp });
    expectExit(nested, 0, "modules and permissions objects");
    expectIncludes(nested.stdout, "RAN 1", "modules and permissions objects");
  } finally {
    clean(tmp);
  }
}

console.log("Limit bounds and unsupported limits in config...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "main.js"), 'console.log("RAN");\n');
    // Values a limit cannot hold are rejected while parsing options, the same
    // way on every binary, before anything runs.
    const tooLarge: [string, string[], string][] = [
      [LOADER, ["--max-fetch-bytes=3GiB", "main.js"], "Invalid value for --max-fetch-bytes: 3GiB (value is too large)"],
      [LOADER, ["--max-stack=99999999999", "main.js"], "Invalid value for --max-stack: 99999999999 (value is too large)"],
      [BARE, ["--max-stack=99999999999", "main.js"], "Invalid value for --max-stack: 99999999999 (value is too large)"],
      [SANDBOXRUNNER, ["--max-fs-nodes=99999999999", "/main.js"], "Invalid value for --max-fs-nodes: 99999999999 (value is too large)"],
    ];
    for (const [binary, args, message] of tooLarge) {
      const result = run(binary, args, { cwd: tmp });
      expectExit(result, 1, `${binary} ${args.join(" ")}`);
      expectIncludes(result.combined, message, `${binary} ${args.join(" ")}`);
      expectExcludes(result.combined, "Fatal error", `${binary} ${args.join(" ")}`);
      expectExcludes(result.stdout, "RAN", `${binary} ${args.join(" ")}`);
    }

    // A per-file limit is bounded the same way.
    mkdirSync(join(tmp, "sub"));
    writeFileSync(join(tmp, "sub", "main.js"), 'console.log("RAN");\n');
    writeFileSync(join(tmp, "sub", "goccia.json"), '{"max-fetch-bytes": "3GiB"}\n');
    const perFile = run(LOADER, [join("sub", "main.js")], { cwd: tmp });
    expectExit(perFile, 1, "per-file max-fetch-bytes 3GiB");
    expectIncludes(perFile.combined, `Invalid value for "max-fetch-bytes" in ${join(tmp, "sub", "goccia.json")}: 3GiB (value is too large)`, "per-file max-fetch-bytes 3GiB");

    // A limit a binary does not apply is ignored in config, not validated.
    writeFileSync(join(tmp, "goccia.json"), '{"timeout": "5x", "max-memory": "64MB", "max-stack": -1}\n');
    const bundle = run(BUNDLER, ["main.js"], { cwd: tmp });
    expectExit(bundle, 0, "Bundler ignores unsupported limits in config");
    const loader = run(LOADER, ["main.js"], { cwd: tmp });
    expectExit(loader, 1, "Loader validates the same config");
  } finally {
    clean(tmp);
  }
}

// -- Config trust ---------------------------------------------------------------------

function expectEmptyDirectory(path: string, label: string): void {
  const entries = Bun.spawnSync(["ls", "-A", path]).stdout.toString().trim();
  if (!isWindows && entries !== "") throw new Error(`${label}: expected ${path} to stay empty, got: ${entries}`);
}

function readJSON(path: string): any {
  return JSON.parse(readFileSync(path, "utf8"));
}

console.log("An untrusted config refuses the run with a report naming the fix...");
{
  const tmp = makeTmp();
  try {
    const real = realpathSync(tmp);
    const project = join(tmp, "project");
    mkdirSync(join(project, "data"), { recursive: true });
    writeFileSync(join(project, "goccia.json"), '{"permissions": {"allow-read": ["./data"]}}\n');
    writeFileSync(join(project, "main.js"), 'console.log("RAN");\n');
    const main = join("project", "main.js");

    // 1. Nothing runs, and the report is exact.
    const refused = run(LOADER, ["--trust-store=trust.json", main], { cwd: tmp });
    expectExit(refused, 2, "untrusted config");
    expectExcludes(refused.stdout, "RAN", "untrusted config runs nothing");
    if (!isWindows) {
      const expected = [
        "Error: 1 config file requests permissions that have not been trusted:",
        "",
        "  project/goccia.json (never trusted)",
        `    allow-read: ${real}/project/data`,
        "",
        `Nothing was run. To trust these requests (stored in ${real}/trust.json):`,
        "  GocciaScriptLoader --trust-store=trust.json --trust project/goccia.json",
        "To accept them for this run only:",
        "  GocciaScriptLoader -P --trust-store=trust.json project/main.js",
        "To run with command-line grants only:",
        "  GocciaScriptLoader --ignore-config-permissions --trust-store=trust.json project/main.js",
        "",
      ].join("\n");
      if (refused.stderr !== expected)
        throw new Error(`untrusted report: expected\n${expected}\ngot\n${refused.stderr}`);
    }
    if (existsSync(join(tmp, "trust.json"))) throw new Error("A refused run must not create the store");

    // 3. --trust needs a terminal or --yes; the store is left alone.
    const unconfirmed = run(LOADER, ["--trust-store=trust.json", "--trust", "project"], { cwd: tmp });
    expectExit(unconfirmed, 2, "--trust without --yes");
    expectIncludes(unconfirmed.stderr, "Error: --trust needs confirmation; re-run with --yes to trust without a prompt", "--trust without --yes");
    expectIncludes(unconfirmed.stdout, "(new)\n    allow-read:", "--trust shows the requests first");
    if (existsSync(join(tmp, "trust.json"))) throw new Error("An unconfirmed --trust must not write the store");

    // 2. --trust <dir> --yes records it, and the run proceeds.
    const trusted = run(LOADER, ["--trust-store=trust.json", "--trust", "project", "--yes"], { cwd: tmp });
    expectExit(trusted, 0, "--trust --yes");
    expectIncludes(trusted.stdout, `Trusted 1 config file in ${real}/trust.json`.replaceAll("/", isWindows ? "\\" : "/"), "--trust --yes");
    const store = readJSON(join(tmp, "trust.json"));
    if (store.version !== 1 || Object.keys(store.trusted).length !== 1)
      throw new Error(`trust store shape: ${JSON.stringify(store)}`);
    const again = run(LOADER, ["--trust-store=trust.json", "--trust", "project", "--yes"], { cwd: tmp });
    expectIncludes(again.stdout, "The config file under project/ is already trusted".replaceAll("/", isWindows ? "\\" : "/"), "--trust twice");
    const ran = run(LOADER, ["--trust-store=trust.json", main], { cwd: tmp });
    expectExit(ran, 0, "trusted run");
    expectIncludes(ran.stdout, "RAN", "trusted run");

    // 4. An edit invalidates the trust and shows what changed.
    writeFileSync(join(project, "goccia.json"), '{"permissions": {"allow-read": ["./data"], "allow-net": ["example.com"]}}\n');
    const changed = run(LOADER, ["--trust-store=trust.json", main], { cwd: tmp });
    expectExit(changed, 2, "changed config");
    expectIncludes(changed.stderr, "goccia.json (changed since trusted ", "changed config");
    expectIncludes(changed.stderr, "\n    allow-read: ", "changed config keeps unchanged lines");
    expectIncludes(changed.stderr, "  + allow-net: example.com", "changed config marks the addition");
    const listed = run(LOADER, ["--trust-store=trust.json", "--list-trusted"], { cwd: tmp });
    expectExit(listed, 0, "--list-trusted");
    expectIncludes(listed.stdout, "Trust store: ", "--list-trusted header");
    expectIncludes(listed.stdout, "  allow-read  (changed)", "--list-trusted marks a change");

    // 20. The same block at another path is not trusted.
    run(LOADER, ["--trust-store=trust.json", "--trust", "project", "--yes"], { cwd: tmp });
    cpSync(project, join(tmp, "copy"), { recursive: true });
    const copied = run(LOADER, ["--trust-store=trust.json", join("copy", "main.js")], { cwd: tmp });
    expectExit(copied, 2, "copied block");
    expectIncludes(copied.stderr, "(never trusted)", "copied block");

    // 13. --untrust, and a trusted config that disappeared.
    const untrusted = run(LOADER, ["--trust-store=trust.json", "--untrust", "copy"], { cwd: tmp });
    expectIncludes(untrusted.stdout, "No trusted config at or under copy", "--untrust of nothing");
    run(LOADER, ["--trust-store=trust.json", "--trust", "copy", "--yes"], { cwd: tmp });
    clean(join(tmp, "copy"));
    const missing = run(LOADER, ["--trust-store=trust.json", "--list-trusted"], { cwd: tmp });
    expectIncludes(missing.stdout, "(missing)", "--list-trusted marks a missing config");
    const removed = run(LOADER, ["--trust-store=trust.json", "--untrust", "copy"], { cwd: tmp });
    expectIncludes(removed.stdout, "Removed trust for 1 config file under copy", "--untrust");
    const after = run(LOADER, ["--trust-store=trust.json", "--list-trusted"], { cwd: tmp });
    expectExcludes(after.stdout, "(missing)", "--untrust removed the entry");

    // An empty path names nothing: it is a usage error, not "everything under
    // the working directory".
    for (const args of [["--untrust="], ["--untrust", ""], ["--trust", ""], ["--trust="]]) {
      const empty = run(LOADER, ["--trust-store=trust.json", ...args], { cwd: tmp });
      expectExit(empty, 2, `${args.join(" ")} (empty path)`);
      expectIncludes(empty.stderr, `Error: ${args[0].replace("=", "")} needs a path`, `${args.join(" ")} (empty path)`);
    }
    expectIncludes(run(LOADER, ["--trust-store=trust.json", "--list-trusted"], { cwd: tmp }).stdout, "project", "empty --untrust removed nothing");

    // --untrust reports a removal only once the store is saved.
    writeFileSync(join(tmp, "trust.json.lock"), "");
    const locked = run(LOADER, ["--trust-store=trust.json", "--untrust", "project"], { cwd: tmp });
    expectExit(locked, 1, "--untrust with a held lock");
    expectExcludes(locked.stdout, "Removed trust", "--untrust with a held lock");
    rmSync(join(tmp, "trust.json.lock"));
  } finally {
    clean(tmp);
  }
}

console.log("-P and --ignore-config-permissions decide for one run...");
{
  const tmp = makeTmp();
  try {
    const project = join(tmp, "project");
    mkdirSync(project);
    writeFileSync(join(project, "goccia.json"), '{"permissions": {"allow-net": ["127.0.0.1"], "deny-net": ["10.0.0.0/8"]}}\n');
    const probe = (url: string) =>
      `try { fetch(${JSON.stringify(url)}).then(() => console.log("reached"), (e) => console.log(e.name, e.message)); } catch (e) { console.log(e.name, e.message); }\n`;
    writeFileSync(join(project, "loopback.js"), probe("http://127.0.0.1:1/"));
    writeFileSync(join(project, "ten.js"), probe("http://10.1.2.3:1/"));

    // 5. -P accepts the request; it reads and writes no store.
    const accepted = run(LOADER, ["-P", join(project, "loopback.js")]);
    expectExit(accepted, 0, "-P");
    expectIncludes(accepted.stdout, "TypeError", "-P grants the config's allow");
    const acceptedLong = run(LOADER, ["--accept-config-permissions", "--trust-store=never.json", join(project, "loopback.js")], { cwd: tmp });
    expectExcludes(acceptedLong.stdout, "PermissionDenied", "--accept-config-permissions");
    if (existsSync(join(tmp, "never.json"))) throw new Error("-P must not create a store");
    expectEmptyDirectory(privateHome, "-P leaves HOME alone");

    // 6. --ignore-config-permissions drops the grant but keeps the deny.
    const ignored = run(LOADER, ["--ignore-config-permissions", join(project, "loopback.js")]);
    expectExit(ignored, 0, "--ignore-config-permissions");
    expectIncludes(ignored.stdout, "PermissionDenied net: 127.0.0.1:1", "--ignore-config-permissions drops the allow");
    const ignoredDeny = run(LOADER, ["--ignore-config-permissions", "--allow-net=10.0.0.0/8", join(project, "ten.js")]);
    expectIncludes(ignoredDeny.stdout, "PermissionDenied net: 10.1.2.3:1", "a config deny subtracts from a CLI allow");

    // 7. A command-line deny subtracts from a trusted allow.
    run(LOADER, ["--trust-store=trust.json", "--trust", "project", "--yes"], { cwd: tmp });
    const trustedDeny = run(LOADER, ["--trust-store=trust.json", "--deny-net=127.0.0.1", join("project", "loopback.js")], { cwd: tmp });
    expectExit(trustedDeny, 0, "CLI deny over trusted allow");
    expectIncludes(trustedDeny.stdout, "PermissionDenied net: 127.0.0.1:1", "CLI deny over trusted allow");

    // 18. -P and --ignore-config-permissions cannot be combined.
    const both = run(LOADER, ["-P", "--ignore-config-permissions", join(project, "loopback.js")]);
    expectExit(both, 2, "-P with --ignore-config-permissions");
    expectIncludes(both.stderr, "Error: -P and --ignore-config-permissions cannot be combined", "-P with --ignore-config-permissions");
    const modeWithInput = run(LOADER, ["--trust", "project", join(project, "loopback.js")], { cwd: tmp });
    expectExit(modeWithInput, 2, "--trust with input files");
    expectIncludes(modeWithInput.stderr, "Error: --trust cannot be combined with input files; run it on its own", "--trust with input files");
  } finally {
    clean(tmp);
  }
}

console.log("Trust follows each file's own config...");
{
  const tmp = makeTmp();
  try {
    // 10. A deny-only config needs no trust.
    mkdirSync(join(tmp, "deny"));
    writeFileSync(join(tmp, "deny", "goccia.json"), '{"permissions": {"deny-read": true}}\n');
    writeFileSync(join(tmp, "deny", "main.js"), 'console.log("RAN");\n');
    const denyOnly = run(LOADER, ["--trust-store=trust.json", join("deny", "main.js")], { cwd: tmp });
    expectExit(denyOnly, 0, "deny-only config");
    expectIncludes(denyOnly.stdout, "RAN", "deny-only config");

    // 8. Per-file configs: only the untrusted one is reported, and a change to
    // an extends base invalidates its child.
    for (const name of ["a", "b"]) {
      mkdirSync(join(tmp, "suite", name), { recursive: true });
      writeFileSync(join(tmp, "suite", name, "x.test.js"), 'test("x", () => { expect(1).toBe(1); });\n');
    }
    writeFileSync(join(tmp, "suite", "base.json"), '{"permissions": {"allow-net": ["a.test"]}}\n');
    writeFileSync(join(tmp, "suite", "a", "goccia.json"), '{"extends": "../base.json", "permissions": {"allow-read": ["."]}}\n');
    writeFileSync(join(tmp, "suite", "b", "goccia.json"), '{"permissions": {"allow-net": ["b.test"]}}\n');
    run(TESTRUNNER, ["--trust-store=trust.json", "--trust", join("suite", "a"), "--yes"], { cwd: tmp });
    const partial = run(TESTRUNNER, ["--trust-store=trust.json", "suite", "--no-progress"], { cwd: tmp });
    expectExit(partial, 2, "one untrusted config");
    expectIncludes(partial.stderr, "Error: 1 config file requests", "one untrusted config");
    expectIncludes(partial.stderr, join("suite", "b", "goccia.json"), "one untrusted config");
    expectExcludes(partial.stderr, join("suite", "a", "goccia.json"), "one untrusted config");
    run(TESTRUNNER, ["--trust-store=trust.json", "--trust", "suite", "--yes"], { cwd: tmp });
    expectIncludes(run(TESTRUNNER, ["--trust-store=trust.json", "suite", "--no-progress"], { cwd: tmp }).stdout, "Passed: 2", "trusted suite");
    writeFileSync(join(tmp, "suite", "base.json"), '{"permissions": {"allow-net": ["a.test", "c.test"]}}\n');
    const baseChanged = run(TESTRUNNER, ["--trust-store=trust.json", "suite", "--no-progress"], { cwd: tmp });
    expectExit(baseChanged, 2, "extends base changed");
    expectIncludes(baseChanged.stderr, `${join("suite", "a", "goccia.json")} (changed since trusted`, "extends base changed");
    expectIncludes(baseChanged.stderr, "  + allow-net: a.test, c.test", "extends base changed");

    // 11. unsafe-function-constructor in config needs trust; the flag does not.
    mkdirSync(join(tmp, "unsafe"));
    writeFileSync(join(tmp, "unsafe", "goccia.json"), '{"unsafe-function-constructor": true}\n');
    writeFileSync(join(tmp, "unsafe", "main.js"), 'console.log(new Function("return 6 * 7")());\n');
    const unsafeUntrusted = run(LOADER, ["--trust-store=trust.json", join("unsafe", "main.js")], { cwd: tmp });
    expectExit(unsafeUntrusted, 2, "config unsafe-function-constructor");
    expectIncludes(unsafeUntrusted.stderr, "    unsafe-function-constructor: true", "config unsafe-function-constructor");
    const unsafeIgnored = run(LOADER, ["--ignore-config-permissions", join("unsafe", "main.js")], { cwd: tmp });
    expectExcludes(unsafeIgnored.stdout, "42", "ignored unsafe-function-constructor");
    const unsafeFlag = run(LOADER, ["--ignore-config-permissions", "--unsafe-function-constructor", join("unsafe", "main.js")], { cwd: tmp });
    expectExit(unsafeFlag, 0, "--unsafe-function-constructor");
    expectIncludes(unsafeFlag.stdout, "42", "--unsafe-function-constructor");
    expectIncludes(run(LOADER, ["-P", join("unsafe", "main.js")], { cwd: tmp }).stdout, "42", "-P unsafe-function-constructor");

    // 15. The bundler runs nothing, so a declaring config only warns.
    const bundled = run(BUNDLER, [join("unsafe", "main.js"), "--output=out.gbc"], { cwd: tmp });
    expectExit(bundled, 0, "Bundler with unsafe config");
    expectIncludes(bundled.stderr, "requests unsafe-function-constructor, which GocciaBundler cannot grant; ignoring it", "Bundler warns");

    // The bundler checks every file argument's config before emitting any:
    // no partial output, and an audit event for a single file too.
    mkdirSync(join(tmp, "bundle", "good"), { recursive: true });
    mkdirSync(join(tmp, "bundle", "bad"), { recursive: true });
    writeFileSync(join(tmp, "bundle", "good", "a.js"), "1;\n");
    writeFileSync(join(tmp, "bundle", "bad", "b.js"), "2;\n");
    writeFileSync(join(tmp, "bundle", "bad", "goccia.json"), '{"permissions": {"deny-nett": true}}\n');
    mkdirSync(join(tmp, "bundle", "out"));
    const partial = run(BUNDLER, [join("bundle", "good", "a.js"), join("bundle", "bad", "b.js"), `--output=${join("bundle", "out")}`], { cwd: tmp });
    expectExit(partial, 2, "Bundler with a malformed config among its files");
    if (existsSync(join(tmp, "bundle", "out", "a.gbc"))) throw new Error("The bundler emitted a.gbc before refusing b's config");
    const bundleAudit = run(BUNDLER, [join("unsafe", "main.js"), "--output=single.gbc", "--audit-log=bundle.jsonl"], { cwd: tmp });
    expectExit(bundleAudit, 0, "Bundler audit for a single file");
    const bundleEvents = readFileSync(join(tmp, "bundle.jsonl"), "utf8");
    expectIncludes(bundleEvents, '"kind":"config.permissions"', "Bundler audits a single file's config");

    // 17. The REPL refuses before its prompt.
    const repl = run(REPL, ["--trust-store=trust.json"], { cwd: join(tmp, "unsafe"), stdin: "1 + 1\n" });
    expectExit(repl, 2, "REPL with an untrusted config");
    expectExcludes(repl.stdout, "Goccia REPL", "REPL refuses before the prompt");

    // 12. The trust options, and top-level grants, are command-line-only.
    mkdirSync(join(tmp, "keys"));
    writeFileSync(join(tmp, "keys", "main.js"), "1;\n");
    for (const key of ['"allow-net": ["a.test"]', '"accept-config-permissions": true', '"trust-store": "x.json"', '"ignore-config-permissions": true']) {
      writeFileSync(join(tmp, "keys", "goccia.json"), `{${key}}\n`);
      const result = run(LOADER, [join("keys", "main.js")], { cwd: tmp });
      expectExit(result, 2, `config ${key}`);
      expectIncludes(result.stderr, "can only be given on the command line", `config ${key}`);
    }

    // 21. Trusting through a symlink covers the real path.
    if (!isWindows) {
      mkdirSync(join(tmp, "real"));
      writeFileSync(join(tmp, "real", "goccia.json"), '{"permissions": {"allow-net": ["a.test"]}}\n');
      writeFileSync(join(tmp, "real", "main.js"), 'console.log("RAN");\n');
      symlinkSync(join(tmp, "real"), join(tmp, "link"));
      run(LOADER, ["--trust-store=trust.json", "--trust", join("link", "goccia.json"), "--yes"], { cwd: tmp });
      const viaReal = run(LOADER, ["--trust-store=trust.json", join("real", "main.js")], { cwd: tmp });
      expectExit(viaReal, 0, "trusted through a symlink");
      expectIncludes(viaReal.stdout, "RAN", "trusted through a symlink");

      // A symlinked config FILE governs the files beside the link, so it does
      // not inherit the trust of the config it points at.
      mkdirSync(join(tmp, "trusted"));
      mkdirSync(join(tmp, "evil"));
      writeFileSync(join(tmp, "trusted", "goccia.json"), '{"unsafe-function-constructor": true}\n');
      symlinkSync(join("..", "trusted", "goccia.json"), join(tmp, "evil", "goccia.json"));
      writeFileSync(join(tmp, "evil", "a.js"), 'console.log("EVIL", new Function("return 7")());\n');
      run(LOADER, ["--trust-store=trust.json", "--trust", "trusted", "--yes"], { cwd: tmp });
      const evil = run(LOADER, ["--trust-store=trust.json", join("evil", "a.js")], { cwd: tmp });
      expectExit(evil, 2, "symlinked config file");
      expectExcludes(evil.stdout, "EVIL", "symlinked config file runs nothing");
      expectIncludes(evil.stderr, `${join("evil", "goccia.json")} (never trusted)`, "symlinked config file has its own trust");
    }
  } finally {
    clean(tmp);
  }
}

console.log("Unreadable trust stores are errors, not trust...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "project"));
    writeFileSync(join(tmp, "project", "goccia.json"), '{"permissions": {"allow-net": ["a.test"]}}\n');
    writeFileSync(join(tmp, "project", "main.js"), "1;\n");

    // 14. A corrupt store refuses --trust and leaves runs untrusted.
    writeFileSync(join(tmp, "corrupt.json"), "{ not json");
    const corruptTrust = run(LOADER, ["--trust-store=corrupt.json", "--trust", "project", "--yes"], { cwd: tmp });
    expectExit(corruptTrust, 1, "--trust into a corrupt store");
    expectIncludes(corruptTrust.combined, "corrupt.json is not valid JSON; fix or delete it", "--trust into a corrupt store");
    if (readFileSync(join(tmp, "corrupt.json"), "utf8") !== "{ not json") throw new Error("--trust overwrote a corrupt store");
    const corruptRun = run(LOADER, ["--trust-store=corrupt.json", join("project", "main.js")], { cwd: tmp });
    expectExit(corruptRun, 2, "run with a corrupt store");
    expectIncludes(corruptRun.stderr, "corrupt.json is not valid JSON; fix or delete it. Then, to trust these requests:", "run with a corrupt store");

    writeFileSync(join(tmp, "newer.json"), '{"version": 2, "trusted": {}}\n');
    const newer = run(LOADER, ["--trust-store=newer.json", "--list-trusted"], { cwd: tmp });
    expectExit(newer, 1, "newer store");
    expectIncludes(newer.combined, "newer.json was written by a newer GocciaScript (version 2); upgrade GocciaScript or remove the file", "newer store");

    // Without a HOME there is no default store to name.
    const noHomeEnv = { ...isolatedEnv };
    delete noHomeEnv.HOME;
    delete noHomeEnv.USERPROFILE;
    const noHome = Bun.spawnSync([resolve(LOADER), "--list-trusted"], { cwd: tmp, env: noHomeEnv, stdout: "pipe", stderr: "pipe" });
    if (!isWindows && !(noHome.stdout.toString() + noHome.stderr.toString()).includes("cannot locate the per-user trust store (HOME is not set); pass --trust-store=<path>"))
      throw new Error(`no HOME: ${noHome.stdout}${noHome.stderr}`);
  } finally {
    clean(tmp);
  }
}

console.log("Runs read a store another --trust is rewriting...");
{
  const tmp = makeTmp();
  try {
    // 9. Eight trusted folders run on four workers while --trust rewrites the
    // store: readers never lock and always see a whole file.
    for (let i = 0; i < 8; i++) {
      const dir = join(tmp, "suite", `f${i}`);
      mkdirSync(dir, { recursive: true });
      writeFileSync(join(dir, "goccia.json"), `{"permissions": {"allow-net": ["h${i}.test"]}}\n`);
      writeFileSync(join(dir, "x.test.js"), 'test("x", () => { expect(1).toBe(1); });\n');
    }
    run(TESTRUNNER, ["--trust-store=trust.json", "--trust", "suite", "--yes"], { cwd: tmp });
    for (let i = 0; i < 4; i++) {
      mkdirSync(join(tmp, "other", `o${i}`), { recursive: true });
      writeFileSync(join(tmp, "other", `o${i}`, "goccia.json"), `{"permissions": {"allow-net": ["o${i}.test"]}}\n`);
    }
    const runs = [0, 1, 2].map(() => runAsync(TESTRUNNER, ["--trust-store=trust.json", "suite", "--jobs=4", "--no-progress"], tmp));
    const writers = [0, 1, 2, 3].map((i) =>
      runAsync(TESTRUNNER, ["--trust-store=trust.json", "--trust", join("other", `o${i}`), "--yes"], tmp));
    for (const result of await Promise.all(runs)) {
      expectExit(result, 0, "run during --trust");
      expectIncludes(result.stdout, "Passed: 8", "run during --trust");
    }
    for (const result of await Promise.all(writers)) expectExit(result, 0, "concurrent --trust");
    const store = readJSON(join(tmp, "trust.json"));
    if (Object.keys(store.trusted).length !== 12)
      throw new Error(`concurrent --trust lost entries: ${Object.keys(store.trusted).length}`);
  } finally {
    clean(tmp);
  }
}

console.log("Audit records config trust and where capabilities came from...");
{
  const tmp = makeTmp();
  try {
    // 19. config.permissions and capabilities.effective provenance.
    mkdirSync(join(tmp, "project"));
    writeFileSync(join(tmp, "project", "goccia.json"), '{"permissions": {"allow-net": ["a.test"]}}\n');
    writeFileSync(join(tmp, "project", "main.js"), "1;\n");
    const audited = run(LOADER, ["-P", "--allow-read=project", "--audit-log=audit.jsonl", join("project", "main.js")], { cwd: tmp });
    expectExit(audited, 0, "audited run");
    const events = readFileSync(join(tmp, "audit.jsonl"), "utf8").trim().split("\n").map((line) => JSON.parse(line));
    const config = events.find((event) => event.kind === "config.permissions");
    if (!config || config.decision !== "allow" || config.reason !== "accepted for this run (-P)" || !String(config.subject).endsWith("goccia.json"))
      throw new Error(`config.permissions event: ${JSON.stringify(events)}`);
    const effective = events.find((event) => event.kind === "capabilities.effective");
    if (!effective || !String(effective.reason).startsWith("cli --allow-read=project; config ") ||
        !String(effective.reason).endsWith("goccia.json accepted for this run (-P)"))
      throw new Error(`capabilities.effective provenance: ${JSON.stringify(effective)}`);

    run(LOADER, ["--trust-store=trust.json", "--trust", "project", "--yes"], { cwd: tmp });
    run(LOADER, ["--trust-store=trust.json", "--audit-log=trusted.jsonl", join("project", "main.js")], { cwd: tmp });
    const trusted = readFileSync(join(tmp, "trusted.jsonl"), "utf8").trim().split("\n").map((line) => JSON.parse(line));
    const trustedEvent = trusted.find((event) => event.kind === "config.permissions");
    if (!trustedEvent || !/^trusted sha256:[0-9a-f]{64}$/.test(trustedEvent.reason))
      throw new Error(`trusted config.permissions event: ${JSON.stringify(trusted)}`);

    // A deny-only config needs no trust but still shapes the set, so the
    // provenance names it.
    mkdirSync(join(tmp, "denies"));
    writeFileSync(join(tmp, "denies", "goccia.json"), '{"permissions": {"deny-net": ["example.com"]}}\n');
    writeFileSync(join(tmp, "denies", "main.js"), "1;\n");
    run(LOADER, ["--audit-log=denies.jsonl", join("denies", "main.js")], { cwd: tmp });
    const deniesEffective = readFileSync(join(tmp, "denies.jsonl"), "utf8").trim().split("\n").map((line) => JSON.parse(line))
      .find((event) => event.kind === "capabilities.effective");
    if (!deniesEffective || !String(deniesEffective.reason).startsWith("config ") ||
        !String(deniesEffective.reason).endsWith("goccia.json denies only"))
      throw new Error(`deny-only provenance: ${JSON.stringify(deniesEffective)}`);

    run(LOADER, ["--trust-store=none.json", "--audit-log=denied.jsonl", join("project", "main.js")], { cwd: tmp });
    const denied = readFileSync(join(tmp, "denied.jsonl"), "utf8").trim().split("\n").map((line) => JSON.parse(line));
    if (!denied.some((event) => event.kind === "config.permissions" && event.decision === "deny" && event.reason === "not trusted"))
      throw new Error(`denied config.permissions event: ${JSON.stringify(denied)}`);
  } finally {
    clean(tmp);
  }
}

console.log("GocciaWasmTestRunner takes -P instead of a trust store...");
if (existsSync(resolve(WASMTESTRUNNER))) {
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "unsafe"));
    writeFileSync(join(tmp, "unsafe", "goccia.json"), '{"unsafe-function-constructor": true}\n');
    writeFileSync(join(tmp, "unsafe", "x.test.js"), 'test("x", () => { expect(new Function("return 1")()).toBe(1); });\n');
    writeFileSync(join(tmp, "manifest.txt"), `${join(tmp, "unsafe", "x.test.js")}\n`);
    const refused = run(WASMTESTRUNNER, [join(tmp, "manifest.txt")]);
    expectExit(refused, 1, "Wasm runner without -P");
    expectIncludes(refused.stdout, `FILEERROR ${join(tmp, "unsafe", "x.test.js")} :: ${join(tmp, "unsafe", "goccia.json")} requests permissions; pass -P to accept them (GocciaWasmTestRunner has no trust store)`, "Wasm runner without -P");
    const accepted = run(WASMTESTRUNNER, ["-P", join(tmp, "manifest.txt")]);
    expectExit(accepted, 0, "Wasm runner -P");
    expectIncludes(accepted.stdout, "PASS ", "Wasm runner -P");
  } finally {
    clean(tmp);
  }
}

clean(privateHome);
console.log("\nAll test-cli-permissions.ts tests passed.");
