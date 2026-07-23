#!/usr/bin/env bun

import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";

const HARNESS_DIRECTORY = "scripts/es_toolkit_harness";
const DEFAULT_MANIFEST = join(HARNESS_DIRECTORY, "manifest.json");
const RESULT_MARKER = "GocciaEsToolkitResult:";
const ENVIRONMENT_MARKER = "GocciaEsToolkitEnvironment:";
const MODES = ["interpreted", "bytecode"] as const;
const NPM_REGISTRY_HOST = "registry.npmjs.org";
const MAX_REDIRECTS = 10;

type Mode = (typeof MODES)[number];
type JsonObject = Record<string, any>;

interface Options {
  manifest: string;
  packageDir?: string;
  tarballCache?: string;
  goccia: string;
  output?: string;
  timeoutSeconds: number;
  printCacheKey: boolean;
}

interface Manifest {
  schemaVersion: number;
  upstream: {
    name: string;
    version: string;
    repository: string;
    commit: string;
    tarball: string;
    integrity: string;
  };
  engineFlags: string[];
  expectedEnvironment: {
    functionConstructorErrorPattern: string;
  };
  modules: Record<string, string>;
  probes: Array<{
    id: string;
    file: string;
    knownBytecodeDivergence?: {
      errorPattern: string;
      reason: string;
    };
  }>;
  environmentProbe: string;
}

interface ProbeRun {
  mode: Mode;
  transport: "completed" | "timeout";
  exitCode: number | null;
  stdout: string;
  stderr: string;
  marker: JsonObject | null;
  markerError: string | null;
}

interface PreparedPackage {
  root: string;
  temporaryDirectory?: string;
}

function printHelp(): void {
  console.log(`Run bounded, pinned es-toolkit compatibility probes in both Goccia execution modes.

Usage: bun run scripts/run_es_toolkit_validation.ts [options]

Options:
  --manifest=<path>          Use this validation manifest.
  --package-dir=<path>       Use an already extracted npm package directory.
  --tarball-cache=<path>     Use or populate a verified npm tarball cache.
  --goccia=<path>            Use this GocciaScriptLoader binary.
  --output=<path>            Write the normalized JSON report to this path.
  --timeout-seconds=<n>      Set the per-probe timeout (default: 30).
  --print-cache-key          Print the integrity-derived workflow cache key.
  -h, --help                 Show this help.`);
}

function optionValue(argv: string[], index: number, name: string): [string, number] {
  const argument = argv[index];
  const prefix = `${name}=`;
  if (argument.startsWith(prefix)) return [argument.slice(prefix.length), index];
  if (argument === name && index + 1 < argv.length) return [argv[index + 1], index + 1];
  throw new Error(`${name} requires a value`);
}

export function parseArgs(argv: string[]): Options | null {
  const options: Options = {
    manifest: DEFAULT_MANIFEST,
    goccia: "build/GocciaScriptLoader",
    timeoutSeconds: 30,
    printCacheKey: false,
  };

  for (let index = 0; index < argv.length; index++) {
    const argument = argv[index];
    if (argument === "-h" || argument === "--help") {
      printHelp();
      return null;
    }
    if (argument === "--print-cache-key") {
      options.printCacheKey = true;
      continue;
    }

    const definitions: Array<[string, keyof Options]> = [
      ["--manifest", "manifest"],
      ["--package-dir", "packageDir"],
      ["--tarball-cache", "tarballCache"],
      ["--goccia", "goccia"],
      ["--output", "output"],
    ];
    const definition = definitions.find(([name]) => argument === name || argument.startsWith(`${name}=`));
    if (definition) {
      const [value, consumedIndex] = optionValue(argv, index, definition[0]);
      (options as any)[definition[1]] = value;
      index = consumedIndex;
      continue;
    }
    if (argument === "--timeout-seconds" || argument.startsWith("--timeout-seconds=")) {
      const [value, consumedIndex] = optionValue(argv, index, "--timeout-seconds");
      options.timeoutSeconds = Number(value);
      index = consumedIndex;
      continue;
    }
    throw new Error(`unknown argument: ${argument}`);
  }

  if (!Number.isInteger(options.timeoutSeconds) || options.timeoutSeconds < 1) {
    throw new Error("--timeout-seconds must be a positive integer");
  }
  if (options.packageDir && options.tarballCache) {
    throw new Error("--package-dir and --tarball-cache cannot be used together");
  }
  return options;
}

export function loadManifest(path: string): Manifest {
  const manifest = JSON.parse(readFileSync(path, "utf8")) as Manifest;
  if (manifest.schemaVersion !== 1) {
    throw new Error("es-toolkit manifest schemaVersion must be 1");
  }
  return manifest;
}

export function validateTarballUrl(url: string): void {
  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch {
    throw new Error(`es-toolkit tarball URL must use https://${NPM_REGISTRY_HOST}`);
  }
  if (parsed.protocol !== "https:" || parsed.hostname !== NPM_REGISTRY_HOST) {
    throw new Error(`es-toolkit tarball URL must use https://${NPM_REGISTRY_HOST}`);
  }
}

export async function downloadTarball(url: string, timeoutSeconds = 30): Promise<Uint8Array> {
  let currentUrl = url;
  for (let redirects = 0; redirects <= MAX_REDIRECTS; redirects++) {
    validateTarballUrl(currentUrl);
    const response = await fetch(currentUrl, {
      redirect: "manual",
      signal: AbortSignal.timeout(timeoutSeconds * 1000),
    });
    if ([301, 302, 303, 307, 308].includes(response.status)) {
      const location = response.headers.get("location");
      await response.body?.cancel();
      if (!location) throw new Error("es-toolkit tarball redirect is missing a Location header");
      if (redirects === MAX_REDIRECTS) throw new Error("es-toolkit tarball exceeded 10 redirects");
      currentUrl = new URL(location, currentUrl).toString();
      validateTarballUrl(currentUrl);
      continue;
    }
    if (!response.ok) {
      await response.body?.cancel();
      throw new Error(`es-toolkit tarball download failed with HTTP ${response.status}`);
    }
    return response.bytes();
  }
  throw new Error("es-toolkit tarball exceeded 10 redirects");
}

export function verifyIntegrity(data: Uint8Array, integrity: string): void {
  const separator = integrity.indexOf("-");
  const algorithm = separator < 0 ? integrity : integrity.slice(0, separator);
  const expected = separator < 0 ? "" : integrity.slice(separator + 1);
  if (algorithm !== "sha512") {
    throw new Error(`unsupported npm integrity algorithm: ${algorithm}`);
  }
  const actual = new Bun.CryptoHasher("sha512").update(data).digest("base64");
  if (actual !== expected) {
    throw new Error("downloaded es-toolkit package does not match the pinned npm integrity");
  }
}

export function integrityCacheKey(integrity: string): string {
  const digest = new Bun.CryptoHasher("sha256").update(integrity).digest("hex");
  return `es-toolkit-${digest}`;
}

export function resolveArchiveEntry(destination: string, memberName: string): string {
  const normalizedName = memberName.replaceAll("\\", "/");
  if (normalizedName.startsWith("/") || normalizedName.startsWith("//") || /^[A-Za-z]:\//.test(normalizedName)) {
    throw new Error(`unsafe path in es-toolkit package: ${memberName}`);
  }
  const target = resolve(destination, ...normalizedName.split("/"));
  const relativePath = relative(destination, target);
  if (!relativePath || relativePath === ".." || relativePath.startsWith(`..${sep}`) || isAbsolute(relativePath)) {
    throw new Error(`unsafe path in es-toolkit package: ${memberName}`);
  }
  return target;
}

export async function extractVerifiedPackage(data: Uint8Array, destination: string): Promise<string> {
  const destinationRoot = resolve(destination);
  mkdirSync(destinationRoot, { recursive: true });
  // Bun exposes regular files only; create directories ourselves so archive
  // symlinks, devices, and other special entries are never materialized.
  const files = await new Bun.Archive(data).files();
  if (files.size === 0) throw new Error("es-toolkit package archive contains no regular files");

  for (const [memberName, file] of files) {
    const target = resolveArchiveEntry(destinationRoot, memberName);
    mkdirSync(dirname(target), { recursive: true });
    await Bun.write(target, file);
  }
  return join(destinationRoot, "package");
}

async function preparePackage(
  manifest: Manifest,
  packageDirectory?: string,
  tarballCache?: string,
): Promise<PreparedPackage> {
  let root: string;
  let temporaryDirectory: string | undefined;
  try {
    if (packageDirectory) {
      root = resolve(packageDirectory);
    } else {
      temporaryDirectory = mkdtempSync(join(tmpdir(), "goccia-es-toolkit-"));
      const cacheIsFile = Boolean(tarballCache && existsSync(tarballCache) && statSync(tarballCache).isFile());
      const data = cacheIsFile
        ? new Uint8Array(readFileSync(tarballCache!))
        : await downloadTarball(manifest.upstream.tarball);
      verifyIntegrity(data, manifest.upstream.integrity);
      if (tarballCache && !existsSync(tarballCache)) {
        mkdirSync(dirname(tarballCache), { recursive: true });
        writeFileSync(tarballCache, data);
      }
      root = await extractVerifiedPackage(data, temporaryDirectory);
    }

    const packageMetadata = JSON.parse(readFileSync(join(root, "package.json"), "utf8"));
    if (packageMetadata.name !== manifest.upstream.name || packageMetadata.version !== manifest.upstream.version) {
      throw new Error(`expected es-toolkit ${manifest.upstream.version} at ${root}`);
    }
    return { root, temporaryDirectory };
  } catch (error) {
    if (temporaryDirectory) rmSync(temporaryDirectory, { recursive: true, force: true });
    throw error;
  }
}

function writeImportMap(manifest: Manifest, packageRoot: string, directory: string): string {
  const imports = Object.fromEntries(
    Object.entries(manifest.modules).map(([specifier, relativePath]) => [specifier, resolve(packageRoot, relativePath)]),
  );
  const importMap = join(directory, "importmap.json");
  writeFileSync(importMap, `${JSON.stringify({ imports }, null, 2)}\n`);
  return importMap;
}

export function parseSingleMarker(stdout: string, marker: string): [JsonObject | null, string | null] {
  const markerLines = stdout
    .split(/\r?\n/)
    .filter((line) => line.startsWith(marker))
    .map((line) => line.slice(marker.length));
  if (markerLines.length !== 1) {
    return [null, `expected one ${marker} marker, found ${markerLines.length}`];
  }
  try {
    return [JSON.parse(markerLines[0]), null];
  } catch (error) {
    return [null, `invalid ${marker} JSON: ${error instanceof Error ? error.message : String(error)}`];
  }
}

async function runProbe(
  goccia: string,
  probePath: string,
  mode: Mode,
  importMap: string,
  engineFlags: string[],
  marker: string,
  timeoutSeconds: number,
): Promise<ProbeRun> {
  const subprocess = Bun.spawn(
    [goccia, probePath, `--mode=${mode}`, `--import-map=${importMap}`, ...engineFlags],
    { stdout: "pipe", stderr: "pipe" },
  );
  const stdoutPromise = new Response(subprocess.stdout).text();
  const stderrPromise = new Response(subprocess.stderr).text();
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
    subprocess.kill("SIGKILL");
  }, timeoutSeconds * 1000);
  const exitCode = await subprocess.exited;
  clearTimeout(timer);
  const [stdout, stderr] = await Promise.all([stdoutPromise, stderrPromise]);

  if (timedOut) {
    return {
      mode,
      transport: "timeout",
      exitCode: null,
      stdout,
      stderr,
      marker: null,
      markerError: `probe exceeded ${timeoutSeconds}s`,
    };
  }

  const [parsedMarker, markerError] = parseSingleMarker(stdout, marker);
  return {
    mode,
    transport: "completed",
    exitCode,
    stdout,
    stderr,
    marker: parsedMarker,
    markerError,
  };
}

export function isNativeOrHarnessFailure(result: ProbeRun): boolean {
  return result.transport !== "completed" || result.exitCode === null || result.exitCode < 0 || result.exitCode > 1;
}

export function classifySemanticProbe(
  probe: Manifest["probes"][number],
  runs: Record<Mode, ProbeRun>,
): [string, string] {
  const interpreted = runs.interpreted;
  const bytecode = runs.bytecode;
  if (isNativeOrHarnessFailure(interpreted) || isNativeOrHarnessFailure(bytecode)) {
    return ["harness-failure", "probe timed out, crashed, or exited outside the JavaScript error contract"];
  }
  if (interpreted.marker === null && bytecode.marker === null) {
    return ["harness-failure", "neither mode produced a result marker"];
  }
  if (interpreted.marker === null || bytecode.marker === null) {
    return ["bytecode-divergence", "only one execution mode completed the probe contract"];
  }
  if (interpreted.marker.id !== probe.id || bytecode.marker.id !== probe.id) {
    return ["harness-failure", "probe marker id does not match the manifest"];
  }
  const interpretedStatus = interpreted.marker.status;
  const bytecodeStatus = bytecode.marker.status;
  if (!["pass", "fail"].includes(interpretedStatus) || !["pass", "fail"].includes(bytecodeStatus)) {
    return ["harness-failure", "probe marker status must be pass or fail"];
  }
  if (interpretedStatus === "pass" && bytecodeStatus === "pass") {
    return ["semantic-pass", "both execution modes passed"];
  }
  if (interpretedStatus === "fail" && bytecodeStatus === "fail") {
    return ["semantic-failure", "both execution modes reported the same semantic class of failure"];
  }
  return ["bytecode-divergence", "execution modes produced different semantic outcomes"];
}

function compactRun(result: ProbeRun): JsonObject {
  return {
    transport: result.transport,
    exitCode: result.exitCode,
    marker: result.marker,
    markerError: result.markerError,
    stdout: result.stdout.trim(),
    stderr: result.stderr.trim(),
  };
}

export function matchesKnownBytecodeDivergence(
  probe: Manifest["probes"][number],
  classification: string,
  runs: Record<Mode, ProbeRun>,
): boolean {
  const expected = probe.knownBytecodeDivergence;
  if (classification !== "bytecode-divergence" || !expected) return false;
  const interpretedMarker = runs.interpreted.marker ?? {};
  const bytecodeMarker = runs.bytecode.marker ?? {};
  return interpretedMarker.status === "pass"
    && bytecodeMarker.status === "fail"
    && new RegExp(expected.errorPattern).test(bytecodeMarker.error ?? "");
}

async function buildReport(
  manifest: Manifest,
  goccia: string,
  packageRoot: string,
  importMap: string,
  timeoutSeconds: number,
): Promise<JsonObject> {
  const semanticResults: JsonObject[] = [];
  const summary: Record<string, any[]> = {
    semanticPasses: [],
    semanticFailures: [],
    bytecodeDivergences: [],
    disabledCapabilities: [],
    hostGlobals: [],
    harnessFailures: [],
  };

  for (const probe of manifest.probes) {
    const runs = {} as Record<Mode, ProbeRun>;
    for (const mode of MODES) {
      runs[mode] = await runProbe(
        goccia,
        join(HARNESS_DIRECTORY, probe.file),
        mode,
        importMap,
        manifest.engineFlags,
        RESULT_MARKER,
        timeoutSeconds,
      );
    }
    const [classification, reason] = classifySemanticProbe(probe, runs);
    const knownDivergence = matchesKnownBytecodeDivergence(probe, classification, runs);
    semanticResults.push({
      id: probe.id,
      classification,
      reason,
      knownBytecodeDivergence: probe.knownBytecodeDivergence ?? null,
      matchesKnownBytecodeDivergence: knownDivergence,
      modes: Object.fromEntries(MODES.map((mode) => [mode, compactRun(runs[mode])])),
    });
    if (classification === "semantic-pass") summary.semanticPasses.push(probe.id);
    else if (classification === "semantic-failure") summary.semanticFailures.push(probe.id);
    else if (classification === "bytecode-divergence") {
      summary.bytecodeDivergences.push({
        id: probe.id,
        known: knownDivergence,
        reason: probe.knownBytecodeDivergence?.reason ?? reason,
      });
    } else summary.harnessFailures.push({ id: probe.id, reason });
  }

  const environmentRuns = {} as Record<Mode, ProbeRun>;
  for (const mode of MODES) {
    environmentRuns[mode] = await runProbe(
      goccia,
      join(HARNESS_DIRECTORY, manifest.environmentProbe),
      mode,
      importMap,
      manifest.engineFlags,
      ENVIRONMENT_MARKER,
      timeoutSeconds,
    );
  }
  for (const mode of MODES) {
    const result = environmentRuns[mode];
    if (isNativeOrHarnessFailure(result) || result.marker === null) {
      summary.harnessFailures.push({ id: `environment:${mode}`, reason: result.markerError });
    }
  }

  const environmentMarkers = Object.fromEntries(
    MODES.flatMap((mode) => environmentRuns[mode].marker === null ? [] : [[mode, environmentRuns[mode].marker]]),
  ) as Partial<Record<Mode, JsonObject>>;
  if (Object.keys(environmentMarkers).length === MODES.length) {
    summary.disabledCapabilities.push({
      name: "Function constructor",
      interpreted: environmentMarkers.interpreted!.functionConstructor,
      bytecode: environmentMarkers.bytecode!.functionConstructor,
    });
    for (const name of ["Buffer", "Blob", "process"]) {
      summary.hostGlobals.push({
        name,
        interpreted: environmentMarkers.interpreted!.hostGlobals[name],
        bytecode: environmentMarkers.bytecode!.hostGlobals[name],
      });
    }
  }

  const unexpectedDivergences = summary.bytecodeDivergences.filter((item) => !item.known);
  const functionCapabilities = Object.values(environmentMarkers).map((marker) => marker!.functionConstructor);
  const functionErrorPattern = new RegExp(manifest.expectedEnvironment.functionConstructorErrorPattern);
  const defaultFunctionBoundaryPreserved = functionCapabilities.length === MODES.length
    && functionCapabilities.every(
      (capability) => capability.status === "disabled" && functionErrorPattern.test(capability.error ?? ""),
    );
  const ok = summary.semanticFailures.length === 0
    && unexpectedDivergences.length === 0
    && summary.harnessFailures.length === 0
    && defaultFunctionBoundaryPreserved;

  return {
    schemaVersion: 1,
    upstream: manifest.upstream,
    packageRoot,
    goccia,
    engineFlags: manifest.engineFlags,
    semanticResults,
    environment: Object.fromEntries(MODES.map((mode) => [mode, compactRun(environmentRuns[mode])])),
    summary,
    defaultFunctionBoundaryPreserved,
    ok,
  };
}

function printSummary(report: JsonObject): void {
  const summary = report.summary;
  console.log(`es-toolkit ${report.upstream.version} @ ${report.upstream.commit}`);
  console.log(`Semantic probes passed in both modes: ${summary.semanticPasses.length}`);
  console.log(`Semantic failures: ${summary.semanticFailures.length}`);
  console.log(`Bytecode divergences: ${summary.bytecodeDivergences.length}`);
  for (const divergence of summary.bytecodeDivergences) {
    console.log(`  - ${divergence.id} (${divergence.known ? "known" : "unexpected"}): ${divergence.reason}`);
  }
  console.log(`Disabled capabilities: ${summary.disabledCapabilities.length}`);
  console.log(`Host globals observed: ${summary.hostGlobals.length}`);
  console.log(`Harness failures: ${summary.harnessFailures.length}`);
}

export async function main(argv = process.argv.slice(2)): Promise<number> {
  const options = parseArgs(argv);
  if (options === null) return 0;
  const manifest = loadManifest(options.manifest);
  if (options.printCacheKey) {
    console.log(integrityCacheKey(manifest.upstream.integrity));
    return 0;
  }

  const prepared = await preparePackage(manifest, options.packageDir, options.tarballCache);
  const importMapDirectory = mkdtempSync(join(tmpdir(), "goccia-es-toolkit-importmap-"));
  try {
    const importMap = writeImportMap(manifest, prepared.root, importMapDirectory);
    const report = await buildReport(
      manifest,
      resolve(options.goccia),
      prepared.root,
      importMap,
      options.timeoutSeconds,
    );
    printSummary(report);
    if (options.output) {
      mkdirSync(dirname(options.output), { recursive: true });
      writeFileSync(options.output, `${JSON.stringify(report, null, 2)}\n`);
    }
    return report.ok ? 0 : 1;
  } finally {
    rmSync(importMapDirectory, { recursive: true, force: true });
    if (prepared.temporaryDirectory) {
      rmSync(prepared.temporaryDirectory, { recursive: true, force: true });
    }
  }
}

if (import.meta.main) {
  try {
    process.exitCode = await main();
  } catch (error) {
    console.error(`es-toolkit validation harness failure: ${error instanceof Error ? error.message : String(error)}`);
    process.exitCode = 2;
  }
}
