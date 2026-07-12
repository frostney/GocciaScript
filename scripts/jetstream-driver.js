#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');
const {
  buildGeomeanRatios,
  buildMetricTargetSummary,
  engineAllowedForTarget,
  resolveEngines,
  runEngineSample,
} = require('./cross-engine-report.js');

const DRIVER_VERSION = 1;
const RESULT_MARKER = 'GOCCIA_JETSTREAM_RESULT ';
const DEFAULT_MANIFEST = path.join('perf', 'jetstream', 'manifest.json');
const DEFAULT_TIMEOUT_MS = 300000;
const DEFAULT_REPETITIONS = 3;
const JETSTREAM_GOCCIA_FLAGS = [
  '--compat-var',
  '--compat-loose-equality',
  '--compat-non-strict-mode',
  '--compat-asi',
  '--compat-label',
  '--compat-arguments-object',
];

function usage() {
  return [
    'Usage: node scripts/jetstream-driver.js [options]',
    '',
    'Targets:',
    '  --benchmark <name>           Run a pinned JetStream 3 workload; repeatable',
    '  --list                       List pinned JetStream workloads',
    '',
    'Inputs and engines:',
    '  --jetstream-dir <path>       Path to the pinned WebKit/JetStream checkout',
    '  --manifest <path>            Manifest path (default: perf/jetstream/manifest.json)',
    '  --engines <csv>              Engine order (default: goccia,qjs,node)',
    '  --goccia <path>              GocciaScriptLoader path (default: build/GocciaScriptLoader)',
    '  --goccia-baseline <path>     Optional baseline Goccia binary',
    '  --goccia-candidate <path>    Optional candidate Goccia binary',
    '  --qjs <path>                 QuickJS shell path (default: qjs)',
    '  --node <path>                Node.js path (default: current node)',
    '  --goccia-flag <flag>         Extra Goccia flag; repeatable',
    '',
    'Measurement:',
    '  --repetitions <n>            Process samples per engine/workload (default: 3)',
    '  --iterations <n>             Override upstream iteration count for diagnostics',
    '  --timeout-ms <n>             Per-sample timeout (default: 300000)',
    '  --output <path>              Write normalized JSON report',
    '  --keep-temp                  Keep generated shell bundles',
  ].join('\n');
}

function parsePositiveInteger(value, optionName) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isSafeInteger(parsed) || parsed < 1) {
    throw new Error(`${optionName} must be a positive integer, got ${value}`);
  }
  return parsed;
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    jetStreamDir: '',
    benchmarks: [],
    engines: null,
    goccia: path.join('build', 'GocciaScriptLoader'),
    gocciaBaseline: '',
    gocciaCandidate: '',
    qjs: 'qjs',
    node: process.execPath,
    gocciaFlags: [...JETSTREAM_GOCCIA_FLAGS],
    repetitions: DEFAULT_REPETITIONS,
    iterations: null,
    timeoutMs: DEFAULT_TIMEOUT_MS,
    output: '',
    profile: '',
    profileDir: '',
    keepTemp: false,
    list: false,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    const nextValue = () => {
      if (arg.includes('=')) return arg.slice(arg.indexOf('=') + 1);
      index += 1;
      if (index >= argv.length) throw new Error(`Missing value for ${arg}`);
      return argv[index];
    };
    if (arg === '--help' || arg === '-h') options.help = true;
    else if (arg === '--list') options.list = true;
    else if (arg === '--keep-temp') options.keepTemp = true;
    else if (arg === '--manifest' || arg.startsWith('--manifest=')) options.manifestPath = nextValue();
    else if (arg === '--jetstream-dir' || arg.startsWith('--jetstream-dir=')) options.jetStreamDir = nextValue();
    else if (arg === '--benchmark' || arg.startsWith('--benchmark=')) options.benchmarks.push(nextValue());
    else if (arg === '--engines' || arg.startsWith('--engines=')) options.engines = nextValue().split(',').map((value) => value.trim()).filter(Boolean);
    else if (arg === '--goccia' || arg.startsWith('--goccia=')) options.goccia = nextValue();
    else if (arg === '--goccia-baseline' || arg.startsWith('--goccia-baseline=')) options.gocciaBaseline = nextValue();
    else if (arg === '--goccia-candidate' || arg.startsWith('--goccia-candidate=')) options.gocciaCandidate = nextValue();
    else if (arg === '--qjs' || arg.startsWith('--qjs=')) options.qjs = nextValue();
    else if (arg === '--node' || arg.startsWith('--node=')) options.node = nextValue();
    else if (arg === '--goccia-flag' || arg.startsWith('--goccia-flag=')) options.gocciaFlags.push(nextValue());
    else if (arg === '--repetitions' || arg.startsWith('--repetitions=')) options.repetitions = parsePositiveInteger(nextValue(), '--repetitions');
    else if (arg === '--iterations' || arg.startsWith('--iterations=')) options.iterations = parsePositiveInteger(nextValue(), '--iterations');
    else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) options.timeoutMs = parsePositiveInteger(nextValue(), '--timeout-ms');
    else if (arg === '--output' || arg.startsWith('--output=')) options.output = nextValue();
    else throw new Error(`Unknown option: ${arg}`);
  }
  return options;
}

function readManifest(fileName) {
  return JSON.parse(fs.readFileSync(fileName, 'utf8'));
}

function deterministicRandomSource() {
  return `
(() => {
  const initialSeed = 49734321;
  let seed = initialSeed;
  Math.random = () => {
    seed = ((seed + 0x7ed55d16) + (seed << 12)) & 0xffffffff;
    seed = ((seed ^ 0xc761c23c) ^ (seed >>> 19)) & 0xffffffff;
    seed = ((seed + 0x165667b1) + (seed << 5)) & 0xffffffff;
    seed = ((seed + 0xd3a2646c) ^ (seed << 9)) & 0xffffffff;
    seed = ((seed + 0xfd7046c5) + (seed << 3)) & 0xffffffff;
    seed = ((seed ^ 0xb55a4f09) ^ (seed >>> 16)) & 0xffffffff;
    return (seed >>> 0) / 0x100000000;
  };
  Math.random.__resetSeed = () => { seed = initialSeed; };
})();`;
}

function buildJetStreamBundle({ jetStreamDir, name, benchmark }) {
  const config = {
    name,
    iterations: benchmark.iterations,
    worstCaseCount: benchmark.worstCaseCount,
    deterministicRandom: Boolean(benchmark.deterministicRandom),
  };
  const chunks = [
    `const __jetStreamConfig = ${JSON.stringify(config)};`,
    'const __jetStreamNow = () => (typeof performance !== "undefined" && performance.now ? performance.now() : Date.now());',
  ];
  if (config.deterministicRandom) chunks.push(deterministicRandomSource());
  for (const relativeFile of benchmark.files) {
    const fileName = path.join(jetStreamDir, relativeFile);
    chunks.push(`// Upstream JetStream 3: ${relativeFile}\n${fs.readFileSync(fileName, 'utf8')}`);
  }
  chunks.push(`
const __benchmark = new Benchmark();
const __times = [];
for (let index = 0; index < __jetStreamConfig.iterations; index += 1) {
  if (__jetStreamConfig.deterministicRandom) Math.random.__resetSeed();
  if (__benchmark.prepareForNextIteration) __benchmark.prepareForNextIteration();
  const started = __jetStreamNow();
  __benchmark.runIteration(index);
  __times.push(Math.max(1, __jetStreamNow() - started));
}
if (__benchmark.validate) __benchmark.validate(__jetStreamConfig.iterations);
const __firstTime = __times[0];
const __remaining = __times.slice(1).sort((left, right) => right - left);
const __mean = (values) => values.reduce((sum, value) => sum + value, 0) / values.length;
const __toScore = (time) => 5000 / Math.max(time, 1);
const __subScores = { First: __toScore(__firstTime) };
if (__jetStreamConfig.worstCaseCount > 0) {
  __subScores.Worst = __toScore(__mean(__remaining.slice(0, __jetStreamConfig.worstCaseCount)));
}
if (__jetStreamConfig.iterations > 1) __subScores.Average = __toScore(__mean(__remaining));
const __scoreValues = Object.values(__subScores);
const __score = __scoreValues.reduce((product, value) => product * value, 1) ** (1 / __scoreValues.length);
const __result = {
  kind: "jetstream",
  name: __jetStreamConfig.name,
  score: __score,
  subScores: __subScores,
  checksum: "verified",
  verificationPassed: true,
  iterations: __jetStreamConfig.iterations,
};
console.log(${JSON.stringify(RESULT_MARKER)} + JSON.stringify(__result));
`);
  return `${chunks.join('\n\n')}\n`;
}

function resolveTargets(options, manifest) {
  const benchmarkMap = manifest.jetStream?.benchmarks || {};
  return options.benchmarks.map((name) => {
    const benchmark = benchmarkMap[name];
    if (!benchmark) throw new Error(`Unknown JetStream benchmark: ${name}`);
    if (!options.jetStreamDir) throw new Error('--jetstream-dir is required when --benchmark is used');
    return {
      kind: 'jetstream',
      name,
      benchmark: options.iterations ? { ...benchmark, iterations: options.iterations } : benchmark,
      focusAreas: benchmark.focusAreas || [],
      tags: ['jetstream', ...(benchmark.focusAreas || [])],
      source: benchmark.files.map((file) => `${options.jetStreamDir}/${file}`),
    };
  });
}

function normalizeJetStreamResult(parsed, target) {
  return {
    score: parsed?.score ?? null,
    subScores: parsed?.subScores ?? null,
    checksum: parsed?.checksum ?? null,
    verificationPassed: parsed?.verificationPassed ?? false,
    iterations: target.benchmark.iterations,
  };
}

function commandText(command, args) {
  const proc = spawnSync(command, args, { encoding: 'utf8' });
  return proc.status === 0 ? (proc.stdout || proc.stderr || '').trim() : '';
}

function engineVersion(engine) {
  if (engine.kind === 'node') return commandText(engine.command, ['--version']);
  if (engine.kind === 'qjs') {
    const proc = spawnSync(engine.command, ['-h'], { encoding: 'utf8' });
    return `${proc.stdout || ''}${proc.stderr || ''}`.split(/\r?\n/)[0].trim();
  }
  return commandText(engine.command, ['--version']);
}

function collectMetadata(options, manifest, engines) {
  return {
    driver: { version: DRIVER_VERSION, script: 'scripts/jetstream-driver.js' },
    goccia: {
      commit: commandText('git', ['rev-parse', 'HEAD']),
      shortCommit: commandText('git', ['rev-parse', '--short', 'HEAD']),
      status: commandText('git', ['status', '--short']),
      buildMode: 'external-binary',
      fpcVersion: commandText('fpc', ['-iV']),
    },
    platform: { os: os.platform(), release: os.release(), arch: os.arch(), cpus: os.cpus().length },
    corpus: { jetStream: manifest.jetStream || null },
    engines: engines.map((engine) => ({
      name: engine.name,
      kind: engine.kind,
      command: engine.command,
      version: engineVersion(engine),
    })),
    options: { repetitions: options.repetitions, timeoutMs: options.timeoutMs },
  };
}

function buildTargetReport(target, samples, generatedBundle = null) {
  return {
    kind: target.kind,
    name: target.name,
    source: target.source,
    generatedBundle,
    focusAreas: target.focusAreas,
    tags: target.tags,
    iterations: target.benchmark.iterations,
    samples,
    summary: buildMetricTargetSummary(samples, {
      field: 'score',
      outputPrefix: 'Score',
      ratioDirection: 'higher-is-better',
    }),
  };
}

function buildReport(metadata, targetReports) {
  return {
    schemaVersion: 1,
    generatedAt: new Date().toISOString(),
    metadata,
    targets: targetReports,
    geomeanRatios: buildGeomeanRatios(targetReports),
  };
}

function writeReport(fileName, report) {
  const outputDirectory = path.dirname(fileName);
  if (outputDirectory && outputDirectory !== '.') fs.mkdirSync(outputDirectory, { recursive: true });
  const temporaryFile = `${fileName}.tmp`;
  fs.writeFileSync(temporaryFile, `${JSON.stringify(report, null, 2)}\n`);
  fs.renameSync(temporaryFile, fileName);
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.help) {
    console.log(usage());
    return 0;
  }
  const manifest = readManifest(options.manifestPath);
  if (options.list) {
    for (const name of Object.keys(manifest.jetStream?.benchmarks || {}).sort()) console.log(name);
    return 0;
  }
  const targets = resolveTargets(options, manifest);
  if (targets.length === 0) throw new Error('No targets selected. Use --benchmark or --list.');
  const engines = resolveEngines(options);
  const metadata = collectMetadata(options, manifest, engines);
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-jetstream-'));
  const targetReports = [];
  try {
    for (const target of targets) {
      const bundle = buildJetStreamBundle({
        jetStreamDir: options.jetStreamDir,
        name: target.name,
        benchmark: target.benchmark,
      });
      const bundleFile = path.join(temporaryDirectory, `jetstream-${target.name}.js`);
      fs.writeFileSync(bundleFile, bundle);
      const samples = [];
      const generatedBundle = options.keepTemp ? bundleFile : null;
      if (options.output) {
        writeReport(options.output, buildReport(metadata, [
          ...targetReports,
          buildTargetReport(target, samples, generatedBundle),
        ]));
      }
      for (let repetition = 0; repetition < options.repetitions; repetition += 1) {
        for (const engine of engines) {
          if (!engineAllowedForTarget(engine, target)) continue;
          const sample = runEngineSample({
            engine,
            target,
            fileName: bundleFile,
            repetition,
            options,
            marker: RESULT_MARKER,
            normalizeResult: normalizeJetStreamResult,
          });
          samples.push(sample);
          console.error(`${target.name} ${engine.name} rep ${repetition + 1}/${options.repetitions}: ${sample.outcome}${sample.score ? ` ${sample.score.toFixed(2)} pts` : ''}`);
          if (options.output) {
            writeReport(options.output, buildReport(metadata, [
              ...targetReports,
              buildTargetReport(target, samples, generatedBundle),
            ]));
          }
        }
      }
      targetReports.push(buildTargetReport(target, samples, generatedBundle));
    }
  } finally {
    if (!options.keepTemp) fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
  const report = buildReport(metadata, targetReports);
  if (options.output) writeReport(options.output, report);
  else process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
  return 0;
}

if (require.main === module) {
  try {
    process.exitCode = main();
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = {
  RESULT_MARKER,
  buildReport,
  buildJetStreamBundle,
  buildTargetReport,
  normalizeJetStreamResult,
  parseArgs,
  resolveTargets,
  writeReport,
};
