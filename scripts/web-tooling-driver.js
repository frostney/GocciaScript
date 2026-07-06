#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const DRIVER_VERSION = 1;
const DEFAULT_MANIFEST = path.join('perf', 'web-tooling', 'manifest.json');
const DEFAULT_TIMEOUT_MS = 300000;
const DEFAULT_REPETITIONS = 1;
const DEFAULT_GOCCIA_FLAGS = [
  '--mode=bytecode',
  '--compat-asi',
  '--compat-var',
  '--compat-function',
  '--compat-arguments-object',
  '--compat-traditional-for-loop',
  '--compat-for-in-loop',
  '--compat-while-loops',
  '--compat-loose-equality',
  '--compat-label',
  '--compat-non-strict-mode',
  '--unsafe-function-constructor',
];

function usage() {
  return [
    'Usage: node scripts/web-tooling-driver.js [options]',
    '',
    'Targets:',
    '  --workload <name>            Run a pinned Web Tooling workload; repeatable',
    '  --list                       List pinned workload names',
    '',
    'Inputs:',
    '  --web-tooling-dir <path>     Path to v8/web-tooling-benchmark checkout',
    '  --manifest <path>            Manifest path (default: perf/web-tooling/manifest.json)',
    '  --goccia <path>              GocciaScriptLoader path (default: build/GocciaScriptLoader)',
    '  --goccia-flag <flag>         Extra Goccia flag; repeatable',
    '',
    'Measurement:',
    '  --repetitions <n>            Raw runs per workload (default: 1)',
    '  --timeout-ms <n>             Per-run timeout (default: 300000)',
    '  --output <path>              Write normalized JSON report',
    '  --keep-bundles               Keep generated upstream dist/cli.js copies',
  ].join('\n');
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    webToolingDir: '',
    workloads: [],
    goccia: path.join('build', 'GocciaScriptLoader'),
    gocciaFlags: [],
    repetitions: DEFAULT_REPETITIONS,
    timeoutMs: DEFAULT_TIMEOUT_MS,
    output: '',
    keepBundles: false,
    list: false,
    help: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const nextValue = () => {
      if (arg.includes('=')) return arg.slice(arg.indexOf('=') + 1);
      i += 1;
      if (i >= argv.length) throw new Error(`Missing value for ${arg}`);
      return argv[i];
    };

    if (arg === '--help' || arg === '-h') {
      options.help = true;
    } else if (arg === '--list') {
      options.list = true;
    } else if (arg === '--keep-bundles') {
      options.keepBundles = true;
    } else if (arg === '--manifest' || arg.startsWith('--manifest=')) {
      options.manifestPath = nextValue();
    } else if (arg === '--web-tooling-dir' || arg.startsWith('--web-tooling-dir=')) {
      options.webToolingDir = nextValue();
    } else if (arg === '--workload' || arg.startsWith('--workload=')) {
      options.workloads.push(nextValue());
    } else if (arg === '--goccia' || arg.startsWith('--goccia=')) {
      options.goccia = nextValue();
    } else if (arg === '--goccia-flag' || arg.startsWith('--goccia-flag=')) {
      options.gocciaFlags.push(nextValue());
    } else if (arg === '--repetitions' || arg.startsWith('--repetitions=')) {
      options.repetitions = parsePositiveInteger(nextValue(), '--repetitions');
    } else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) {
      options.timeoutMs = parsePositiveInteger(nextValue(), '--timeout-ms');
    } else if (arg === '--output' || arg.startsWith('--output=')) {
      options.output = nextValue();
    } else {
      throw new Error(`Unknown option: ${arg}`);
    }
  }

  return options;
}

function parsePositiveInteger(value, optionName) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isSafeInteger(parsed) || parsed < 1) {
    throw new Error(`${optionName} must be a positive integer, got ${value}`);
  }
  return parsed;
}

function readJSON(fileName) {
  return JSON.parse(fs.readFileSync(fileName, 'utf8'));
}

function manifestWorkloads(manifest) {
  const workloads = manifest.webTooling?.workloads;
  if (!Array.isArray(workloads) || workloads.length === 0) {
    throw new Error('manifest webTooling.workloads must be a non-empty array');
  }
  return workloads;
}

function resolveTargets(options, manifest) {
  const known = new Set(manifestWorkloads(manifest));
  const requested = options.workloads.length > 0 ? options.workloads : manifestWorkloads(manifest);
  return requested.map((name) => {
    if (!known.has(name)) throw new Error(`Unknown Web Tooling workload: ${name}`);
    return {
      kind: 'web-tooling',
      name,
      source: `${options.webToolingDir}/src/${name}-benchmark.js`,
    };
  });
}

function listTargets(manifest) {
  console.log('Web Tooling workloads:');
  for (const workload of manifestWorkloads(manifest)) console.log(`  ${workload}`);
}

function trimOutput(value) {
  const text = String(value || '').trim();
  if (text.length <= 4000) return text;
  return text.slice(0, 2000) + '\n...[truncated]...\n' + text.slice(-2000);
}

function buildWebToolingBundle({ webToolingDir, workload, tempDir }) {
  const command = ['npm', 'run', 'build', '--', '--env.only', workload];
  const startedAt = new Date().toISOString();
  const started = Date.now();
  const proc = spawnSync(command[0], command.slice(1), {
    cwd: webToolingDir,
    encoding: 'utf8',
    maxBuffer: 32 * 1024 * 1024,
  });
  const durationMs = Date.now() - started;
  const distFile = path.join(webToolingDir, 'dist', 'cli.js');
  const outputFile = path.join(tempDir, `web-tooling-${workload}.js`);
  const ok = proc.status === 0 && fs.existsSync(distFile);

  if (ok) fs.copyFileSync(distFile, outputFile);

  return {
    outcome: ok ? 'ok' : 'build-failed',
    startedAt,
    durationMs,
    command: command.join(' '),
    exitCode: proc.status,
    signal: proc.signal || null,
    stdout: ok ? '' : trimOutput(proc.stdout),
    stderr: ok ? '' : trimOutput(proc.stderr),
    bundleFile: ok ? outputFile : null,
  };
}

function runPreparationScript(webToolingDir, scriptName, outputFile) {
  const fileName = path.join(webToolingDir, outputFile);
  if (fs.existsSync(fileName)) return;

  const command = ['npm', 'run', scriptName];
  const proc = spawnSync(command[0], command.slice(1), {
    cwd: webToolingDir,
    encoding: 'utf8',
    maxBuffer: 16 * 1024 * 1024,
  });
  if (proc.status !== 0 || !fs.existsSync(fileName)) {
    throw new Error([
      `Failed to prepare Web Tooling generated dependency ${outputFile}`,
      `command: ${command.join(' ')}`,
      trimOutput(proc.stdout),
      trimOutput(proc.stderr),
    ].filter(Boolean).join('\n'));
  }
}

function prepareWebToolingBuildDependencies(webToolingDir) {
  runPreparationScript(webToolingDir, 'build:terser-bundled', path.join('build', 'terser-bundled.js'));
  runPreparationScript(webToolingDir, 'build:uglify-js-bundled', path.join('build', 'uglify-js-bundled.js'));
}

function parseMetric(stdout, workload) {
  const lines = stdout.split(/\r?\n/);
  const escaped = workload.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const workloadPattern = new RegExp(`^\\s*${escaped}:\\s*([0-9]+(?:\\.[0-9]+)?)\\s*runs/s\\s*$`, 'i');
  const geomeanPattern = /^\s*Geometric mean:\s*([0-9]+(?:\.[0-9]+)?)\s*runs\/s\s*$/i;
  let runsPerSecond = null;
  let geometricMean = null;

  for (const line of lines) {
    const workloadMatch = workloadPattern.exec(line);
    if (workloadMatch) runsPerSecond = Number(workloadMatch[1]);
    const geomeanMatch = geomeanPattern.exec(line);
    if (geomeanMatch) geometricMean = Number(geomeanMatch[1]);
  }

  if (!Number.isFinite(runsPerSecond)) runsPerSecond = null;
  if (!Number.isFinite(geometricMean)) geometricMean = null;
  return runsPerSecond === null && geometricMean === null
    ? null
    : { runsPerSecond, geometricMean };
}

function classifyProcessOutcome(proc, metric, stdout, stderr) {
  if (proc.error && proc.error.code === 'ETIMEDOUT') return 'timeout';
  const combined = `${stdout}\n${stderr}`.toLowerCase();
  if (/\bout[\s-]of[\s-]memory\b/.test(combined) || /\boom\b/.test(combined) || proc.signal === 'SIGKILL') return 'oom';
  if (proc.status !== 0) {
    if (/\bsyntaxerror:/.test(combined)) return 'syntax-error';
    if (/\b(referenceerror|typeerror|rangeerror|urierror|evalerror|aggregateerror):/.test(combined)) return 'runtime-error';
    if (proc.signal) return 'crash';
    if (/\bfatal error:/.test(combined)) return 'crash';
    return metric ? 'runtime-error' : 'crash';
  }
  if (!metric) return 'missing-result';
  return 'ok';
}

function runGocciaSample({ bundleFile, workload, repetition, options }) {
  const args = [
    bundleFile,
    ...DEFAULT_GOCCIA_FLAGS,
    ...options.gocciaFlags,
  ];
  const startedAt = new Date().toISOString();
  const proc = spawnSync(options.goccia, args, {
    encoding: 'utf8',
    timeout: options.timeoutMs,
    maxBuffer: 32 * 1024 * 1024,
  });
  const stdout = proc.stdout || '';
  const stderr = proc.stderr || '';
  const metric = parseMetric(stdout, workload);
  const outcome = classifyProcessOutcome(proc, metric, stdout, stderr);

  return {
    engine: 'goccia',
    repetition,
    startedAt,
    outcome,
    runsPerSecond: metric?.runsPerSecond ?? null,
    geometricMean: metric?.geometricMean ?? null,
    exitCode: proc.status,
    signal: proc.signal || null,
    command: [options.goccia, ...args],
    stdout: outcome === 'ok' ? '' : trimOutput(stdout),
    stderr: outcome === 'ok' ? '' : trimOutput(stderr),
  };
}

function median(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
}

function iqrFiltered(values) {
  if (values.length < 4) return [...values];
  const sorted = [...values].sort((a, b) => a - b);
  const q1 = sorted[Math.floor(sorted.length / 4)];
  const q3 = sorted[Math.floor((sorted.length * 3) / 4)];
  const iqr = q3 - q1;
  if (iqr <= 0) return sorted;
  const lower = q1 - 1.5 * iqr;
  const upper = q3 + 1.5 * iqr;
  return sorted.filter((value) => value >= lower && value <= upper);
}

function coefficientOfVariation(values) {
  if (values.length === 0) return null;
  const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
  if (mean === 0) return null;
  const variance = values.reduce((sum, value) => sum + ((value - mean) * (value - mean)), 0) / values.length;
  return (Math.sqrt(variance) / mean) * 100;
}

function summarizeSamples(samples) {
  const values = samples
    .filter((sample) => sample.outcome === 'ok' && typeof sample.runsPerSecond === 'number' && sample.runsPerSecond >= 0)
    .map((sample) => sample.runsPerSecond);
  const filtered = iqrFiltered(values);
  return {
    ok: samples.filter((sample) => sample.outcome === 'ok').length,
    timeout: samples.filter((sample) => sample.outcome === 'timeout').length,
    crash: samples.filter((sample) => sample.outcome === 'crash').length,
    syntaxError: samples.filter((sample) => sample.outcome === 'syntax-error').length,
    runtimeError: samples.filter((sample) => sample.outcome === 'runtime-error').length,
    oom: samples.filter((sample) => sample.outcome === 'oom').length,
    missingResult: samples.filter((sample) => sample.outcome === 'missing-result').length,
    rawCount: values.length,
    medianRunsPerSecond: median(values),
    iqrMedianRunsPerSecond: median(filtered),
    minRunsPerSecond: values.length ? Math.min(...values) : null,
    maxRunsPerSecond: values.length ? Math.max(...values) : null,
    coefficientOfVariation: coefficientOfVariation(values),
  };
}

function buildOverallSummary(targetReports, repetitions) {
  const counts = {
    workloadCount: targetReports.length,
    builtCount: 0,
    completedCount: 0,
    buildFailedCount: 0,
    timeoutCount: 0,
    crashCount: 0,
    syntaxErrorCount: 0,
    runtimeErrorCount: 0,
    oomCount: 0,
    missingResultCount: 0,
    repetitions,
  };

  for (const target of targetReports) {
    if (target.build.outcome === 'ok') counts.builtCount += 1;
    if (target.summary.ok > 0) counts.completedCount += 1;
    if (target.build.outcome !== 'ok') counts.buildFailedCount += 1;
    counts.timeoutCount += target.summary.timeout || 0;
    counts.crashCount += target.summary.crash || 0;
    counts.syntaxErrorCount += target.summary.syntaxError || 0;
    counts.runtimeErrorCount += target.summary.runtimeError || 0;
    counts.oomCount += target.summary.oom || 0;
    counts.missingResultCount += target.summary.missingResult || 0;
  }

  return counts;
}

function commandText(command, args) {
  const proc = spawnSync(command, args, { encoding: 'utf8' });
  if (proc.status !== 0) return '';
  return (proc.stdout || '').trim();
}

function collectMetadata(options, manifest) {
  return {
    driver: {
      version: DRIVER_VERSION,
      script: 'scripts/web-tooling-driver.js',
    },
    goccia: {
      commit: commandText('git', ['rev-parse', 'HEAD']),
      shortCommit: commandText('git', ['rev-parse', '--short', 'HEAD']),
      status: commandText('git', ['status', '--short']),
      buildMode: 'external-binary',
      fpcVersion: commandText('fpc', ['-iV']),
      command: options.goccia,
      version: commandText(options.goccia, ['--version']),
    },
    platform: {
      os: os.platform(),
      release: os.release(),
      arch: os.arch(),
      cpus: os.cpus().length,
    },
    corpus: {
      webTooling: manifest.webTooling || null,
    },
    options: {
      repetitions: options.repetitions,
      timeoutMs: options.timeoutMs,
      gocciaFlags: DEFAULT_GOCCIA_FLAGS.concat(options.gocciaFlags),
    },
  };
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.help) {
    console.log(usage());
    return 0;
  }

  const manifest = readJSON(options.manifestPath);
  if (options.list) {
    listTargets(manifest);
    return 0;
  }
  if (!options.webToolingDir) throw new Error('--web-tooling-dir is required');

  const targets = resolveTargets(options, manifest);
  prepareWebToolingBuildDependencies(options.webToolingDir);
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-web-tooling-'));
  const targetReports = [];

  try {
    for (const target of targets) {
      const build = buildWebToolingBundle({
        webToolingDir: options.webToolingDir,
        workload: target.name,
        tempDir,
      });
      const samples = [];

      if (build.outcome === 'ok') {
        for (let repetition = 0; repetition < options.repetitions; repetition += 1) {
          const sample = runGocciaSample({
            bundleFile: build.bundleFile,
            workload: target.name,
            repetition,
            options,
          });
          samples.push(sample);
          const metric = typeof sample.runsPerSecond === 'number' ? ` ${sample.runsPerSecond} runs/s` : '';
          console.error(`${target.name} goccia rep ${repetition + 1}/${options.repetitions}: ${sample.outcome}${metric}`);
        }
      } else {
        console.error(`${target.name} build: ${build.outcome}`);
      }

      targetReports.push({
        kind: target.kind,
        name: target.name,
        source: target.source,
        generatedBundle: options.keepBundles ? build.bundleFile : null,
        build: {
          outcome: build.outcome,
          startedAt: build.startedAt,
          durationMs: build.durationMs,
          command: build.command,
          exitCode: build.exitCode,
          signal: build.signal,
          stdout: build.stdout,
          stderr: build.stderr,
        },
        samples,
        summary: summarizeSamples(samples),
      });
    }
  } finally {
    if (!options.keepBundles) fs.rmSync(tempDir, { recursive: true, force: true });
  }

  const report = {
    schemaVersion: 1,
    generatedAt: new Date().toISOString(),
    metadata: collectMetadata(options, manifest),
    summary: buildOverallSummary(targetReports, options.repetitions),
    targets: targetReports,
  };

  const json = `${JSON.stringify(report, null, 2)}\n`;
  if (options.output) {
    const outputDir = path.dirname(options.output);
    if (outputDir && outputDir !== '.') fs.mkdirSync(outputDir, { recursive: true });
    fs.writeFileSync(options.output, json);
  } else {
    process.stdout.write(json);
  }

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
  DEFAULT_GOCCIA_FLAGS,
  classifyProcessOutcome,
  coefficientOfVariation,
  iqrFiltered,
  manifestWorkloads,
  median,
  parseArgs,
  parseMetric,
  prepareWebToolingBuildDependencies,
  summarizeSamples,
};
