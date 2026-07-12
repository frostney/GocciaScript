#!/usr/bin/env node

const fs = require('fs');
const { spawnSync } = require('child_process');
const { expandGocciaEngines } = require('./cross-engine-report.js');

const DEFAULT_MANIFEST = 'perf/jetstream/manifest.json';
const DEFAULT_OUTPUT = 'jetstream-report.json';
const DEFAULT_ENGINES = 'goccia,qjs,node';
const DEFAULT_TIMEOUT_MS = 300000;

function usage() {
  return [
    'Usage: node scripts/jetstream-ci-report.js --jetstream-dir <path> [options]',
    '',
    '  --manifest <path>            Manifest path',
    '  --jetstream-dir <path>       Pinned WebKit/JetStream checkout',
    '  --output <path>              Report path',
    '  --engines <csv>              Engine order (default: goccia,qjs,node)',
    '  --goccia-baseline <path>     Optional baseline Goccia binary',
    '  --goccia-candidate <path>    Optional candidate Goccia binary',
    '  --timeout-ms <n>             Per-sample timeout',
  ].join('\n');
}

function positiveInteger(value, name) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isSafeInteger(parsed) || parsed < 1) throw new Error(`${name} must be a positive integer, got ${value}`);
  return parsed;
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    jetStreamDir: '',
    output: DEFAULT_OUTPUT,
    engines: DEFAULT_ENGINES,
    gocciaBaseline: '',
    gocciaCandidate: '',
    timeoutMs: DEFAULT_TIMEOUT_MS,
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
    else if (arg === '--manifest' || arg.startsWith('--manifest=')) options.manifestPath = nextValue();
    else if (arg === '--jetstream-dir' || arg.startsWith('--jetstream-dir=')) options.jetStreamDir = nextValue();
    else if (arg === '--output' || arg.startsWith('--output=')) options.output = nextValue();
    else if (arg === '--engines' || arg.startsWith('--engines=')) options.engines = nextValue();
    else if (arg === '--goccia-baseline' || arg.startsWith('--goccia-baseline=')) options.gocciaBaseline = nextValue();
    else if (arg === '--goccia-candidate' || arg.startsWith('--goccia-candidate=')) options.gocciaCandidate = nextValue();
    else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) options.timeoutMs = positiveInteger(nextValue(), '--timeout-ms');
    else throw new Error(`Unknown option: ${arg}`);
  }
  return options;
}

function expectedEngines(options) {
  const requested = options.engines.split(',').map((engine) => engine.trim()).filter(Boolean);
  return expandGocciaEngines(requested, options);
}

function validationFailures(report, manifest, engines) {
  const failures = [];
  const expectedBenchmarks = manifest.ciReport?.benchmarks || [];
  const expectedRepetitions = manifest.ciReport?.repetitions || 5;
  const targets = new Map((report.targets || []).map((target) => [target.name, target]));
  if (report.metadata?.options?.repetitions !== expectedRepetitions) {
    failures.push(`expected ${expectedRepetitions} repetitions, report recorded ${report.metadata?.options?.repetitions}`);
  }
  for (const name of expectedBenchmarks) {
    const target = targets.get(name);
    if (!target) {
      failures.push(`${name}: missing target`);
      continue;
    }
    if (target.summary?.checksumAgreement?.ok === false) failures.push(`${name}: checksum mismatch`);
    for (const engine of engines) {
      const stats = target.summary?.engineStats?.[engine];
      if (!stats) {
        failures.push(`${name}/${engine}: missing engine stats`);
        continue;
      }
      const nonOk = (stats.timeout || 0) + (stats.crash || 0) + (stats.oom || 0) +
        (stats.verificationFailed || 0) + (stats.missingResult || 0);
      if (stats.rawCount !== expectedRepetitions || typeof stats.medianScore !== 'number' || nonOk > 0) {
        failures.push(`${name}/${engine}: ${JSON.stringify(stats)}`);
      }
    }
  }
  return failures;
}

function runReport(options, manifest) {
  if (!options.jetStreamDir) throw new Error('--jetstream-dir is required');
  const args = ['scripts/jetstream-driver.js', '--jetstream-dir', options.jetStreamDir];
  for (const benchmark of manifest.ciReport?.benchmarks || []) args.push('--benchmark', benchmark);
  if (options.gocciaBaseline) args.push('--goccia-baseline', options.gocciaBaseline);
  if (options.gocciaCandidate) args.push('--goccia-candidate', options.gocciaCandidate);
  args.push(
    '--manifest', options.manifestPath,
    '--repetitions', String(manifest.ciReport?.repetitions || 5),
    '--timeout-ms', String(options.timeoutMs),
    '--engines', options.engines,
    '--output', options.output,
  );
  const result = spawnSync(process.execPath, args, { stdio: 'inherit' });
  if (!fs.existsSync(options.output)) throw new Error(`JetStream driver did not produce ${options.output}`);
  const report = JSON.parse(fs.readFileSync(options.output, 'utf8'));
  if (!Array.isArray(report.targets)) throw new Error('JetStream report has no targets array');
  const failures = validationFailures(report, manifest, expectedEngines(options));
  if (result.status !== 0) failures.unshift(`JetStream driver exited with status ${result.status}`);
  return failures;
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.help) {
    console.log(usage());
    return 0;
  }
  const manifest = JSON.parse(fs.readFileSync(options.manifestPath, 'utf8'));
  const failures = runReport(options, manifest);
  if (failures.length > 0) throw new Error(failures.join('\n'));
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

module.exports = { expectedEngines, parseArgs, runReport, validationFailures };
