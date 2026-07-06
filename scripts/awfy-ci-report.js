#!/usr/bin/env node

const fs = require('fs');
const { spawnSync } = require('child_process');

const DEFAULT_MANIFEST = 'perf/awfy/manifest.json';
const DEFAULT_OUTPUT = 'awfy-report.json';
const DEFAULT_ENGINES = 'goccia,qjs,node';
const DEFAULT_TIMEOUT_MS = 300000;

function usage() {
  return [
    'Usage: node scripts/awfy-ci-report.js --awfy-dir <path> [options]',
    '',
    'Options:',
    '  --manifest <path>    AWFY manifest path (default: perf/awfy/manifest.json)',
    '  --awfy-dir <path>    Path to are-we-fast-yet/benchmarks/JavaScript',
    '  --output <path>      Report path (default: awfy-report.json)',
    '  --engines <csv>      Engine order (default: goccia,qjs,node)',
    '  --goccia-baseline <path>',
    '                       Optional baseline Goccia binary, interleaved as goccia-baseline',
    '  --goccia-candidate <path>',
    '                       Optional candidate Goccia binary, interleaved as goccia-candidate',
    '  --timeout-ms <n>     Per-sample timeout (default: 300000)',
  ].join('\n');
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    awfyDir: '',
    output: DEFAULT_OUTPUT,
    engines: DEFAULT_ENGINES,
    gocciaBaseline: '',
    gocciaCandidate: '',
    timeoutMs: DEFAULT_TIMEOUT_MS,
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
    } else if (arg === '--manifest' || arg.startsWith('--manifest=')) {
      options.manifestPath = nextValue();
    } else if (arg === '--awfy-dir' || arg.startsWith('--awfy-dir=')) {
      options.awfyDir = nextValue();
    } else if (arg === '--output' || arg.startsWith('--output=')) {
      options.output = nextValue();
    } else if (arg === '--engines' || arg.startsWith('--engines=')) {
      options.engines = nextValue();
    } else if (arg === '--goccia-baseline' || arg.startsWith('--goccia-baseline=')) {
      options.gocciaBaseline = nextValue();
    } else if (arg === '--goccia-candidate' || arg.startsWith('--goccia-candidate=')) {
      options.gocciaCandidate = nextValue();
    } else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) {
      options.timeoutMs = parsePositiveInteger(nextValue(), '--timeout-ms');
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

function expectedEnginesForRun(options) {
  const requested = options.engines.split(',').map((engine) => engine.trim()).filter(Boolean);
  const expected = [];

  for (const engine of requested) {
    if (engine === 'goccia' && (options.gocciaBaseline || options.gocciaCandidate)) {
      if (options.gocciaBaseline) expected.push('goccia-baseline');
      if (options.gocciaCandidate) expected.push('goccia-candidate');
    } else {
      expected.push(engine);
    }
  }

  return expected;
}

function reportConfig(manifest) {
  return manifest.ciReport || manifest.ciSmoke || {};
}

function targetSpec(item) {
  if (typeof item === 'string') return item;
  if (item && typeof item.name === 'string') {
    return item.innerIterations ? `${item.name}:${item.innerIterations}` : item.name;
  }
  throw new Error(`Invalid AWFY CI target entry: ${JSON.stringify(item)}`);
}

function targetSpecs(config, key) {
  return (config[key] || []).map(targetSpec);
}

function expectedEnginesForTarget(manifest, target, expectedEngines) {
  if (target.kind !== 'probe') return expectedEngines;
  const probe = (manifest.diagnosticProbes || []).find((entry) => entry.name === target.name);
  if (!probe?.engines) return expectedEngines;
  return expectedEngines.filter((engine) => (
    probe.engines.includes(engine) ||
    (engine.startsWith('goccia') && probe.engines.includes('goccia'))
  ));
}

function validateReport(report, manifest, expectedEngines) {
  const config = reportConfig(manifest);
  const expectedRepetitions = config.repetitions || 5;
  const failures = [];

  if (report.metadata?.options?.repetitions !== expectedRepetitions) {
    failures.push(`expected ${expectedRepetitions} repetitions, report recorded ${report.metadata?.options?.repetitions}`);
  }

  for (const target of report.targets || []) {
    if (target.summary?.checksumAgreement?.ok === false) {
      failures.push(`${target.name}: checksum mismatch`);
    }
    for (const engine of expectedEnginesForTarget(manifest, target, expectedEngines)) {
      const stats = target.summary?.engineStats?.[engine];
      if (!stats) {
        failures.push(`${target.name}/${engine}: missing engine stats`);
        continue;
      }
      const nonOk = (stats.timeout || 0) + (stats.crash || 0) + (stats.oom || 0) +
        (stats.verificationFailed || 0) + (stats.missingResult || 0);
      if ((stats.ok || 0) === 0 || (stats.rawCount || 0) !== expectedRepetitions ||
        typeof stats.medianMicros !== 'number' || nonOk > 0) {
        failures.push(`${target.name}/${engine}: ${JSON.stringify(stats)}`);
      }
    }
  }

  if (failures.length > 0) {
    throw new Error(failures.join('\n'));
  }
}

function runReport(options, manifest) {
  if (!options.awfyDir) throw new Error('--awfy-dir is required');

  const config = reportConfig(manifest);
  const repetitions = config.repetitions || 5;
  const args = ['scripts/awfy-driver.js', '--awfy-dir', options.awfyDir];

  for (const benchmark of targetSpecs(config, 'benchmarks')) {
    args.push('--benchmark', benchmark);
  }
  for (const probe of targetSpecs(config, 'probes')) {
    args.push('--probe', probe);
  }
  if (options.gocciaBaseline) args.push('--goccia-baseline', options.gocciaBaseline);
  if (options.gocciaCandidate) args.push('--goccia-candidate', options.gocciaCandidate);
  args.push(
    '--repetitions', String(repetitions),
    '--timeout-ms', String(options.timeoutMs),
    '--engines', options.engines,
    '--output', options.output,
  );

  const result = spawnSync(process.execPath, args, { stdio: 'inherit' });
  if (result.status !== 0) {
    throw new Error(`AWFY driver exited with status ${result.status}`);
  }

  const report = JSON.parse(fs.readFileSync(options.output, 'utf8'));
  const expectedEngines = expectedEnginesForRun(options);
  validateReport(report, manifest, expectedEngines);
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.help) {
    console.log(usage());
    return 0;
  }

  const manifest = JSON.parse(fs.readFileSync(options.manifestPath, 'utf8'));
  runReport(options, manifest);
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
  expectedEnginesForTarget,
  expectedEnginesForRun,
  parseArgs,
  reportConfig,
  targetSpec,
  targetSpecs,
  validateReport,
};
