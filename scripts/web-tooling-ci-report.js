#!/usr/bin/env node

const fs = require('fs');
const { spawnSync } = require('child_process');

const DEFAULT_MANIFEST = 'perf/web-tooling/manifest.json';
const DEFAULT_OUTPUT = 'web-tooling-report.json';
const DEFAULT_TIMEOUT_MS = 1500000;

function usage() {
  return [
    'Usage: node scripts/web-tooling-ci-report.js --web-tooling-dir <path> [options]',
    '',
    'Options:',
    '  --manifest <path>          Web Tooling manifest path (default: perf/web-tooling/manifest.json)',
    '  --web-tooling-dir <path>   Path to v8/web-tooling-benchmark checkout',
    '  --workload <name>          Run one manifest workload; repeatable',
    '  --output <path>            Report path (default: web-tooling-report.json)',
    '  --timeout-ms <n>           Override the configured Goccia timeout',
  ].join('\n');
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    webToolingDir: '',
    workloads: [],
    output: DEFAULT_OUTPUT,
    timeoutMs: DEFAULT_TIMEOUT_MS,
    timeoutExplicit: false,
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
    } else if (arg === '--manifest' || arg.startsWith('--manifest=')) {
      options.manifestPath = nextValue();
    } else if (arg === '--web-tooling-dir' || arg.startsWith('--web-tooling-dir=')) {
      options.webToolingDir = nextValue();
    } else if (arg === '--workload' || arg.startsWith('--workload=')) {
      options.workloads.push(nextValue());
    } else if (arg === '--output' || arg.startsWith('--output=')) {
      options.output = nextValue();
    } else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) {
      options.timeoutMs = parsePositiveInteger(nextValue(), '--timeout-ms');
      options.timeoutExplicit = true;
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

function reportConfig(manifest) {
  return manifest.ciReport || {};
}

function workloadTimeoutMs(config, workload, fallback = DEFAULT_TIMEOUT_MS) {
  const configuredDefault = config.timeoutMs ?? fallback;
  const configuredOverride = config.timeoutMsByWorkload?.[workload];
  return parsePositiveInteger(
    String(configuredOverride ?? configuredDefault),
    `timeout for ${workload}`,
  );
}

function targetSpecs(config, manifest, requested = []) {
  const configured = config.workloads || manifest.webTooling?.workloads || [];
  const workloads = requested.length > 0 ? requested : configured;
  if (!Array.isArray(workloads) || workloads.length === 0) {
    throw new Error('Web Tooling CI report has no workloads');
  }
  const known = new Set(configured);
  for (const workload of workloads) {
    if (!known.has(workload)) throw new Error(`Unknown Web Tooling CI workload: ${workload}`);
  }
  return workloads;
}

function validateReport(report, manifest, expectedWorkloads = null) {
  const config = reportConfig(manifest);
  const workloads = expectedWorkloads || targetSpecs(config, manifest);
  const expectedRepetitions = config.repetitions || 1;
  const failures = [];

  if (report.metadata?.options?.repetitions !== expectedRepetitions) {
    failures.push(`expected ${expectedRepetitions} repetitions, report recorded ${report.metadata?.options?.repetitions}`);
  }

  const targets = Array.isArray(report.targets) ? report.targets : [];
  const byName = new Map(targets.map((target) => [target.name, target]));
  for (const workload of workloads) {
    const target = byName.get(workload);
    if (!target) {
      failures.push(`${workload}: missing target report`);
      continue;
    }
    if (target.kind !== 'web-tooling') failures.push(`${workload}: wrong kind ${target.kind}`);
    if (!target.build || typeof target.build.outcome !== 'string') {
      failures.push(`${workload}: missing build outcome`);
    }
    if (!target.summary || typeof target.summary.ok !== 'number') {
      failures.push(`${workload}: missing sample summary`);
    }
  }

  if (report.summary?.workloadCount !== workloads.length) {
    failures.push(`expected ${workloads.length} workloads, report recorded ${report.summary?.workloadCount}`);
  }

  if (failures.length > 0) throw new Error(failures.join('\n'));
}

function runReport(options, manifest) {
  if (!options.webToolingDir) throw new Error('--web-tooling-dir is required');

  const config = reportConfig(manifest);
  const repetitions = config.repetitions || 1;
  const workloads = targetSpecs(config, manifest, options.workloads);
  const timeoutMs = options.timeoutExplicit
    ? options.timeoutMs
    : Math.max(...workloads.map((workload) => workloadTimeoutMs(config, workload)));
  const args = ['scripts/web-tooling-driver.js', '--web-tooling-dir', options.webToolingDir];

  for (const workload of workloads) {
    args.push('--workload', workload);
  }
  args.push(
    '--repetitions', String(repetitions),
    '--timeout-ms', String(timeoutMs),
    '--output', options.output,
  );

  const result = spawnSync(process.execPath, args, { stdio: 'inherit' });
  if (result.status !== 0) {
    throw new Error(`Web Tooling driver exited with status ${result.status}`);
  }

  const report = JSON.parse(fs.readFileSync(options.output, 'utf8'));
  validateReport(report, manifest, workloads);
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
  parseArgs,
  reportConfig,
  targetSpecs,
  validateReport,
  workloadTimeoutMs,
};
