#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { buildGeomeanRatios } = require('./cross-engine-report.js');
const {
  reportBenchmarks,
  validationFailures,
} = require('./jetstream-ci-report.js');

const DEFAULT_MANIFEST = 'perf/jetstream/manifest.json';
const DEFAULT_OUTPUT = 'jetstream-report.json';

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    output: DEFAULT_OUTPUT,
    reports: [],
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    const nextValue = () => {
      if (arg.includes('=')) return arg.slice(arg.indexOf('=') + 1);
      index += 1;
      if (index >= argv.length) throw new Error(`Missing value for ${arg}`);
      return argv[index];
    };
    if (arg === '--manifest' || arg.startsWith('--manifest=')) options.manifestPath = nextValue();
    else if (arg === '--output' || arg.startsWith('--output=')) options.output = nextValue();
    else if (arg.startsWith('-')) throw new Error(`Unknown option: ${arg}`);
    else options.reports.push(arg);
  }
  return options;
}

function comparableEngineMetadata(report) {
  return (report.metadata?.engines || []).map(({ name, kind, version, sourceCommit }) => ({
    name,
    kind,
    version,
    sourceCommit: sourceCommit || null,
  }));
}

function mergeReports(reports, manifest) {
  if (reports.length === 0) throw new Error('No JetStream shard reports provided');
  const first = reports[0];
  const targets = [];
  const names = new Set();
  const expected = reportBenchmarks(manifest);
  const firstEngines = JSON.stringify(comparableEngineMetadata(first));

  for (const report of reports) {
    if (report.schemaVersion !== first.schemaVersion) {
      throw new Error('JetStream shard schema versions do not match');
    }
    if (report.metadata?.goccia?.checkoutCommit !== first.metadata?.goccia?.checkoutCommit) {
      throw new Error('JetStream shard checkout commits do not match');
    }
    if (report.metadata?.corpus?.jetStream?.commit !== first.metadata?.corpus?.jetStream?.commit) {
      throw new Error('JetStream shard corpus commits do not match');
    }
    if (report.metadata?.driver?.version !== first.metadata?.driver?.version) {
      throw new Error('JetStream shard driver versions do not match');
    }
    if (report.metadata?.options?.repetitions !== first.metadata?.options?.repetitions) {
      throw new Error('JetStream shard repetition counts do not match');
    }
    if (JSON.stringify(comparableEngineMetadata(report)) !== firstEngines) {
      throw new Error('JetStream shard reference-engine versions do not match');
    }
    for (const target of report.targets || []) {
      if (names.has(target.name)) throw new Error(`Duplicate JetStream shard: ${target.name}`);
      names.add(target.name);
      targets.push(target);
    }
  }

  const order = new Map(expected.map((name, index) => [name, index]));
  targets.sort((left, right) =>
    (order.get(left.name) ?? Number.MAX_SAFE_INTEGER) -
    (order.get(right.name) ?? Number.MAX_SAFE_INTEGER));

  const merged = {
    schemaVersion: first.schemaVersion,
    generatedAt: new Date().toISOString(),
    metadata: {
      ...first.metadata,
      shards: { count: reports.length },
    },
    targets,
    geomeanRatios: buildGeomeanRatios(targets),
  };
  const engines = (first.metadata?.engines || []).map((engine) => engine.name);
  const failures = validationFailures(merged, manifest, engines, expected);
  merged.validationFailures = failures;
  return merged;
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.reports.length === 0) throw new Error('Provide at least one shard report path');
  const manifest = JSON.parse(fs.readFileSync(options.manifestPath, 'utf8'));
  const reports = options.reports.map((fileName) => JSON.parse(fs.readFileSync(fileName, 'utf8')));
  const merged = mergeReports(reports, manifest);
  const outputDirectory = path.dirname(options.output);
  if (outputDirectory !== '.') fs.mkdirSync(outputDirectory, { recursive: true });
  fs.writeFileSync(options.output, `${JSON.stringify(merged, null, 2)}\n`);
  if (merged.validationFailures.length > 0) throw new Error(merged.validationFailures.join('\n'));
}

if (require.main === module) {
  try {
    main();
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = { comparableEngineMetadata, mergeReports, parseArgs };
