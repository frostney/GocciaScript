#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { buildOverallSummary } = require('./web-tooling-driver.js');
const { targetSpecs, validateReport } = require('./web-tooling-ci-report.js');

const DEFAULT_MANIFEST = 'perf/web-tooling/manifest.json';
const DEFAULT_OUTPUT = 'web-tooling-report.json';

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    output: DEFAULT_OUTPUT,
    reports: [],
  };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const nextValue = () => {
      if (arg.includes('=')) return arg.slice(arg.indexOf('=') + 1);
      i += 1;
      if (i >= argv.length) throw new Error(`Missing value for ${arg}`);
      return argv[i];
    };
    if (arg === '--manifest' || arg.startsWith('--manifest=')) {
      options.manifestPath = nextValue();
    } else if (arg === '--output' || arg.startsWith('--output=')) {
      options.output = nextValue();
    } else if (arg.startsWith('-')) {
      throw new Error(`Unknown option: ${arg}`);
    } else {
      options.reports.push(arg);
    }
  }
  return options;
}

function mergeReports(reports, manifest) {
  if (reports.length === 0) throw new Error('No Web Tooling shard reports provided');
  const expected = targetSpecs(manifest.ciReport || {}, manifest);
  const repetitions = manifest.ciReport?.repetitions || 1;
  const targets = [];
  const names = new Set();
  const first = reports[0];
  const firstCorpusCommit = first.metadata?.corpus?.webTooling?.commit;
  const firstRepetitions = first.metadata?.options?.repetitions;

  for (const report of reports) {
    if (report.schemaVersion !== first.schemaVersion) {
      throw new Error('Web Tooling shard schema versions do not match');
    }
    if (report.metadata?.goccia?.commit !== first.metadata?.goccia?.commit) {
      throw new Error('Web Tooling shard Goccia commits do not match');
    }
    if (report.metadata?.corpus?.webTooling?.commit !== firstCorpusCommit) {
      throw new Error('Web Tooling shard corpus commits do not match');
    }
    if (report.metadata?.options?.repetitions !== firstRepetitions) {
      throw new Error('Web Tooling shard repetition counts do not match');
    }
    for (const target of report.targets || []) {
      if (names.has(target.name)) throw new Error(`Duplicate Web Tooling shard: ${target.name}`);
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
      shards: {
        count: reports.length,
      },
    },
    summary: buildOverallSummary(targets, repetitions),
    targets,
  };
  validateReport(merged, manifest);
  return merged;
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.reports.length === 0) throw new Error('Provide at least one shard report path');
  const manifest = JSON.parse(fs.readFileSync(options.manifestPath, 'utf8'));
  const reports = options.reports.map((fileName) =>
    JSON.parse(fs.readFileSync(fileName, 'utf8')));
  const merged = mergeReports(reports, manifest);
  const outputDir = path.dirname(options.output);
  if (outputDir !== '.') fs.mkdirSync(outputDir, { recursive: true });
  fs.writeFileSync(options.output, `${JSON.stringify(merged, null, 2)}\n`);
}

if (require.main === module) {
  try {
    main();
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = { mergeReports, parseArgs };
