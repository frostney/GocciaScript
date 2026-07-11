#!/usr/bin/env node

const {
  DEFAULT_GOCCIA_FLAGS,
  classifyProcessOutcome,
  manifestWorkloads,
  parseArgs,
  parseMetric,
  summarizeSamples,
  webToolingEntrySource,
  webToolingVirtualFSSource,
  workloadGocciaFlags,
} = require('./web-tooling-driver.js');
const {
  targetSpecs,
  validateReport,
  workloadTimeoutMs,
} = require('./web-tooling-ci-report.js');
const { mergeReports } = require('./web-tooling-merge-reports.js');
const {
  MARKER,
  buildWebToolingReportComment,
  formatRunsPerSecond,
  targetStatus,
} = require('./web-tooling-comment.js');

let passed = 0;

function assert(condition, message) {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
  passed += 1;
}

function assertEqual(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(`${message}\n  expected: ${JSON.stringify(expected)}\n  actual:   ${JSON.stringify(actual)}`);
  }
  passed += 1;
}

console.log('web-tooling-driver: CLI parsing...');
{
  const options = parseArgs([
    '--web-tooling-dir', 'suite',
    '--workload', 'acorn',
    '--repetitions=2',
    '--timeout-ms', '123',
  ]);
  assertEqual(options.webToolingDir, 'suite', 'parses web-tooling checkout');
  assertEqual(options.workloads.join(','), 'acorn', 'parses workload list');
  assertEqual(options.repetitions, 2, 'parses repetitions');
  assertEqual(options.timeoutMs, 123, 'parses timeout');
  assert(DEFAULT_GOCCIA_FLAGS.includes('--unsafe-function-constructor'), 'Web Tooling enables dynamic Function compatibility');
}

console.log('web-tooling-driver: manifest workloads...');
{
  const manifest = {
    webTooling: {
      workloads: ['acorn', 'babel'],
      maxMemoryBytesByWorkload: { postcss: 25000000 },
    },
  };
  assertEqual(manifestWorkloads(manifest).join(','), 'acorn,babel', 'reads workload list');
  assertEqual(workloadGocciaFlags(manifest, 'acorn').length, 0, 'ordinary workloads keep the default memory ceiling');
  assertEqual(workloadGocciaFlags(manifest, 'postcss').join(','), '--max-memory=25000000', 'workload memory ceilings become Goccia flags');
}

console.log('web-tooling-driver: generated harness...');
{
  const entry = webToolingEntrySource('acorn');
  assert(entry.includes('require("./acorn-benchmark")'), 'entry statically imports one workload');
  assert(entry.includes('target.fn();'), 'entry invokes the upstream workload directly');
  assert(!entry.includes('Benchmark.Suite'), 'entry does not bundle Benchmark.js measurement machinery');

  const vfs = webToolingVirtualFSSource([
    {
      fileName: 'third_party/input.js',
      request: 'raw-loader!../third_party/input.js',
    },
  ]);
  assert(vfs.includes('files["third_party/input.js"]'), 'VFS embeds selected workload payloads');
  const vfsModule = { exports: {} };
  Function('module', 'exports', 'require', vfs)(
    vfsModule,
    vfsModule.exports,
    () => 'payload',
  );
  assertEqual(vfsModule.exports.readFileSync('third_party/input.js'), 'payload', 'VFS exposes synchronous reads');
  vfsModule.exports.readFile('third_party/input.js', (error, value) => {
    assertEqual(error, null, 'VFS callback read succeeds');
    assertEqual(value, 'payload', 'VFS callback read returns the payload');
  });
}

console.log('web-tooling-driver: output parsing...');
{
  const metric = parseMetric([
    'Running Web Tooling Benchmark v0.5.3...',
    '-------------------------------------',
    '         acorn:  8.59 runs/s',
    '-------------------------------------',
    'Geometric mean:  8.59 runs/s',
    '',
  ].join('\n'), 'acorn');
  assertEqual(metric.runsPerSecond, 8.59, 'parses workload runs/s');
  assertEqual(metric.geometricMean, 8.59, 'parses geomean runs/s');
  assertEqual(parseMetric('no benchmark output', 'acorn'), null, 'missing metrics return null');
}

console.log('web-tooling-driver: outcome classification...');
{
  assertEqual(
    classifyProcessOutcome({ status: 0, signal: null }, { runsPerSecond: 1 }, '', ''),
    'ok',
    'metric plus zero exit is ok',
  );
  assertEqual(
    classifyProcessOutcome({ status: 0, signal: null }, null, '', ''),
    'missing-result',
    'zero exit without metric is missing-result',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, '', 'JavaScript heap out of memory'),
    'oom',
    'oom stderr is classified',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, 'SyntaxError: Unterminated regular expression literal', ''),
    'syntax-error',
    'Goccia syntax errors are not process crashes',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, 'ReferenceError: missing is not defined', ''),
    'runtime-error',
    'Goccia runtime errors are not process crashes',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, 'Fatal error: Range check error', ''),
    'crash',
    'native fatal errors remain crashes',
  );
}

console.log('web-tooling-driver: statistics...');
{
  const summary = summarizeSamples([
    { outcome: 'ok', runsPerSecond: 8 },
    { outcome: 'ok', runsPerSecond: 10 },
    { outcome: 'timeout', runsPerSecond: null },
    { outcome: 'syntax-error', runsPerSecond: null },
    { outcome: 'runtime-error', runsPerSecond: null },
  ]);
  assertEqual(summary.ok, 2, 'summary counts ok samples');
  assertEqual(summary.timeout, 1, 'summary counts timeouts');
  assertEqual(summary.syntaxError, 1, 'summary counts syntax errors');
  assertEqual(summary.runtimeError, 1, 'summary counts runtime errors');
  assertEqual(summary.medianRunsPerSecond, 9, 'summary reports median runs/s');
}

console.log('web-tooling-ci-report: validation...');
{
  const manifest = {
    webTooling: { workloads: ['acorn', 'coffeescript'] },
    ciReport: {
      repetitions: 1,
      timeoutMs: 1500000,
      timeoutMsByWorkload: {
        coffeescript: 900000,
        postcss: 3600000,
      },
      workloads: ['acorn', 'coffeescript'],
    },
  };
  assertEqual(targetSpecs(manifest.ciReport, manifest).join(','), 'acorn,coffeescript', 'ci target specs keep all workloads');
  assertEqual(workloadTimeoutMs(manifest.ciReport, 'acorn'), 1500000, 'ci uses the default workload timeout');
  assertEqual(workloadTimeoutMs(manifest.ciReport, 'coffeescript'), 900000, 'ci applies the CoffeeScript timeout override');
  assertEqual(workloadTimeoutMs(manifest.ciReport, 'postcss'), 3600000, 'ci applies the PostCSS timeout override');
  validateReport({
    metadata: { options: { repetitions: 1 } },
    summary: { workloadCount: 2 },
    targets: [
      {
        kind: 'web-tooling',
        name: 'acorn',
        build: { outcome: 'ok' },
        summary: { ok: 1 },
      },
      {
        kind: 'web-tooling',
        name: 'coffeescript',
        build: { outcome: 'ok' },
        summary: { ok: 0, syntaxError: 1 },
      },
    ],
  }, manifest);
  assert(true, 'ci report validation accepts complete workload reports with recorded failures');

  const shardMetadata = {
    goccia: { commit: 'abc123' },
    options: { repetitions: 1 },
  };
  const merged = mergeReports([
    {
      schemaVersion: 1,
      metadata: shardMetadata,
      targets: [{
        kind: 'web-tooling',
        name: 'coffeescript',
        build: { outcome: 'ok' },
        summary: { ok: 1 },
      }],
    },
    {
      schemaVersion: 1,
      metadata: shardMetadata,
      targets: [{
        kind: 'web-tooling',
        name: 'acorn',
        build: { outcome: 'ok' },
        summary: { ok: 1 },
      }],
    },
  ], manifest);
  assertEqual(merged.targets.map((target) => target.name).join(','), 'acorn,coffeescript', 'merge restores manifest order');
  assertEqual(merged.summary.completedCount, 2, 'merge rebuilds the overall summary');
  assertEqual(merged.metadata.shards.count, 2, 'merge records shard count');
}

console.log('web-tooling-comment: markdown...');
{
  assertEqual(formatRunsPerSecond(8.59), '8.59 runs/s', 'formats small run rates');
  assertEqual(targetStatus({ build: { outcome: 'build-failed' }, summary: {} }), 'build-failed', 'status reports build failures');
  const comment = buildWebToolingReportComment({
    metadata: { options: { repetitions: 1 } },
    summary: { workloadCount: 2, completedCount: 1 },
    targets: [
      {
        name: 'acorn',
        build: { outcome: 'ok' },
        summary: { ok: 1, medianRunsPerSecond: 8.59 },
      },
      {
        name: 'babel',
        build: { outcome: 'ok' },
        summary: { ok: 0, syntaxError: 1 },
      },
    ],
  });
  assert(comment.includes(MARKER), 'comment includes stable marker');
  assert(comment.includes('## Web Tooling Benchmark'), 'comment title is present');
  assert(comment.includes('| Workload | Status | Goccia |'), 'comment is Goccia-only');
  assert(comment.includes('| acorn | pass | 8.59 runs/s |'), 'comment renders passing workload');
  assert(comment.includes('| babel | syntax error 1 | - |'), 'comment renders syntax error workload outcome');
  assert(comment.includes('`web-tooling-report` artifact'), 'comment points to artifact');
}

console.log(`web-tooling-driver: ${passed} assertions passed.`);
