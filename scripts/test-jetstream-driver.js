#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');
const { buildMetricTargetSummary } = require('./cross-engine-report.js');
const {
  buildJetStreamBundle,
  buildReport,
  parseArgs: parseDriverArgs,
  writeReport,
} = require('./jetstream-driver.js');
const { buildJetStreamComment } = require('./jetstream-comment.js');
const {
  parseArgs: parseCiArgs,
  reportBenchmarks,
  validationFailures,
} = require('./jetstream-ci-report.js');
const { mergeReports } = require('./jetstream-merge-reports.js');

let passed = 0;
function assert(condition, message) {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
  passed += 1;
}
function equal(actual, expected, message) {
  if (actual !== expected) throw new Error(`${message}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  passed += 1;
}
function throws(callback, pattern, message) {
  try {
    callback();
  } catch (error) {
    if (pattern.test(error.message)) {
      passed += 1;
      return;
    }
    throw new Error(`${message}: unexpected error ${error.message}`);
  }
  throw new Error(`${message}: expected an error`);
}

console.log('jetstream-driver: CLI parsing...');
{
  const options = parseDriverArgs(['--jetstream-dir', 'suite', '--benchmark', 'tiny', '--iterations', '3', '--repetitions', '2']);
  equal(options.jetStreamDir, 'suite', 'parses suite path');
  equal(options.benchmarks[0], 'tiny', 'parses workload');
  equal(options.iterations, 3, 'parses diagnostic iteration override');
  equal(options.repetitions, 2, 'parses repetitions');
  assert(options.gocciaFlags.includes('--compat-var'), 'enables legacy var compatibility for upstream workloads');
  const ciOptions = parseCiArgs(['--jetstream-dir', 'suite', '--benchmark', 'tiny']);
  equal(ciOptions.benchmarks[0], 'tiny', 'CI report parses one workload shard');
}

console.log('jetstream-driver: incremental report checkpoint...');
{
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'jetstream-checkpoint-test-'));
  try {
    const output = path.join(temporaryDirectory, 'report.json');
    const report = buildReport({ options: { repetitions: 1 } }, []);
    writeReport(output, report);
    equal(JSON.parse(fs.readFileSync(output, 'utf8')).schemaVersion, 1, 'writes a readable checkpoint report');
    assert(!fs.existsSync(`${output}.tmp`), 'atomically replaces the checkpoint temporary file');
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
}

console.log('jetstream-driver: upstream bundle and score...');
{
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'jetstream-driver-test-'));
  try {
    fs.writeFileSync(path.join(temporaryDirectory, 'tiny.js'), [
      'class Benchmark {',
      '  runIteration(index) { this.total = (this.total || 0) + index + 1; }',
      '  validate(iterations) { if (this.total !== 6 || iterations !== 3) throw new Error("bad total"); }',
      '}',
    ].join('\n'));
    const bundle = buildJetStreamBundle({
      jetStreamDir: temporaryDirectory,
      name: 'tiny',
      benchmark: { files: ['tiny.js'], iterations: 3, worstCaseCount: 1 },
    });
    assert(bundle.includes('Upstream JetStream 3: tiny.js'), 'identifies unchanged upstream source');
    const bundleFile = path.join(temporaryDirectory, 'bundle.js');
    fs.writeFileSync(bundleFile, bundle);
    const result = spawnSync(process.execPath, [bundleFile], { encoding: 'utf8' });
    equal(result.status, 0, `generated bundle runs: ${result.stderr}`);
    assert(result.stdout.includes('GOCCIA_JETSTREAM_RESULT '), 'emits normalized result marker');
    const payload = JSON.parse(result.stdout.split('GOCCIA_JETSTREAM_RESULT ')[1]);
    assert(payload.score > 0, 'reports positive JetStream score');
    equal(Object.keys(payload.subScores).join(','), 'First,Worst,Average', 'preserves JetStream score components');
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
}

console.log('jetstream-driver: normalized reference ratios...');
{
  const summary = buildMetricTargetSummary([
    { engine: 'goccia', outcome: 'ok', score: 10, checksum: 'verified' },
    { engine: 'qjs', outcome: 'ok', score: 20, checksum: 'verified' },
    { engine: 'node', outcome: 'ok', score: 40, checksum: 'verified' },
  ], { field: 'score', outputPrefix: 'Score', ratioDirection: 'higher-is-better' });
  equal(summary.ratios.goccia_over_qjs, 2, 'inverts higher-is-better score for QuickJS reference ratio');
  equal(summary.ratios.goccia_over_node, 4, 'inverts higher-is-better score for Node reference ratio');
}

console.log('jetstream-ci-report: structural and outcome validation...');
{
  const manifest = { ciReport: { repetitions: 1, benchmarks: ['tiny'] } };
  const report = {
    metadata: { options: { repetitions: 1 } },
    targets: [{
      name: 'tiny',
      summary: {
        checksumAgreement: { ok: true },
        engineStats: { goccia: { ok: 1, rawCount: 1, medianScore: 10 } },
      },
    }],
  };
  equal(validationFailures(report, manifest, ['goccia']).length, 0, 'accepts complete report');
  equal(reportBenchmarks(manifest, ['tiny']).join(','), 'tiny', 'selects one configured workload shard');
  report.targets[0].summary.engineStats.goccia.timeout = 1;
  equal(validationFailures(report, manifest, ['goccia']).length, 1, 'surfaces measured timeout');
}

console.log('jetstream-merge-reports: deterministic shard merge...');
{
  const manifest = { ciReport: { repetitions: 1, benchmarks: ['tiny', 'small'] } };
  const metadata = {
    driver: { version: 1 },
    goccia: { commit: 'abc123' },
    corpus: { jetStream: { commit: 'def456' } },
    engines: [
      { name: 'goccia', kind: 'goccia', version: '0.8.1' },
      { name: 'qjs', kind: 'qjs', version: '2026-06-04' },
    ],
    options: { repetitions: 1 },
  };
  const target = (name, gocciaScore, qjsScore) => ({
    kind: 'jetstream',
    name,
    samples: [],
    summary: buildMetricTargetSummary([
      { engine: 'goccia', outcome: 'ok', score: gocciaScore, checksum: 'verified' },
      { engine: 'qjs', outcome: 'ok', score: qjsScore, checksum: 'verified' },
    ], { field: 'score', outputPrefix: 'Score', ratioDirection: 'higher-is-better' }),
  });
  const merged = mergeReports([
    { schemaVersion: 1, metadata, targets: [target('small', 5, 20)] },
    { schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] },
  ], manifest);
  equal(merged.targets.map((entry) => entry.name).join(','), 'tiny,small', 'restores manifest workload order');
  equal(merged.metadata.shards.count, 2, 'records merged shard count');
  assert(merged.geomeanRatios.goccia_over_qjs > 2, 'recomputes the merged geomean ratio');
  throws(
    () => mergeReports([{ schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] }], manifest),
    /small: missing target/,
    'rejects an incomplete shard set',
  );
  throws(
    () => mergeReports([
      { schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] },
      {
        schemaVersion: 1,
        metadata: { ...metadata, engines: metadata.engines.map((engine) => ({ ...engine, version: `${engine.version}-different` })) },
        targets: [target('small', 5, 20)],
      },
    ], manifest),
    /reference-engine versions do not match/,
    'rejects incompatible shard engine versions',
  );
}

console.log('jetstream-comment: barometer framing...');
{
  const comment = buildJetStreamComment({
    metadata: { engines: [{ name: 'goccia' }, { name: 'qjs' }, { name: 'node' }] },
    targets: [{
      name: 'tiny',
      summary: {
        checksumAgreement: { ok: true },
        engineStats: {
          goccia: { ok: 1, medianScore: 10 },
          qjs: { ok: 1, medianScore: 20 },
          node: { ok: 1, medianScore: 40 },
        },
        ratios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      },
    }],
    geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
  });
  assert(comment.includes('Performance Barometer'), 'uses canonical dashboard vocabulary');
  assert(comment.includes('not a product ranking'), 'avoids competitor framing');
  assert(comment.includes('2.00×'), 'renders normalized ratio');
}

console.log(`jetstream-driver: ${passed} assertions passed.`);
