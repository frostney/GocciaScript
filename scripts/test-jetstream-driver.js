#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');
const { DEFAULT_ENGINES, buildMetricTargetSummary, engineOrderForRepetition, resolveEngines } = require('./cross-engine-report.js');
const {
  DRIVER_VERSION,
  buildJetStreamBundle,
  buildReport,
  executionOrderFromSamples,
  parseArgs: parseDriverArgs,
  writeReport,
} = require('./jetstream-driver.js');
const { buildJetStreamComment } = require('./jetstream-comment.js');
const {
  ciEngines,
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
  const options = parseDriverArgs([
    '--jetstream-dir', 'suite',
    '--benchmark', 'tiny',
    '--iterations', '3',
    '--repetitions', '2',
    '--goccia-baseline-commit', 'base123',
    '--goccia-candidate-commit', 'candidate456',
  ]);
  equal(options.jetStreamDir, 'suite', 'parses suite path');
  equal(options.benchmarks[0], 'tiny', 'parses workload');
  equal(options.iterations, 3, 'parses diagnostic iteration override');
  equal(options.repetitions, 2, 'parses repetitions');
  equal(options.gocciaBaselineCommit, 'base123', 'parses baseline source revision');
  equal(options.gocciaCandidateCommit, 'candidate456', 'parses candidate source revision');
  const engines = resolveEngines({
    ...options,
    engines: ['goccia'],
    gocciaBaseline: 'baseline-loader',
    gocciaCandidate: 'candidate-loader',
  });
  equal(engines[0].sourceCommit, 'base123', 'attaches the baseline binary source revision');
  equal(engines[1].sourceCommit, 'candidate456', 'attaches the candidate binary source revision');
  assert(options.gocciaFlags.includes('--compat-var'), 'enables legacy var compatibility for upstream workloads');
  const ciOptions = parseCiArgs(['--jetstream-dir', 'suite', '--benchmark', 'tiny']);
  equal(ciOptions.benchmarks[0], 'tiny', 'CI report parses one workload shard');
  equal(ciOptions.engines, '', 'CI report takes its engines from the manifest unless told otherwise');
  equal(options.engineOrder, 'alternate', 'engines alternate their order by default');
  equal(parseDriverArgs(['--engine-order', 'fixed']).engineOrder, 'fixed', 'parses the fixed engine order');
  equal(parseDriverArgs(['--jsc=/opt/jsc']).jsc, '/opt/jsc', 'parses the JavaScriptCore shell path');
  throws(() => parseDriverArgs(['--engine-order', 'random']), /--engine-order must be one of/, 'rejects an unknown engine order');
  const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'perf', 'jetstream', 'manifest.json'), 'utf8'));
  equal(manifest.driver.version, DRIVER_VERSION, 'the manifest names the driver version the driver reports');
  equal(ciEngines(manifest), 'goccia,qjs,node,node-jitless,jsc', 'CI adds JSC with its JIT off to the default engines');
  equal(ciEngines({ ciReport: {} }), DEFAULT_ENGINES.join(','), 'a manifest without ciReport.engines runs the driver defaults');
  equal(manifest.ciReport.repetitions, 6, 'CI runs six repetitions');
  const ciEngineNames = ciEngines(manifest).split(',');
  const orders = Array.from({ length: manifest.ciReport.repetitions }, (_, repetition) => (
    engineOrderForRepetition(ciEngineNames, repetition, 'alternate')
  ));
  for (const first of ciEngineNames) {
    for (const second of ciEngineNames) {
      if (first === second) continue;
      const ahead = orders.filter((order) => order.indexOf(first) < order.indexOf(second)).length;
      equal(ahead * 2, orders.length, `${first} runs before ${second} in half the repetitions`);
    }
  }
}

console.log('jetstream-driver: engine order and positions...');
{
  const order = executionOrderFromSamples([
    { engine: 'goccia', repetition: 0, position: 0 },
    { engine: 'qjs', repetition: 0, position: 1 },
    { engine: 'goccia', repetition: 1, position: 1 },
    { engine: 'qjs', repetition: 1, position: 0 },
  ]);
  equal(JSON.stringify(order), JSON.stringify([['goccia', 'qjs'], ['qjs', 'goccia']]), 'reads each repetition\'s order back from the samples');

  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'jetstream-order-test-'));
  try {
    fs.mkdirSync(path.join(temporaryDirectory, 'workloads'));
    fs.writeFileSync(path.join(temporaryDirectory, 'workloads', 'tiny.js'), [
      'class Benchmark {',
      '  runIteration() { let total = 0; for (let index = 0; index < 1000; index += 1) total += index; this.total = total; }',
      '}',
      '',
    ].join('\n'));
    const manifestFile = path.join(temporaryDirectory, 'manifest.json');
    fs.writeFileSync(manifestFile, JSON.stringify({
      jetStream: { commit: 'abc', benchmarks: { tiny: { files: ['workloads/tiny.js'], iterations: 2, worstCaseCount: 1 } } },
    }));
    const output = path.join(temporaryDirectory, 'report.json');
    const run = spawnSync(process.execPath, [
      path.join(__dirname, 'jetstream-driver.js'),
      '--manifest', manifestFile,
      '--jetstream-dir', temporaryDirectory,
      '--benchmark', 'tiny',
      '--engines', 'node,node-jitless',
      '--repetitions', '2',
      '--output', output,
    ], { encoding: 'utf8' });
    equal(run.status, 0, `runs node and node-jitless: ${run.stderr}`);
    const report = JSON.parse(fs.readFileSync(output, 'utf8'));
    const [target] = report.targets;
    equal(report.metadata.driver.version, DRIVER_VERSION, 'the report names the driver version');
    equal(report.metadata.options.engineOrder, 'alternate', 'the report names its engine order');
    equal(JSON.stringify(target.executionOrder), JSON.stringify([['node', 'node-jitless'], ['node-jitless', 'node']]),
      'the second repetition runs the reverse order');
    equal(target.samples.map((sample) => `${sample.engine}@${sample.position}`).join(','),
      'node@0,node-jitless@1,node-jitless@0,node@1', 'each sample records its engine and place');
    assert(target.samples.find((sample) => sample.engine === 'node-jitless').command.includes('--jitless'),
      'the jitless sample\'s command shows the flag');
    assert(typeof report.geomeanRatios['node_over_node-jitless'] === 'number', 'the report has a ratio for each pair');
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
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
    equal(payload.checksum, 'validated', 'records explicit upstream validation');
    equal(payload.verificationKind, 'explicit', 'identifies explicit upstream validation');

    fs.writeFileSync(path.join(temporaryDirectory, 'unchecked.js'), [
      'class Benchmark {',
      '  runIteration() { this.total = (this.total || 0) + 1; }',
      '}',
    ].join('\n'));
    const uncheckedBundle = buildJetStreamBundle({
      jetStreamDir: temporaryDirectory,
      name: 'unchecked',
      benchmark: { files: ['unchecked.js'], iterations: 1, worstCaseCount: 1 },
    });
    const uncheckedFile = path.join(temporaryDirectory, 'unchecked-bundle.js');
    fs.writeFileSync(uncheckedFile, uncheckedBundle);
    const uncheckedResult = spawnSync(process.execPath, [uncheckedFile], { encoding: 'utf8' });
    const uncheckedPayload = JSON.parse(uncheckedResult.stdout.split('GOCCIA_JETSTREAM_RESULT ')[1]);
    equal(
      Number.isFinite(uncheckedPayload.score) && uncheckedPayload.score > 0,
      true,
      'reports a finite score for one diagnostic iteration',
    );
    equal(Object.keys(uncheckedPayload.subScores).join(','), 'First', 'omits unavailable subscores for one iteration');
    equal(uncheckedPayload.checksum, null, 'does not invent a checksum without upstream validation');
    equal(uncheckedPayload.verificationPassed, null, 'does not claim unavailable validation passed');
    equal(uncheckedPayload.verificationKind, 'execution-only', 'labels execution-only coverage');
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
  const manifest = { ciReport: { repetitions: 1, benchmarks: ['tiny'] }, driver: { version: 2 } };
  const report = {
    metadata: { driver: { version: 2 }, options: { repetitions: 1, engineOrder: 'alternate' } },
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
  report.targets[0].summary.engineStats.goccia.timeout = 0;
  report.metadata.options.engineOrder = 'fixed';
  assert(validationFailures(report, manifest, ['goccia']).some((failure) => /alternate engine order/.test(failure)),
    'requires the alternate engine order');
  report.metadata.options.engineOrder = 'alternate';
  report.metadata.driver.version = 1;
  assert(validationFailures(report, manifest, ['goccia']).some((failure) => /driver version 2/.test(failure)),
    'requires the manifest driver version');
  report.metadata.driver.version = 2;
  report.targets[0].summary.engineStats.jsc = { ok: 1, rawCount: 1, medianScore: 20 };
  report.metadata.engines = [{ name: 'goccia', version: 'g' }, { name: 'jsc', version: '' }];
  assert(validationFailures(report, manifest, ['goccia', 'jsc']).some((failure) => /no JavaScriptCore version/.test(failure)),
    'requires a recorded JavaScriptCore version when JSC was measured');
  report.metadata.engines[1].version = 'libjavascriptcoregtk-bin 2.52.6-0ubuntu0.24.04.1';
  equal(validationFailures(report, manifest, ['goccia', 'jsc']).length, 0, 'accepts JSC samples with a recorded version');
}

console.log('jetstream-merge-reports: deterministic shard merge...');
{
  const manifest = { ciReport: { repetitions: 1, benchmarks: ['tiny', 'small'] } };
  const metadata = {
    driver: { version: 1 },
    goccia: { checkoutCommit: 'abc123' },
    corpus: { jetStream: { commit: 'def456' } },
    engines: [
      { name: 'goccia', kind: 'goccia', version: '0.8.1', sourceCommit: 'source123' },
      { name: 'qjs', kind: 'qjs', version: '2026-06-04' },
    ],
    options: { repetitions: 1, engineOrder: 'alternate' },
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
  const incomplete = mergeReports([
    { schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] },
  ], manifest);
  assert(incomplete.validationFailures.some((failure) => /small: missing target/.test(failure)),
    'retains validation failures in an incomplete merged report');
  throws(
    () => mergeReports([], manifest),
    /No JetStream shard reports found; missing workloads: tiny, small/,
    'names every missing workload when no shard report arrived',
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
  throws(
    () => mergeReports([
      { schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] },
      {
        schemaVersion: 1,
        metadata: {
          ...metadata,
          engines: metadata.engines.map((engine) =>
            engine.name === 'goccia' ? { ...engine, sourceCommit: 'different-source' } : engine),
        },
        targets: [target('small', 5, 20)],
      },
    ], manifest),
    /reference-engine versions do not match/,
    'rejects mismatched Goccia binary source revisions',
  );
  throws(
    () => mergeReports([
      { schemaVersion: 1, metadata, targets: [target('tiny', 10, 20)] },
      { schemaVersion: 1, metadata: { ...metadata, options: { ...metadata.options, engineOrder: 'fixed' } }, targets: [target('small', 5, 20)] },
    ], manifest),
    /engine orders do not match/,
    'rejects shards run in different engine orders',
  );

  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'jetstream-merge-test-'));
  try {
    const manifestFile = path.join(temporaryDirectory, 'manifest.json');
    const shardFile = path.join(temporaryDirectory, 'shard.json');
    const outputFile = path.join(temporaryDirectory, 'merged.json');
    fs.writeFileSync(manifestFile, JSON.stringify(manifest));
    fs.writeFileSync(shardFile, JSON.stringify({
      schemaVersion: 1,
      metadata,
      targets: [target('tiny', 10, 20)],
    }));
    const mergeResult = spawnSync(process.execPath, [
      'scripts/jetstream-merge-reports.js',
      '--manifest', manifestFile,
      '--output', outputFile,
      shardFile,
    ], { encoding: 'utf8' });
    equal(mergeResult.status, 1, 'keeps incomplete report validation as a failing CI result');
    assert(fs.existsSync(outputFile), 'writes an incomplete merged report before failing validation');
    const written = JSON.parse(fs.readFileSync(outputFile, 'utf8'));
    assert(written.validationFailures.some((failure) => /small: missing target/.test(failure)),
      'persists validation failures for publication and diagnostics');
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
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
  assert(comment.includes('QuickJS is the bar for the interpreter-speed target'), 'names QuickJS as the target bar');
  assert(comment.includes('2.00×'), 'renders normalized ratio');
  assert(!comment.includes('vs Node.js --jitless'), 'a report without node-jitless has no node-jitless column');

  const withJitless = buildJetStreamComment({
    metadata: { engines: [{ name: 'goccia' }, { name: 'qjs' }, { name: 'node' }, { name: 'node-jitless' }] },
    targets: [{
      name: 'tiny',
      summary: {
        checksumAgreement: { ok: true },
        engineStats: {
          goccia: { ok: 1, medianScore: 10 },
          qjs: { ok: 1, medianScore: 20 },
          node: { ok: 1, medianScore: 40 },
          'node-jitless': { ok: 1, medianScore: 30 },
        },
        ratios: { goccia_over_qjs: 2, goccia_over_node: 4, 'goccia_over_node-jitless': 3 },
      },
    }],
    geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4, 'goccia_over_node-jitless': 3 },
  });
  assert(withJitless.includes('| vs QuickJS | vs Node.js --jitless | vs Node.js |'), 'shows a ratio column per comparator, QuickJS first');
  assert(withJitless.includes('| 2.00× | 3.00× | 4.00× |'), 'renders each comparator ratio separately');
  assert(withJitless.includes('Node.js --jitless 3.00×'), 'shows the node-jitless geomean');
}

console.log(`jetstream-driver: ${passed} assertions passed.`);
