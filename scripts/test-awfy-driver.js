#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const {
  RESULT_MARKER,
  buildAwfyBundle,
  buildGeomeanRatios,
  checksumAgreement,
  median,
  parseResult,
  summarizeSamples,
} = require('./awfy-driver.js');
const { MARKER, buildAwfyReportComment, formatMicros } = require('./awfy-comment.js');
const { reportConfig, targetSpecs, validateReport } = require('./awfy-ci-report.js');

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

console.log('awfy-driver: statistics...');
{
  assertEqual(median([3, 1, 2]), 2, 'median sorts odd-length values');
  assertEqual(median([4, 1, 2, 3]), 2.5, 'median sorts even-length values');

  const summary = summarizeSamples([
    { outcome: 'ok', durationMicros: 100 },
    { outcome: 'ok', durationMicros: 110 },
    { outcome: 'timeout', durationMicros: null },
  ]);
  assertEqual(summary.ok, 2, 'summary counts ok samples');
  assertEqual(summary.timeout, 1, 'summary counts timeout samples');
  assertEqual(summary.medianMicros, 105, 'summary reports sample median');

  const zeroSummary = summarizeSamples([{ outcome: 'ok', durationMicros: 0 }]);
  assertEqual(zeroSummary.rawCount, 1, 'summary keeps zero-duration samples visible');
  assertEqual(zeroSummary.medianMicros, 0, 'summary reports zero-duration sample median');
}

console.log('awfy-driver: checksum agreement...');
{
  assert(checksumAgreement([
    { outcome: 'ok', checksum: 'abc' },
    { outcome: 'ok', checksum: 'abc' },
    { outcome: 'crash', checksum: null },
  ]).ok, 'matching checksums agree');

  assert(!checksumAgreement([
    { outcome: 'ok', checksum: 'abc' },
    { outcome: 'ok', checksum: 'def' },
  ]).ok, 'different checksums disagree');
}

console.log('awfy-driver: geomean ratios...');
{
  const ratios = buildGeomeanRatios([
    { summary: { ratios: { goccia_over_qjs: 4 } } },
    { summary: { ratios: { goccia_over_qjs: 9 } } },
  ]);
  assertEqual(Math.round(ratios.goccia_over_qjs * 1000) / 1000, 6, 'geomean ratio uses logs');
}

console.log('awfy-driver: portable AWFY bundle...');
{
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'awfy-driver-test-'));
  try {
    fs.writeFileSync(path.join(tmp, 'benchmark.js'), [
      'class Benchmark {',
      '  innerBenchmarkLoop(innerIterations) {',
      '    for (let i = 0; i < innerIterations; i += 1) {',
      '      if (!this.verifyResult(this.benchmark())) return false;',
      '    }',
      '    return true;',
      '  }',
      '}',
      'exports.Benchmark = Benchmark;',
      '',
    ].join('\n'));
    fs.writeFileSync(path.join(tmp, 'tiny.js'), [
      "const { Benchmark } = require('./benchmark');",
      'class Tiny extends Benchmark {',
      '  benchmark() { return 42; }',
      '  verifyResult(result) { return result === 42; }',
      '}',
      'exports.newInstance = () => new Tiny();',
      '',
    ].join('\n'));
    fs.writeFileSync(path.join(tmp, 'cd.js'), 'this is deliberately invalid if bundled\n');

    const bundle = buildAwfyBundle({
      awfyDir: tmp,
      benchmarkName: 'Tiny',
      moduleName: 'tiny',
      innerIterations: 2,
    });
    assert(!bundle.includes('deliberately invalid'), 'bundle only includes selected dependency graph');

    const bundleFile = path.join(tmp, 'bundle.js');
    fs.writeFileSync(bundleFile, bundle);
    const proc = spawnSync(process.execPath, [bundleFile], { encoding: 'utf8' });
    assertEqual(proc.status, 0, `portable bundle exits cleanly: ${proc.stderr}`);
    const parsed = parseResult(proc.stdout);
    assert(parsed !== null, 'portable bundle emits normalized result');
    assertEqual(parsed.name, 'Tiny', 'portable bundle names benchmark');
    assertEqual(parsed.verificationPassed, true, 'portable bundle carries verification');
    assert(proc.stdout.includes(RESULT_MARKER), 'portable bundle uses stable marker');
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
}

console.log('awfy-driver: PR comment markdown...');
{
  assertEqual(formatMicros(0.25), '250ns', 'comment formats sub-microsecond durations as ns');
  assertEqual(formatMicros(250), '250.00µs', 'comment formats microsecond durations with project unit');
  assertEqual(formatMicros(750), '0.75ms', 'comment switches to ms at the project threshold');
  assertEqual(formatMicros(1250), '1.25ms', 'comment formats millisecond durations');

  const comment = buildAwfyReportComment({
    generatedAt: '2026-07-05T00:00:00.000Z',
    metadata: {
      driver: { version: 1 },
      options: { repetitions: 5 },
      corpus: {
        awfy: {
          repository: 'https://github.com/smarr/are-we-fast-yet',
          commit: '1234567890abcdef1234567890abcdef12345678',
        },
      },
    },
    targets: [
      {
        name: 'NBody',
        kind: 'awfy',
        summary: {
          checksumAgreement: { ok: true, checksums: ['verified'] },
          engineStats: {
            goccia: { ok: 5, rawCount: 5, medianMicros: 1000 },
            qjs: { ok: 5, rawCount: 5, medianMicros: 500 },
            node: { ok: 5, rawCount: 5, medianMicros: 250 },
          },
        },
      },
    ],
    geomeanRatios: {
      goccia_over_qjs: 2,
      goccia_over_node: 4,
      qjs_over_goccia: 0.5,
      qjs_over_node: 2,
      node_over_goccia: 0.25,
      node_over_qjs: 0.5,
    },
  });
  assert(comment.includes(MARKER), 'comment includes stable marker');
  assert(comment.includes('## AWFY Results'), 'comment uses results title');
  assert(comment.includes('NBody'), 'comment includes target name');
  assert(comment.includes('| Target | Status | Goccia | QuickJS | Node |'), 'comment uses concise target table');
  assert(!comment.includes('| Kind |'), 'comment does not need a kind column for the PR AWFY lane');
  assert(!comment.includes('Checksum'), 'comment avoids ambiguous checksum wording');
  assert(!comment.includes('| Outcome |'), 'comment avoids separate outcome and verification columns');
  assert(!comment.includes('| Verify |'), 'comment avoids separate outcome and verification columns');
  assert(comment.includes('| NBody | pass | 1.00ms | 0.50ms | 250.00µs |'), 'comment renders status and timings');
  assert(comment.includes('| Ratio (row / column) | Goccia | QuickJS | Node |'), 'comment uses a ratio matrix');
  assert(comment.includes('| Goccia | 1.000 | 2.000 | 4.000 |'), 'comment includes geomean matrix row');
  assert(comment.includes('1 pinned AWFY benchmark'), 'comment summarizes AWFY count');
  assert(comment.includes('Medians from 5 interleaved samples per engine'), 'comment reports interleaved sample count');
  assert(comment.includes('raw JSON includes min/max/CV'), 'comment points to variation stats');
  assert(comment.includes('`awfy-report` artifact'), 'comment points to report artifact');
  assert(comment.includes('Rows below 0.5ms are timer-floor sensitive'), 'comment flags very small timing rows');
  assert(!comment.includes('diagnostic probe'), 'comment omits zero diagnostic probe noise');
  assert(!comment.includes('Pinned corpus:'), 'comment omits pin boilerplate');
  assert(!comment.includes('Generated:'), 'comment omits generated timestamp boilerplate');
}

console.log('awfy-ci-report: manifest helpers...');
{
  const manifest = {
    ciReport: {
      repetitions: 5,
      benchmarks: ['NBody', { name: 'CD', innerIterations: 2 }],
      probes: [],
    },
  };
  assertEqual(reportConfig(manifest).repetitions, 5, 'ci report config is preferred');
  assertEqual(targetSpecs(manifest.ciReport, 'benchmarks').join(','), 'NBody,CD:2', 'ci report target specs keep inner iterations');
}

console.log('awfy-ci-report: validation...');
{
  const manifest = { ciReport: { repetitions: 5 }, diagnosticProbes: [] };
  validateReport({
    metadata: { options: { repetitions: 5 } },
    targets: [{
      kind: 'awfy',
      name: 'NBody',
      summary: {
        checksumAgreement: { ok: true, checksums: ['verified'] },
        engineStats: {
          goccia: { ok: 5, rawCount: 5, medianMicros: 1000 },
          qjs: { ok: 5, rawCount: 5, medianMicros: 500 },
          node: { ok: 5, rawCount: 5, medianMicros: 250 },
        },
      },
    }],
  }, manifest, ['goccia', 'qjs', 'node']);
  assert(true, 'ci report validation accepts five clean samples');
}

console.log(`awfy-driver: ${passed} assertions passed.`);
