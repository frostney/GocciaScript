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

console.log(`awfy-driver: ${passed} assertions passed.`);
