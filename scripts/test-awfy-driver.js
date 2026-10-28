#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const {
  RESULT_MARKER,
  buildAwfyBundle,
  buildGeomeanRatios,
  classifyProcessOutcome,
  checksumAgreement,
  median,
  parseArgs,
  parseResult,
  refreshBinaryCopies,
  releaseBinaryCopies,
  summarizeSamples,
} = require('./awfy-driver.js');
const { MARKER, buildAwfyReportComment, counterDeltaCell, formatMicros } = require('./awfy-comment.js');
const {
  buildMetricTargetSummary,
  counterCommand,
  parsePerfStat,
  probeCounters,
  uncountedProcess,
} = require('./cross-engine-report.js');
const {
  expectedEnginesForRun,
  parseArgs: parseCiReportArgs,
  reportConfig,
  targetSpecs,
  validateReport,
} = require('./awfy-ci-report.js');

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
    assert(!bundle.includes('const require ='), 'portable bundle avoids a top-level require shim');
    assert(bundle.includes('const __awfyRequire ='), 'portable bundle uses a private AWFY loader');

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

console.log('awfy-driver: outcome classification...');
{
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, '', 'benchmark boom'),
    'crash',
    'ordinary words containing oom are not memory failures',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, '', 'JavaScript heap out of memory'),
    'oom',
    'out of memory stderr is classified as oom',
  );
  assertEqual(
    classifyProcessOutcome({ status: 1, signal: null }, null, 'OOM killed', ''),
    'oom',
    'standalone OOM marker is classified as oom',
  );
  assertEqual(
    classifyProcessOutcome({ status: null, signal: 'SIGKILL' }, null, '', ''),
    'oom',
    'SIGKILL remains classified as oom',
  );
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
      engines: [
        { name: 'goccia', version: '' },
        { name: 'qjs', version: 'QuickJS version 2026-06-04' },
        { name: 'node', version: 'v26.4.0' },
      ],
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
  assert(comment.includes('| Target | Status | Goccia | QuickJS 2026-06-04 | NodeJS v26.4.0 |'), 'comment table header includes reference engine versions');
  assert(!comment.includes('| Kind |'), 'comment does not need a kind column for the PR AWFY lane');
  assert(!comment.includes('Checksum'), 'comment avoids ambiguous checksum wording');
  assert(!comment.includes('| Outcome |'), 'comment avoids separate outcome and verification columns');
  assert(!comment.includes('| Verify |'), 'comment avoids separate outcome and verification columns');
  assert(comment.includes('| NBody | pass | 1.00ms | 0.50ms | 250.00µs |'), 'comment renders status and timings');
  assert(comment.includes('| Ratio (row / column) | Goccia | QuickJS 2026-06-04 | NodeJS v26.4.0 |'), 'geomean matrix header includes reference engine versions');
  assert(comment.includes('| Goccia | 1.000 | 2.000 | 4.000 |'), 'comment includes geomean matrix row');
  assert(comment.includes('1 pinned AWFY benchmark'), 'comment summarizes AWFY count');
  assert(comment.includes('Medians from 5 interleaved samples per engine'), 'comment reports interleaved sample count');
  assert(comment.includes('raw JSON includes min/max/CV'), 'comment points to variation stats');
  assert(comment.includes('`awfy-report` artifact'), 'comment points to report artifact');
  assert(comment.includes('Rows below 0.5ms are timer-floor sensitive'), 'comment flags very small timing rows');
  assert(!comment.includes('diagnostic probe'), 'comment omits zero diagnostic probe noise');
  assert(!comment.includes('Pinned corpus:'), 'comment omits pin boilerplate');
  assert(!comment.includes('Generated:'), 'comment omits generated timestamp boilerplate');

  const missingEngineComment = buildAwfyReportComment({
    metadata: {
      options: { repetitions: 5 },
      engines: [
        { name: 'goccia', version: '' },
        { name: 'qjs', version: 'QuickJS version 2026-06-04' },
        { name: 'node', version: 'v26.4.0' },
      ],
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
          },
        },
      },
    ],
    geomeanRatios: {},
  });
  assert(missingEngineComment.includes('| NBody | engine issue: node | 1.00ms | 0.50ms | - |'), 'comment flags missing AWFY engine stats');

  const comparisonComment = buildAwfyReportComment({
    metadata: {
      options: { repetitions: 5 },
      engines: [
        { name: 'goccia-baseline', version: '' },
        { name: 'goccia-candidate', version: '' },
        { name: 'qjs', version: 'QuickJS version 2026-06-04' },
        { name: 'node', version: 'v26.4.0' },
      ],
    },
    targets: [
      {
        name: 'NBody',
        kind: 'awfy',
        summary: {
          checksumAgreement: { ok: true, checksums: ['verified'] },
          engineStats: {
            'goccia-baseline': { ok: 5, rawCount: 5, medianMicros: 1000, minMicros: 990, maxMicros: 1010 },
            'goccia-candidate': { ok: 5, rawCount: 5, medianMicros: 900, minMicros: 890, maxMicros: 910 },
            qjs: { ok: 5, rawCount: 5, medianMicros: 500 },
            node: { ok: 5, rawCount: 5, medianMicros: 250 },
          },
          ratios: {
            'goccia-candidate_over_goccia-baseline': 0.9,
          },
        },
      },
      {
        name: 'Noisy',
        kind: 'awfy',
        summary: {
          checksumAgreement: { ok: true, checksums: ['verified'] },
          engineStats: {
            'goccia-baseline': { ok: 5, rawCount: 5, medianMicros: 1000, minMicros: 900, maxMicros: 1100 },
            'goccia-candidate': { ok: 5, rawCount: 5, medianMicros: 900, minMicros: 850, maxMicros: 1050 },
            qjs: { ok: 5, rawCount: 5, medianMicros: 500 },
            node: { ok: 5, rawCount: 5, medianMicros: 250 },
          },
          ratios: {
            'goccia-candidate_over_goccia-baseline': 0.9,
          },
        },
      },
    ],
    geomeanRatios: {
      'goccia-baseline_over_goccia-candidate': 1.111111111,
      'goccia-candidate_over_goccia-baseline': 0.9,
    },
  });
  assert(comparisonComment.includes('| Target | Status | Goccia main | Goccia PR | QuickJS 2026-06-04 | NodeJS v26.4.0 | Δ PR vs main |'), 'comparison comment shows main and PR Goccia columns');
  assert(comparisonComment.includes('| NBody | pass | 1.00ms | 0.90ms | 0.50ms | 250.00µs | 🟢 +10.00% |'), 'comparison comment renders non-overlapping improvement delta');
  assert(comparisonComment.includes('| Noisy | pass | 1.00ms | 0.90ms | 0.50ms | 250.00µs | ~ overlap (+10.00%) |'), 'comparison comment marks overlapping ranges as unchanged noise');
  assert(comparisonComment.includes('| Goccia main | 1.000 | 1.111 | - | - |'), 'comparison geomean rows use main label');
  assert(comparisonComment.includes('| Goccia PR | 0.900 | 1.000 | - | - |'), 'comparison geomean rows use PR label');
  assert(comparisonComment.includes('| QuickJS 2026-06-04 | - | - | 1.000 | - |'), 'comparison geomean rows use versioned QuickJS label');
  assert(comparisonComment.includes('| NodeJS v26.4.0 | - | - | - | 1.000 |'), 'comparison geomean rows use versioned Node label');
  assert(comparisonComment.includes('Δ compares PR Goccia median against the same-runner main Goccia median; overlapping min/max ranges are treated as unchanged noise'), 'comparison comment explains delta basis and overlap classifier');
}

console.log('awfy-driver: hardware counters...');
{
  const perfText = [
    '# started on Tue Oct 27 2026',
    '',
    '1891235675,,instructions:u,455491637,100.00,,',
    '456097787,,cycles:u,455491637,100.00,,',
    '618686,,branch-misses:u,455491637,100.00,,',
    '<not supported>,,L1-icache-load-misses:u,0,100.00,,',
    '903,,iTLB-load-misses:u,455491637,83.00,,',
  ].join('\n');
  const parsed = parsePerfStat(perfText);
  assertEqual(parsed.counters.instructions, 1891235675, 'perf parser reads instructions');
  assertEqual(parsed.counters.cycles, 456097787, 'perf parser reads cycles');
  assertEqual(parsed.counters.icacheMisses, null, 'perf parser marks an unsupported event as null');
  assertEqual(parsed.multiplexed, true, 'perf parser flags a multiplexed (scaled) event');

  const offLinux = probeCounters({ platform: 'darwin' });
  assertEqual(offLinux.available, false, 'counters are unavailable outside Linux');
  assert(offLinux.reason.includes('darwin'), 'unavailable counters say why');

  const missingPerf = probeCounters({
    platform: 'linux',
    spawn: () => ({ error: Object.assign(new Error('spawn perf ENOENT'), { code: 'ENOENT' }) }),
  });
  assertEqual(missingPerf.available, false, 'counters are unavailable without perf');
  assert(missingPerf.reason.includes('ENOENT'), 'missing perf names the spawn error');

  const forbidden = probeCounters({
    platform: 'linux',
    spawn: () => ({ status: 255, stderr: 'Error:\nAccess to performance monitoring and observability operations is limited.\n' }),
  });
  assertEqual(forbidden.available, false, 'counters are unavailable when perf_event is not permitted');
  assert(forbidden.reason.includes('Access to performance monitoring'), 'the reason includes the line after perf\'s bare "Error:"');

  const partial = probeCounters({
    platform: 'linux',
    spawn: () => ({ status: 0, stderr: perfText }),
  });
  assertEqual(partial.available, true, 'counters are available when instructions count');
  assertEqual(partial.events.map((entry) => entry.key).join(','), 'instructions,cycles,branchMisses,itlbMisses',
    'only the events perf counted are requested');

  const sample = (engine, instructions, cycles, icacheMisses) => ({
    engine,
    outcome: 'ok',
    durationMicros: cycles / 1000,
    checksum: 'verified',
    counters: { instructions, cycles, icacheMisses, multiplexed: false },
  });
  const summary = buildMetricTargetSummary([
    sample('goccia-baseline', 1000, 4000, 100),
    sample('goccia-candidate', 900, 4100, 150),
    sample('goccia-baseline', 1000, 4200, 120),
    sample('goccia-candidate', 900, 4300, 130),
    { engine: 'qjs', outcome: 'ok', durationMicros: 1, checksum: 'verified' },
  ], { field: 'durationMicros', outputPrefix: 'Micros', ratioDirection: 'lower-is-better' });
  assertEqual(summary.engineStats['goccia-baseline'].counters.instructions.median, 1000, 'counter summary keeps the median');
  assertEqual(summary.engineStats['goccia-candidate'].counters.cycles.min, 4100, 'counter summary keeps the minimum');
  assertEqual(summary.engineStats.qjs.counters, undefined, 'engines without counters get no counter summary');
  assertEqual(summary.counterRatios['goccia-candidate_over_goccia-baseline'].instructions, 0.9, 'counter ratios compare medians');
  assertEqual(summary.counterRatios['goccia-candidate_over_goccia-baseline'].icacheMisses, 140 / 110, 'counter ratios cover cache misses');

  const target = { name: 'Richards', summary };
  assertEqual(counterDeltaCell(target, 'instructions'), '-10.00%', 'instruction delta is the change in work');
  assertEqual(counterDeltaCell(target, 'itlbMisses'), '-', 'a counter without data renders as a dash');
  const zeroTarget = { summary: { counterRatios: { 'goccia-candidate_over_goccia-baseline': { icacheMisses: 0 } } } };
  assertEqual(counterDeltaCell(zeroTarget, 'icacheMisses'), '-100.00%', 'a count that falls to zero renders as -100%');
  const scaledTarget = {
    summary: {
      engineStats: { 'goccia-baseline': { counters: { multiplexed: true } }, 'goccia-candidate': { counters: { multiplexed: false } } },
      counterRatios: { 'goccia-candidate_over_goccia-baseline': { instructions: 0.9 } },
    },
  };
  assertEqual(counterDeltaCell(scaledTarget, 'instructions'), '-10.00% (scaled)', 'a multiplexed count is marked as scaled');

  const counterComment = buildAwfyReportComment({
    metadata: {
      options: { repetitions: 2 },
      counters: { available: true, reason: null, events: ['instructions:u'] },
      engines: [{ name: 'goccia-baseline', version: '' }, { name: 'goccia-candidate', version: '' }],
    },
    targets: [{ name: 'Richards', kind: 'awfy', summary: { ...summary, ratios: { 'goccia-candidate_over_goccia-baseline': 1.02 } } }],
    geomeanRatios: {},
  });
  assert(counterComment.includes('| Target | Status | Goccia main | Goccia PR | Δ PR vs main | Instructions | L1i misses |'),
    'comment adds counter columns when both Goccia binaries were counted, and shows only the engines that ran');
  assert(counterComment.includes('| -10.00% | +27.27% |'), 'comment renders instruction and L1i miss deltas');
  assert(counterComment.includes('Instructions do not depend on code placement'), 'comment explains how to read the counters');
  assert(counterComment.includes('negative is fewer'), 'comment states the sign of the counter columns');

  const unavailableCounters = buildAwfyReportComment({
    metadata: {
      options: { repetitions: 2 },
      counters: { available: false, reason: 'hardware counters need Linux perf_event (platform darwin)', events: [] },
      engines: [{ name: 'goccia-baseline', version: '' }, { name: 'goccia-candidate', version: '' }],
    },
    targets: [{ name: 'Richards', kind: 'awfy', summary: { engineStats: {}, ratios: {} } }],
    geomeanRatios: {},
  });
  assert(!unavailableCounters.includes('| Instructions |'), 'comment omits counter columns without counters');
  assert(unavailableCounters.includes('not recorded: hardware counters need Linux perf_event'), 'comment says why counters are missing');

  const options = parseArgs(['--counters', '--cpu', '3', '--fresh-copies', '--alternate-order', '--warmup=1']);
  assertEqual(options.counters, true, '--counters enables counters');
  assertEqual(options.cpu, 3, '--cpu takes a CPU number');
  assertEqual(options.freshCopies, true, '--fresh-copies is parsed');
  assertEqual(options.alternateOrder, true, '--alternate-order is parsed');
  assertEqual(options.warmup, 1, '--warmup takes a count');

  const copyDir = fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-awfy-copies-'));
  try {
    const binary = path.join(copyDir, 'runner');
    fs.writeFileSync(binary, 'binary bytes');
    const engines = [{ name: 'goccia-baseline', kind: 'goccia', command: binary }, { name: 'qjs', kind: 'qjs', command: 'qjs' }];
    refreshBinaryCopies(engines, copyDir, 0);
    const first = engines[0].sampleCommand;
    assert(first && first !== binary && fs.readFileSync(first, 'utf8') === 'binary bytes', 'a fresh copy holds the same bytes');
    refreshBinaryCopies(engines, copyDir, 1);
    assert(engines[0].sampleCommand !== first && fs.existsSync(first), 'the next repetition runs a new copy, beside the previous one');
    assertEqual(engines[1].sampleCommand, undefined, 'reference engines are not copied');
    const second = engines[0].sampleCommand;
    releaseBinaryCopies(engines);
    assert(!fs.existsSync(first) && !fs.existsSync(second), 'a finished target releases its copies');
    assertEqual(engines[0].sampleCommand, null, 'after release the engine runs its own binary');
  } finally {
    fs.rmSync(copyDir, { recursive: true, force: true });
  }
}

console.log('awfy-driver: counted samples through a stand-in perf...');
{
  const hybrid = parsePerfStat([
    '100,,cpu_core/instructions:u/,10,100.00,,',
    '20,,cpu_atom/instructions:u/,10,100.00,,',
    '<not counted>,,cpu_atom/cycles:u/,0,0.00,,',
    '300,,cpu_core/cycles:u/,10,100.00,,',
  ].join('\n'));
  assertEqual(hybrid.counters.instructions, 120, 'hybrid CPUs: per-core-type counts add up');
  assertEqual(hybrid.counters.cycles, 300, 'hybrid CPUs: an uncounted core type adds nothing');

  const command = counterCommand('runner', ['a.js'], { perf: 'perf', events: [{ event: 'instructions:u' }] }, 'out.csv', 2500);
  assertEqual(command.args.join(' '), 'stat -x, -o out.csv -e instructions:u -- timeout --kill-after=1 2.5 runner a.js',
    'a counted sample runs under timeout, which enforces the limit to the millisecond');
  assertEqual(counterCommand('r', [], { events: [{ event: 'instructions:u' }] }, 'o', 100).args[9], '0.1',
    'a limit under a second stays under a second');
  assertEqual(uncountedProcess({ status: 124 }).error.code, 'ETIMEDOUT', 'timeout\'s exit status is a timeout');
  assertEqual(uncountedProcess({ status: 137 }).signal, 'SIGKILL', 'a killed sample reports its signal');
  assertEqual(uncountedProcess({ status: 1 }).status, 1, 'an ordinary exit status is kept');

  const ciOptions = parseCiReportArgs(['--awfy-dir', 'x', '--counters']);
  assertEqual(ciOptions.counters, true, 'awfy-ci-report takes --counters');

  const sandbox = fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-awfy-perf-'));
  try {
    // A stand-in perf: it writes fixed counts to its -o file, then runs the
    // command after `--`, so the whole path from probe to report is exercised.
    const perf = path.join(sandbox, 'perf');
    fs.writeFileSync(perf, [
      '#!/bin/sh',
      'out=""',
      'while [ "$1" != "--" ]; do [ "$1" = "-o" ] && out="$2"; shift; done',
      'shift',
      'counts="1000,,instructions:u,1,100.00,,\n2000,,cycles:u,1,100.00,,\n5,,branch-misses:u,1,100.00,,\n7,,L1-icache-load-misses:u,1,100.00,,\n1,,iTLB-load-misses:u,1,100.00,,"',
      'if [ -n "$out" ]; then printf "$counts\\n" > "$out"; else printf "$counts\\n" >&2; fi',
      'exec "$@"',
      '',
    ].join('\n'));
    fs.chmodSync(perf, 0o755);
    const runner = path.join(sandbox, 'runner');
    fs.writeFileSync(runner, [
      '#!/bin/sh',
      `echo '${RESULT_MARKER}{"durationMicros":5,"checksum":"1","verificationPassed":true}'`,
      '',
    ].join('\n'));
    fs.chmodSync(runner, 0o755);
    const output = path.join(sandbox, 'report.json');
    const proc = spawnSync(process.execPath, [
      path.join(__dirname, 'awfy-driver.js'),
      '--probe', 'loop-dispatch-floor:1',
      '--engines', 'goccia',
      '--goccia-baseline', runner,
      '--goccia-candidate', runner,
      '--repetitions', '2',
      '--warmup', '1',
      '--counters',
      '--fresh-copies',
      '--alternate-order',
      '--output', output,
    ], { encoding: 'utf8', env: { ...process.env, PATH: `${sandbox}${path.delimiter}${process.env.PATH}` } });
    if (process.platform === 'linux') {
      assertEqual(proc.status, 0, `driver run with the stand-in perf succeeds: ${proc.stderr}`);
      const report = JSON.parse(fs.readFileSync(output, 'utf8'));
      const target = report.targets[0];
      assertEqual(report.metadata.counters.available, true, 'the stand-in perf counts');
      assertEqual(target.samples.length, 4, 'warm-up samples are not recorded');
      // Rounds alternate counting the warm-up, so the first recorded one is
      // the second round and runs the candidate first.
      assertEqual(target.samples[0].engine, 'goccia-candidate', 'alternate order: the second round starts with the candidate');
      assertEqual(target.samples[2].engine, 'goccia-baseline', 'alternate order: the third round starts with the baseline');
      assert(target.samples.every((sample) => sample.counters?.instructions === 1000), 'every sample carries its counts');
      assert(target.samples.every((sample) => !sample.command.includes(runner)), 'samples run fresh copies of the binary');
      assertEqual(target.summary.counterRatios['goccia-candidate_over_goccia-baseline'].instructions, 1,
        'counter ratios compare the two binaries');
    } else {
      assert(proc.status === 0 || proc.status === 1, 'driver run outside Linux ends');
    }
  } finally {
    fs.rmSync(sandbox, { recursive: true, force: true });
  }
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
  assertEqual(
    expectedEnginesForRun({
      engines: 'goccia,qjs,node',
      gocciaBaseline: 'main-loader',
      gocciaCandidate: 'pr-loader',
    }).join(','),
    'goccia-baseline,goccia-candidate,qjs,node',
    'ci report validation expands goccia into baseline/candidate engines',
  );
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

  validateReport({
    metadata: { options: { repetitions: 5 } },
    targets: [{
      kind: 'awfy',
      name: 'NBody',
      summary: {
        checksumAgreement: { ok: true, checksums: ['verified'] },
        engineStats: {
          'goccia-baseline': { ok: 5, rawCount: 5, medianMicros: 1000 },
          'goccia-candidate': { ok: 5, rawCount: 5, medianMicros: 900 },
          qjs: { ok: 5, rawCount: 5, medianMicros: 500 },
          node: { ok: 5, rawCount: 5, medianMicros: 250 },
        },
      },
    }],
  }, manifest, ['goccia-baseline', 'goccia-candidate', 'qjs', 'node']);
  assert(true, 'ci report validation accepts baseline/candidate Goccia samples');
}

console.log(`awfy-driver: ${passed} assertions passed.`);
