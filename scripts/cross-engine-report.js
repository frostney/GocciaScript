#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const DEFAULT_GOCCIA_FLAGS = [
  '--compat-traditional-for-loop',
  '--compat-while-loops',
  '--compat-for-in-loop',
  '--compat-function',
];

function expandGocciaEngines(requested, options) {
  const engines = [];
  for (const name of requested) {
    if (name === 'goccia' && (options.gocciaBaseline || options.gocciaCandidate)) {
      if (options.gocciaBaseline) engines.push('goccia-baseline');
      if (options.gocciaCandidate) engines.push('goccia-candidate');
    } else {
      engines.push(name);
    }
  }
  return engines;
}

function resolveEngines(options) {
  const requested = options.engines || ['goccia', 'qjs', 'node'];
  const engines = [];
  const addGoccia = (name, command) => {
    const sourceCommit = name === 'goccia-baseline'
      ? options.gocciaBaselineCommit
      : name === 'goccia-candidate'
        ? options.gocciaCandidateCommit
        : options.gocciaCommit;
    engines.push({
      name,
      kind: 'goccia',
      command,
      sourceCommit: sourceCommit || null,
      baseArgs: DEFAULT_GOCCIA_FLAGS.concat(options.gocciaFlags || []),
    });
  };

  for (const name of expandGocciaEngines(requested, options)) {
    if (name === 'goccia') {
      addGoccia('goccia', options.goccia);
    } else if (name === 'goccia-baseline') {
      if (!options.gocciaBaseline) throw new Error('--goccia-baseline is required for engine goccia-baseline');
      addGoccia('goccia-baseline', options.gocciaBaseline);
    } else if (name === 'goccia-candidate') {
      if (!options.gocciaCandidate) throw new Error('--goccia-candidate is required for engine goccia-candidate');
      addGoccia('goccia-candidate', options.gocciaCandidate);
    } else if (name === 'qjs') {
      engines.push({ name: 'qjs', kind: 'qjs', command: options.qjs, baseArgs: [] });
    } else if (name === 'node') {
      engines.push({ name: 'node', kind: 'node', command: options.node, baseArgs: [] });
    } else {
      throw new Error(`Unknown engine: ${name}`);
    }
  }
  return engines;
}

function engineAllowedForTarget(engine, target) {
  if (!target.engines) return true;
  return target.engines.includes(engine.name) || target.engines.includes(engine.kind);
}

function profileArgs(options, target, engine, repetition) {
  if (!options.profile || engine.kind !== 'goccia') return [];
  const args = [`--profile=${options.profile}`];
  if (options.profileDir) {
    fs.mkdirSync(options.profileDir, { recursive: true });
    args.push(`--profile-output=${path.join(options.profileDir, `${target.name}-${engine.name}-${repetition}.json`)}`);
  }
  return args;
}

// Hardware counters for Goccia samples (issue #1917). Instructions retired do
// not depend on where the code lands in memory, so they separate work from
// placement; the instruction-cache, iTLB and branch-miss counts show placement.
// Linux perf_event only, counted in user space, five events so that no event
// is multiplexed on a core with six counters (a multiplexed count is scaled
// and stops being exact).
const COUNTER_EVENTS = [
  { key: 'instructions', event: 'instructions:u' },
  { key: 'cycles', event: 'cycles:u' },
  { key: 'branchMisses', event: 'branch-misses:u' },
  { key: 'icacheMisses', event: 'L1-icache-load-misses:u' },
  { key: 'itlbMisses', event: 'iTLB-load-misses:u' },
];
const COUNTER_KEYS = COUNTER_EVENTS.map((entry) => entry.key);

// Parses `perf stat -x,` output: value, unit, event, running time, percentage.
// On a hybrid CPU perf reports an event once per core type, as
// `cpu_core/instructions:u/` and `cpu_atom/instructions:u/`; those lines add
// up to the process's count.
function parsePerfStat(text, events = COUNTER_EVENTS) {
  const byEvent = new Map(events.map((entry) => [entry.event, entry.key]));
  const counters = {};
  let multiplexed = false;
  for (const line of text.split(/\r?\n/)) {
    const fields = line.split(',');
    if (fields.length < 3) continue;
    const name = fields[2].replace(/^[a-z_]+\/(.+)\/$/, '$1');
    const key = byEvent.get(name);
    if (!key) continue;
    const value = Number(fields[0]);
    if (fields[0] === '' || !Number.isFinite(value)) {
      if (!(key in counters)) counters[key] = null;
      continue;
    }
    counters[key] = (typeof counters[key] === 'number' ? counters[key] : 0) + value;
    const percentage = Number(fields[4]);
    if (Number.isFinite(percentage) && percentage < 100) multiplexed = true;
  }
  return { counters, multiplexed };
}

// Whether perf can count the events here, and which of them. Unavailable on
// other systems, without perf, or where perf_event_paranoid forbids it; the
// report records why and the run goes on without counters.
function probeCounters({ perf = 'perf', platform = process.platform, spawn = spawnSync } = {}) {
  if (platform !== 'linux') {
    return { available: false, reason: `hardware counters need Linux perf_event (platform ${platform})`, events: [] };
  }
  const proc = spawn(perf, ['stat', '-x,', '-e', COUNTER_EVENTS.map((entry) => entry.event).join(','), '--', 'timeout', '60', process.execPath, '-e', ''], {
    encoding: 'utf8',
  });
  if (proc.error) {
    return { available: false, reason: `${perf} could not be run (${proc.error.code || proc.error.message})`, events: [] };
  }
  if (proc.status !== 0) {
    // perf's first line is often just `Error:`; the next one says why.
    const message = (proc.stderr || '').split(/\r?\n/).map((line) => line.trim()).filter(Boolean)
      .slice(0, 2).join(' ').slice(0, 240) || `exit ${proc.status}`;
    return { available: false, reason: `${perf} stat failed: ${message}`, events: [] };
  }
  const { counters } = parsePerfStat(proc.stderr || '');
  const events = COUNTER_EVENTS.filter((entry) => typeof counters[entry.key] === 'number');
  if (!events.some((entry) => entry.key === 'instructions')) {
    return { available: false, reason: 'perf cannot count instructions:u here', events: [] };
  }
  return { available: true, reason: null, events, perf };
}

// perf runs the sample as its child and does not end it when it is killed
// itself, so the time limit is enforced under perf by coreutils `timeout`:
// TERM at the limit (exit 124), KILL a second later. Its own few instructions
// are counted with the sample's, the same in every sample.
function counterCommand(command, args, counterSetup, outputFile, timeoutMs) {
  const seconds = Math.max(1, Math.ceil(timeoutMs / 1000));
  return {
    command: counterSetup.perf || 'perf',
    args: ['stat', '-x,', '-o', outputFile, '-e', counterSetup.events.map((entry) => entry.event).join(','), '--',
      'timeout', '--kill-after=1', String(seconds), command, ...args],
  };
}

// The process result of a sample run under perf and `timeout`, as if the
// sample had run alone: `timeout` exits 124 when it stopped the sample, and a
// sample killed by a signal makes them exit 128 + its number.
function uncountedProcess(proc) {
  if (proc.error || typeof proc.status !== 'number') return proc;
  if (proc.status === 124) {
    return { ...proc, error: Object.assign(new Error('sample timed out'), { code: 'ETIMEDOUT' }) };
  }
  if (proc.status > 128) {
    const signal = Object.entries(os.constants.signals).find(([, number]) => number === proc.status - 128);
    if (signal) return { ...proc, status: null, signal: signal[0] };
  }
  return proc;
}

function readCounters(outputFile, counterSetup) {
  let text = '';
  try {
    text = fs.readFileSync(outputFile, 'utf8');
  } catch {
    return null;
  } finally {
    fs.rmSync(outputFile, { force: true });
  }
  const { counters, multiplexed } = parsePerfStat(text, counterSetup.events);
  if (typeof counters.instructions !== 'number') return null;
  return { ...counters, multiplexed };
}

// Pins a sample to one CPU with taskset (Linux), so that every sample runs on
// the same core.
function pinnedCommand(command, args, cpu) {
  if (cpu === null || cpu === undefined) return { command, args };
  return { command: 'taskset', args: ['-c', String(cpu), command, ...args] };
}

function parseMarkedResult(stdout, marker) {
  const lines = stdout.split(/\r?\n/);
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    if (lines[index].startsWith(marker)) {
      return JSON.parse(lines[index].slice(marker.length));
    }
  }
  return null;
}

function classifyProcessOutcome(proc, parsed, stdout, stderr) {
  if (proc.error && proc.error.code === 'ETIMEDOUT') return 'timeout';
  const combined = `${stdout}\n${stderr}`.toLowerCase();
  if (/\bout[\s-]of[\s-]memory\b/.test(combined) || /\boom\b/.test(combined) || proc.signal === 'SIGKILL') return 'oom';
  if (!parsed) return proc.status === 0 ? 'missing-result' : 'crash';
  if (parsed.verificationPassed === false) return 'verification-failed';
  if (proc.status !== 0) return 'crash';
  return 'ok';
}

function runEngineSample({ engine, target, fileName, repetition, options, marker, normalizeResult }) {
  const args = [];
  if (engine.kind === 'goccia') {
    args.push(fileName, ...engine.baseArgs, ...(target.gocciaFlags || []), ...profileArgs(options, target, engine, repetition));
  } else {
    args.push(...engine.baseArgs, fileName);
  }
  const executable = engine.sampleCommand || engine.command;
  const counting = engine.kind === 'goccia' && options.counterSetup?.available;
  const ownCounterDir = counting && !options.counterDir
    ? fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-counters-'))
    : null;
  const counterFile = counting
    ? path.join(options.counterDir || ownCounterDir, `${engine.name}-${repetition}.csv`)
    : null;
  let invocation = counting
    ? counterCommand(executable, args, options.counterSetup, counterFile, options.timeoutMs)
    : { command: executable, args };
  invocation = pinnedCommand(invocation.command, invocation.args, options.cpu);
  const startedAt = new Date().toISOString();
  const spawned = spawnSync(invocation.command, invocation.args, {
    encoding: 'utf8',
    // Under perf, `timeout` ends the sample; this only catches perf itself.
    timeout: counting ? options.timeoutMs + 5000 : options.timeoutMs,
    maxBuffer: 16 * 1024 * 1024,
  });
  const proc = counting ? uncountedProcess(spawned) : spawned;
  const stdout = proc.stdout || '';
  const stderr = proc.stderr || '';
  const parsed = parseMarkedResult(stdout, marker);
  const outcome = classifyProcessOutcome(proc, parsed, stdout, stderr);
  const counters = counting ? readCounters(counterFile, options.counterSetup) : null;
  if (ownCounterDir) fs.rmSync(ownCounterDir, { recursive: true, force: true });
  return {
    engine: engine.name,
    repetition,
    startedAt,
    outcome,
    ...(normalizeResult(parsed, target)),
    ...(counting ? { counters } : {}),
    exitCode: proc.status,
    signal: proc.signal || null,
    command: [invocation.command, ...invocation.args],
    stdout: outcome === 'ok' ? '' : stdout.slice(0, 4000),
    stderr: outcome === 'ok' ? '' : stderr.slice(0, 4000),
  };
}

function median(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2;
}

function iqrFiltered(values) {
  if (values.length < 4) return [...values];
  const sorted = [...values].sort((a, b) => a - b);
  const firstQuartile = sorted[Math.floor(sorted.length / 4)];
  const thirdQuartile = sorted[Math.floor((sorted.length * 3) / 4)];
  const interquartileRange = thirdQuartile - firstQuartile;
  if (interquartileRange <= 0) return sorted;
  const lower = firstQuartile - 1.5 * interquartileRange;
  const upper = thirdQuartile + 1.5 * interquartileRange;
  return sorted.filter((value) => value >= lower && value <= upper);
}

function coefficientOfVariation(values) {
  if (values.length === 0) return null;
  const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
  if (mean === 0) return null;
  const variance = values.reduce((sum, value) => sum + ((value - mean) * (value - mean)), 0) / values.length;
  return (Math.sqrt(variance) / mean) * 100;
}

function outcomeCounts(samples) {
  return {
    ok: samples.filter((sample) => sample.outcome === 'ok').length,
    timeout: samples.filter((sample) => sample.outcome === 'timeout').length,
    crash: samples.filter((sample) => sample.outcome === 'crash').length,
    oom: samples.filter((sample) => sample.outcome === 'oom').length,
    verificationFailed: samples.filter((sample) => sample.outcome === 'verification-failed').length,
    missingResult: samples.filter((sample) => sample.outcome === 'missing-result').length,
  };
}

function summarizeMetric(samples, field, outputPrefix) {
  const values = samples
    .filter((sample) => sample.outcome === 'ok' && typeof sample[field] === 'number' && sample[field] >= 0)
    .map((sample) => sample[field]);
  const filtered = iqrFiltered(values);
  return {
    ...outcomeCounts(samples),
    rawCount: values.length,
    [`median${outputPrefix}`]: median(values),
    [`iqrMedian${outputPrefix}`]: median(filtered),
    [`min${outputPrefix}`]: values.length ? Math.min(...values) : null,
    [`max${outputPrefix}`]: values.length ? Math.max(...values) : null,
    coefficientOfVariation: coefficientOfVariation(values),
  };
}

function checksumAgreement(samples) {
  const checkedSamples = samples
    .filter((sample) => sample.outcome === 'ok')
    .map((sample) => sample.checksum)
    .filter((checksum) => checksum !== null && checksum !== undefined);
  const checksums = new Set(checkedSamples);
  return {
    available: checkedSamples.length > 0,
    checkedSampleCount: checkedSamples.length,
    ok: checkedSamples.length > 0 ? checksums.size <= 1 : null,
    checksums: [...checksums].sort(),
  };
}

function buildMetricTargetSummary(samples, { field, outputPrefix, ratioDirection }) {
  const byEngine = new Map();
  for (const sample of samples) {
    if (!byEngine.has(sample.engine)) byEngine.set(sample.engine, []);
    byEngine.get(sample.engine).push(sample);
  }
  const engineStats = {};
  for (const [engine, engineSamples] of byEngine.entries()) {
    engineStats[engine] = summarizeMetric(engineSamples, field, outputPrefix);
    const counters = summarizeCounters(engineSamples);
    if (counters) engineStats[engine].counters = counters;
  }
  const medianField = `median${outputPrefix}`;
  const ratios = {};
  const engines = Object.keys(engineStats);
  for (const left of engines) {
    for (const right of engines) {
      if (left === right) continue;
      const leftMedian = engineStats[left][medianField];
      const rightMedian = engineStats[right][medianField];
      if (leftMedian && rightMedian) {
        ratios[`${left}_over_${right}`] = ratioDirection === 'higher-is-better'
          ? rightMedian / leftMedian
          : leftMedian / rightMedian;
      }
    }
  }
  const counterRatios = {};
  for (const left of engines) {
    for (const right of engines) {
      if (left === right) continue;
      const leftCounters = engineStats[left].counters;
      const rightCounters = engineStats[right].counters;
      if (!leftCounters || !rightCounters) continue;
      const pair = {};
      for (const key of COUNTER_KEYS) {
        const leftValue = leftCounters[key]?.median;
        const rightValue = rightCounters[key]?.median;
        if (typeof leftValue === 'number' && typeof rightValue === 'number' && rightValue > 0) {
          pair[key] = leftValue / rightValue;
        }
      }
      if (Object.keys(pair).length > 0) counterRatios[`${left}_over_${right}`] = pair;
    }
  }
  const summary = { checksumAgreement: checksumAgreement(samples), engineStats, ratios };
  if (Object.keys(counterRatios).length > 0) summary.counterRatios = counterRatios;
  return summary;
}

// Per-counter median, min and max over an engine's ok samples that carry
// counters. Null when none does.
function summarizeCounters(samples) {
  const counted = samples.filter((sample) => sample.outcome === 'ok' && sample.counters);
  if (counted.length === 0) return null;
  const result = { rawCount: counted.length, multiplexed: counted.some((sample) => sample.counters.multiplexed) };
  for (const key of COUNTER_KEYS) {
    const values = counted.map((sample) => sample.counters[key]).filter((value) => typeof value === 'number');
    if (values.length === 0) continue;
    result[key] = { median: median(values), min: Math.min(...values), max: Math.max(...values) };
  }
  return result;
}

function buildGeomeanRatios(targetReports) {
  const buckets = new Map();
  for (const target of targetReports) {
    for (const [pair, ratio] of Object.entries(target.summary.ratios)) {
      if (!Number.isFinite(ratio) || ratio <= 0) continue;
      if (!buckets.has(pair)) buckets.set(pair, []);
      buckets.get(pair).push(Math.log(ratio));
    }
  }
  const result = {};
  for (const [pair, logs] of buckets.entries()) {
    result[pair] = Math.exp(logs.reduce((sum, value) => sum + value, 0) / logs.length);
  }
  return result;
}

module.exports = {
  COUNTER_EVENTS,
  COUNTER_KEYS,
  DEFAULT_GOCCIA_FLAGS,
  buildGeomeanRatios,
  buildMetricTargetSummary,
  checksumAgreement,
  classifyProcessOutcome,
  coefficientOfVariation,
  engineAllowedForTarget,
  expandGocciaEngines,
  iqrFiltered,
  median,
  parseMarkedResult,
  counterCommand,
  parsePerfStat,
  probeCounters,
  resolveEngines,
  runEngineSample,
  summarizeCounters,
  summarizeMetric,
  uncountedProcess,
};
