#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const DEFAULT_GOCCIA_FLAGS = [
  '--mode=bytecode',
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
    engines.push({
      name,
      kind: 'goccia',
      command,
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
  if (!parsed.verificationPassed) return 'verification-failed';
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
  const startedAt = new Date().toISOString();
  const proc = spawnSync(engine.command, args, {
    encoding: 'utf8',
    timeout: options.timeoutMs,
    maxBuffer: 16 * 1024 * 1024,
  });
  const stdout = proc.stdout || '';
  const stderr = proc.stderr || '';
  const parsed = parseMarkedResult(stdout, marker);
  const outcome = classifyProcessOutcome(proc, parsed, stdout, stderr);
  return {
    engine: engine.name,
    repetition,
    startedAt,
    outcome,
    ...(normalizeResult(parsed, target)),
    exitCode: proc.status,
    signal: proc.signal || null,
    command: [engine.command, ...args],
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
  const checksums = new Set(samples
    .filter((sample) => sample.outcome === 'ok')
    .map((sample) => sample.checksum));
  return { ok: checksums.size <= 1, checksums: [...checksums].sort() };
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
  return { checksumAgreement: checksumAgreement(samples), engineStats, ratios };
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
  resolveEngines,
  runEngineSample,
  summarizeMetric,
};
