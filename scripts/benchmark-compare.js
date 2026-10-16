#!/usr/bin/env node

// Builds the PR "Benchmark Results" comment by comparing the PR build against a
// `main` build measured **on the same runner, back-to-back** in the same job.
//
// Why this module exists: the comparison used to read a `main` baseline from the
// Actions cache, written by a *different* job on a *different* GitHub-hosted
// runner at a *different* time. Cross-runner/cross-time variance dwarfs each
// run's own min/max spread, so the range-overlap classifier flipped large
// numbers of benchmarks to improved/regressed even when the diff could not have
// affected them (issue #815). Feeding the classifier two same-runner runs makes
// the systematic offset cancel; the range-overlap rule then only sees within-run
// noise, which it is designed for. See docs/adr/0076.
//
// The classifier itself is unchanged from the original inline implementation
// (range overlap on each run's own min/max). This module exists so that logic
// is a single tested unit instead of ~200 lines embedded in workflow YAML.

const fmtOps = (n) => Math.round(n).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');

const fmtPercent = (change) => {
  if (change == null) return 'N/A';
  return `${change >= 0 ? '+' : ''}${change.toFixed(1)}%`;
};

const fmtAvg = (changes) => {
  if (changes.length === 0) return '';
  const avg = changes.reduce((a, b) => a + b, 0) / changes.length;
  return `avg ${fmtPercent(avg)}`;
};

const fmtDelta = (classification, change) => {
  if (change == null) return classification === 'new' ? '🆕' : 'N/A';
  if (classification === 'improved') return `🟢 ${fmtPercent(change)}`;
  if (classification === 'regressed') return `🔴 ${fmtPercent(change)}`;
  if (classification === 'unchanged') return `~ overlap (${fmtPercent(change)})`;
  return '🆕';
};

const fmtRange = (entry) => {
  if (!entry) return '—';
  if (entry.minOpsPerSec === entry.maxOpsPerSec) return fmtOps(entry.opsPerSec);
  return `${fmtOps(entry.minOpsPerSec)}..${fmtOps(entry.maxOpsPerSec)}`;
};

const fmtBenchmark = (entry) => {
  if (!entry) return '—';
  if (entry.minOpsPerSec === entry.maxOpsPerSec) return `${fmtOps(entry.opsPerSec)} ops/sec`;
  return `${fmtOps(entry.opsPerSec)} ops/sec [${fmtRange(entry)}]`;
};

const fmtCell = (baseEntry, currentEntry) => {
  if (baseEntry) return `${fmtBenchmark(baseEntry)} → ${fmtBenchmark(currentEntry)}`;
  return fmtBenchmark(currentEntry);
};

const normalizeBenchmark = (benchmark) => ({
  opsPerSec: benchmark.opsPerSec,
  minOpsPerSec: benchmark.minOpsPerSec ?? benchmark.opsPerSec,
  maxOpsPerSec: benchmark.maxOpsPerSec ?? benchmark.opsPerSec,
  variancePercentage: benchmark.variancePercentage ?? 0,
});

const calculateChange = (baseEntry, currentEntry) => {
  if (!baseEntry || !baseEntry.opsPerSec) return null;
  return ((currentEntry.opsPerSec - baseEntry.opsPerSec) / baseEntry.opsPerSec) * 100;
};

// Range overlap on each run's own min/max range. Because both ranges now come
// from the same runner, the only thing separating them is within-run noise: a
// classification only fires when the PR range sits *entirely* above (improved)
// or *entirely* below (regressed) the main range. Overlap → unchanged noise.
const classify = (baseEntry, currentEntry) => {
  if (!baseEntry) return 'new';
  if (currentEntry.minOpsPerSec > baseEntry.maxOpsPerSec) return 'improved';
  if (currentEntry.maxOpsPerSec < baseEntry.minOpsPerSec) return 'regressed';
  return 'unchanged';
};

function buildMap(data) {
  const map = {};
  if (!data) return map;
  for (const file of data.files) {
    const m = {};
    for (const b of file.benchmarks) {
      if (!b.error) m[`${b.suite} > ${b.name}`] = normalizeBenchmark(b);
    }
    map[file.fileName] = m;
  }
  return map;
}

// Median of each benchmark's own within-run variancePercentage: an honest,
// measured "noise floor" surfaced so readers can judge whether a sub-noise delta
// is meaningful. It is the spread the classifier already absorbs as overlap.
function medianVariance(data) {
  if (!data) return null;
  const values = [];
  for (const file of data.files) {
    for (const b of file.benchmarks) {
      if (!b.error && typeof b.variancePercentage === 'number' && b.variancePercentage > 0) {
        values.push(b.variancePercentage);
      }
    }
  }
  if (values.length === 0) return null;
  values.sort((a, b) => a - b);
  const mid = Math.floor(values.length / 2);
  return values.length % 2 ? values[mid] : (values[mid - 1] + values[mid]) / 2;
}

const newStats = () => ({ improved: 0, regressed: 0, unchanged: 0, new: 0, changes: [] });

// options:
//   bytecodeData                    — required PR-run report (this run)
//   bytecodeMain                    — same-runner `main`-build report (may be null)
//   profileDiff                     — optional pre-rendered deterministic profile markdown
//   runnerLabel                     — e.g. 'ubuntu-latest x64'
// returns { body } — markdown WITHOUT the HTML comment marker.
function buildBenchmarkComparison(options) {
  const currentData = options.bytecodeData;
  const mainData = options.bytecodeMain || null;
  const runnerLabel = options.runnerLabel || 'ubuntu-latest x64';

  const hasMain = mainData != null;
  const mainMap = buildMap(mainData);

  const totals = newStats();
  let totalBenchmarks = 0;
  const fileBlocks = [];

  for (const file of currentData.files) {
    const fileName = file.fileName;
    const base = mainMap[fileName] || {};
    const fileS = newStats();
    let fileRows = '';
    const benches = file.benchmarks.filter((b) => !b.error);
    totalBenchmarks += benches.length;

    if (hasMain) {
      fileRows += '| Benchmark | main → PR | Δ |\n';
      fileRows += '|-----------|-----------|---|\n';
    } else {
      fileRows += '| Benchmark | ops/sec |\n';
      fileRows += '|-----------|---------|\n';
    }

    for (const bench of benches) {
      const key = `${bench.suite} > ${bench.name}`;
      const current = normalizeBenchmark(bench);
      const baseEntry = base[key];

      if (hasMain) {
        let delta;
        if (baseEntry) {
          const c = calculateChange(baseEntry, current);
          const classification = classify(baseEntry, current);
          if (c != null) fileS.changes.push(c);
          fileS[classification]++;
          delta = fmtDelta(classification, c);
        } else {
          delta = '🆕';
          fileS.new++;
        }
        fileRows += `| ${bench.name} | ${fmtCell(baseEntry, current)} | ${delta} |\n`;
      } else {
        fileRows += `| ${bench.name} | ${fmtRange(current)} |\n`;
      }
    }

    for (const k of ['improved', 'regressed', 'unchanged', 'new']) totals[k] += fileS[k];
    totals.changes.push(...fileS.changes);

    const hasSignificant = fileS.improved + fileS.regressed > 0;

    function fileSummaryOf(s) {
      const p = [];
      if (s.improved > 0) p.push(`🟢 ${s.improved}`);
      if (s.regressed > 0) p.push(`🔴 ${s.regressed}`);
      if (s.new > 0) p.push(`🆕 ${s.new}`);
      if (s.unchanged > 0) p.push(`${s.unchanged} unch.`);
      const avg = fmtAvg(s.changes);
      return `${p.length > 0 ? p.join(', ') : `${s.changes.length + s.new}`}${avg ? ` · ${avg}` : ''}`;
    }

    const fileSummary = hasMain ? fileSummaryOf(fileS) : `${benches.length} benchmarks`;

    fileBlocks.push({ fileName, fileSummary, fileRows, hasSignificant });
  }

  let body = '## Benchmark Results\n\n';

  if (hasMain) {
    const p = [];
    if (totals.improved > 0) p.push(`🟢 ${totals.improved} improved`);
    if (totals.regressed > 0) p.push(`🔴 ${totals.regressed} regressed`);
    if (totals.new > 0) p.push(`🆕 ${totals.new} new`);
    if (totals.unchanged > 0) p.push(`${totals.unchanged} unchanged`);
    const avg = fmtAvg(totals.changes);
    body += `**${totalBenchmarks}** benchmarks · PR vs same-runner \`main\` build\n\n`;
    body += `${p.join(' · ')}${avg ? ` · ${avg}` : ''}\n\n`;

    const noise = medianVariance(currentData);
    if (noise != null) {
      body += `Typical per-run noise (median variance): ±${noise.toFixed(1)}%. Deltas within noise overlap and read as unchanged.\n\n`;
    }
  } else {
    body += `**${totalBenchmarks}** benchmarks (no same-runner \`main\` build to compare)\n\n`;
  }

  for (const block of fileBlocks) {
    const shortName = block.fileName.replace(/^benchmarks\//, '');
    const open = block.hasSignificant ? ' open' : '';
    body += `<details${open}>\n`;
    body += `<summary><strong>${shortName}</strong> — ${block.fileSummary}</summary>\n\n`;
    body += block.fileRows;
    body += `\n</details>\n\n`;
  }

  if (options.profileDiff) {
    body += options.profileDiff + '\n\n';
  }

  body += hasMain
    ? `<sub>Measured on ${runnerLabel}. Each PR run also builds the \`main\` base and benchmarks it back-to-back on the same runner after a warm-up discard, so the ranges compare two runs measured under the same conditions; overlapping min/max ranges are treated as unchanged noise. Percentage deltas are secondary context. See docs/adr/0076.</sub>`
    : `<sub>Measured on ${runnerLabel}. No \`main\` build was available to compare against, so these are raw PR-run numbers. See docs/adr/0076.</sub>`;

  return { body };
}

module.exports = {
  buildBenchmarkComparison,
  classify,
  calculateChange,
  normalizeBenchmark,
  buildMap,
  medianVariance,
};
