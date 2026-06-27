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
//   interpData, bytecodeData       — required PR-run reports (this run)
//   interpMain, bytecodeMain        — same-runner `main`-build reports (may be null)
//   profileDiff                     — optional pre-rendered deterministic profile markdown
//   runnerLabel                     — e.g. 'ubuntu-latest x64'
// returns { body } — markdown WITHOUT the HTML comment marker.
function buildBenchmarkComparison(options) {
  const interpData = options.interpData;
  const bytecodeData = options.bytecodeData;
  const interpMain = options.interpMain || null;
  const bytecodeMain = options.bytecodeMain || null;
  const runnerLabel = options.runnerLabel || 'ubuntu-latest x64';

  const hasAnyMain = interpMain || bytecodeMain;

  const interpMainMap = buildMap(interpMain);
  const bytecodeMainMap = buildMap(bytecodeMain);
  const bytecodeCurrentMap = buildMap(bytecodeData);

  const totals = { interp: newStats(), bytecode: newStats() };
  let totalBenchmarks = 0;
  const fileBlocks = [];

  for (const file of interpData.files) {
    const fileName = file.fileName;
    const iBase = interpMainMap[fileName] || {};
    const bBase = bytecodeMainMap[fileName] || {};
    const bCurrent = bytecodeCurrentMap[fileName] || {};
    const fileS = { interp: newStats(), bytecode: newStats() };
    let fileRows = '';
    const benches = file.benchmarks.filter((b) => !b.error);
    totalBenchmarks += benches.length;

    if (hasAnyMain) {
      fileRows += '| Benchmark | Interpreted (main → PR) | Δ | Bytecode (main → PR) | Δ |\n';
      fileRows += '|-----------|------------|---|---------|---|\n';
    } else {
      fileRows += '| Benchmark | Interpreted | Bytecode |\n';
      fileRows += '|-----------|------------|----------|\n';
    }

    for (const bench of benches) {
      const key = `${bench.suite} > ${bench.name}`;
      const iCurrent = normalizeBenchmark(bench);
      const bCurrentEntry = bCurrent[key];
      const iBaseEntry = iBase[key];
      const bBaseEntry = bBase[key];

      if (hasAnyMain) {
        let iDelta;
        if (iBaseEntry) {
          const c = calculateChange(iBaseEntry, iCurrent);
          const classification = classify(iBaseEntry, iCurrent);
          if (c != null) fileS.interp.changes.push(c);
          fileS.interp[classification]++;
          iDelta = fmtDelta(classification, c);
        } else {
          iDelta = '🆕';
          fileS.interp.new++;
        }

        let bDelta;
        if (bCurrentEntry && bBaseEntry) {
          const c = calculateChange(bBaseEntry, bCurrentEntry);
          const classification = classify(bBaseEntry, bCurrentEntry);
          if (c != null) fileS.bytecode.changes.push(c);
          fileS.bytecode[classification]++;
          bDelta = fmtDelta(classification, c);
        } else if (bCurrentEntry) {
          bDelta = '🆕';
          fileS.bytecode.new++;
        } else {
          bDelta = '—';
        }

        fileRows += `| ${bench.name} | ${fmtCell(iBaseEntry, iCurrent)} | ${iDelta} | ${bCurrentEntry ? fmtCell(bBaseEntry, bCurrentEntry) : '—'} | ${bDelta} |\n`;
      } else {
        fileRows += `| ${bench.name} | ${fmtRange(iCurrent)} | ${bCurrentEntry ? fmtRange(bCurrentEntry) : '—'} |\n`;
      }
    }

    for (const mode of ['interp', 'bytecode']) {
      for (const k of ['improved', 'regressed', 'unchanged', 'new']) totals[mode][k] += fileS[mode][k];
      totals[mode].changes.push(...fileS[mode].changes);
    }

    const hasSignificant = fileS.interp.improved + fileS.interp.regressed + fileS.bytecode.improved + fileS.bytecode.regressed > 0;

    function modeSummary(label, s) {
      const p = [];
      if (s.improved > 0) p.push(`🟢 ${s.improved}`);
      if (s.regressed > 0) p.push(`🔴 ${s.regressed}`);
      if (s.new > 0) p.push(`🆕 ${s.new}`);
      if (s.unchanged > 0) p.push(`${s.unchanged} unch.`);
      const avg = fmtAvg(s.changes);
      return `${label}: ${p.length > 0 ? p.join(', ') : `${s.changes.length + s.new}`}${avg ? ` · ${avg}` : ''}`;
    }

    const fileSummary = hasAnyMain
      ? modeSummary('Interp', fileS.interp) + ' · ' + modeSummary('Bytecode', fileS.bytecode)
      : `${benches.length} benchmarks`;

    fileBlocks.push({ fileName, fileSummary, fileRows, hasSignificant });
  }

  let body = '## Benchmark Results\n\n';

  if (hasAnyMain) {
    function overallMode(label, s) {
      const p = [];
      if (s.improved > 0) p.push(`🟢 ${s.improved} improved`);
      if (s.regressed > 0) p.push(`🔴 ${s.regressed} regressed`);
      if (s.new > 0) p.push(`🆕 ${s.new} new`);
      if (s.unchanged > 0) p.push(`${s.unchanged} unchanged`);
      const avg = fmtAvg(s.changes);
      return `**${label}:** ${p.join(' · ')}${avg ? ` · ${avg}` : ''}`;
    }
    body += `**${totalBenchmarks}** benchmarks · PR vs same-runner \`main\` build\n\n`;
    body += overallMode('Interpreted', totals.interp) + '\\\n';
    body += overallMode('Bytecode', totals.bytecode) + '\n\n';

    const iNoise = medianVariance(interpData);
    const bNoise = medianVariance(bytecodeData);
    const noiseParts = [];
    if (iNoise != null) noiseParts.push(`interpreted ±${iNoise.toFixed(1)}%`);
    if (bNoise != null) noiseParts.push(`bytecode ±${bNoise.toFixed(1)}%`);
    if (noiseParts.length > 0) {
      body += `Typical per-run noise (median variance): ${noiseParts.join(', ')}. Deltas within noise overlap and read as unchanged.\n\n`;
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

  body += hasAnyMain
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
