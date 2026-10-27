#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- awfy-results -->';
const COMPARISON_PAIR = 'goccia-candidate_over_goccia-baseline';
const LEGACY_MARKER = '<!-- awfy-smoke -->';
const DEFAULT_COMMENT_ENGINES = ['goccia', 'qjs', 'node'];
const COMPARISON_COMMENT_ENGINES = ['goccia-baseline', 'goccia-candidate', 'qjs', 'node'];
const ENGINE_LABELS = {
  goccia: 'Goccia',
  'goccia-baseline': 'Goccia main',
  'goccia-candidate': 'Goccia PR',
  qjs: 'QuickJS',
  node: 'NodeJS',
};

function formatMicros(value) {
  if (typeof value !== 'number') return '-';
  if (value >= 10000000) return `${(value / 1000000).toFixed(2)}s`;
  if (value >= 500) return `${(value / 1000).toFixed(2)}ms`;
  if (value >= 0.5) return `${value.toFixed(2)}µs`;
  return `${Math.round(value * 1000)}ns`;
}

function plural(count, singular) {
  return `${count} ${singular}${count === 1 ? '' : 's'}`;
}

function reportHasGocciaComparison(report) {
  const metadataEngines = report?.metadata?.engines || [];
  if (metadataEngines.some((engine) => engine.name === 'goccia-baseline' || engine.name === 'goccia-candidate')) {
    return true;
  }
  return (report?.targets || []).some((target) => {
    const stats = target.summary?.engineStats || {};
    return stats['goccia-baseline'] || stats['goccia-candidate'];
  });
}

function commentEngines(report) {
  return reportHasGocciaComparison(report) ? COMPARISON_COMMENT_ENGINES : DEFAULT_COMMENT_ENGINES;
}

function targetStatus(target, engines = DEFAULT_COMMENT_ENGINES) {
  const stats = target.summary?.engineStats || {};
  const expectedEngines = target.kind === 'awfy' ? engines : Object.keys(stats);
  const bad = [];
  for (const engine of expectedEngines) {
    const engineStats = stats[engine];
    if (!engineStats) {
      bad.push(engine);
      continue;
    }
    const ok = engineStats.ok || 0;
    const failures =
      (engineStats.timeout || 0) +
      (engineStats.crash || 0) +
      (engineStats.oom || 0) +
      (engineStats.verificationFailed || 0) +
      (engineStats.missingResult || 0);
    if (ok === 0 || failures > 0) bad.push(engine);
  }
  const agreement = target.summary?.checksumAgreement;
  const checksums = agreement?.checksums || [];
  if (bad.length > 0) return `engine issue: ${bad.join(', ')}`;
  if (agreement?.ok === false) return `verify mismatch: ${checksums.join(', ') || 'unknown'}`;
  if (checksums.length === 0) return 'no verification';
  return 'pass';
}

function engineCells(target, engines = DEFAULT_COMMENT_ENGINES) {
  const stats = target.summary?.engineStats || {};
  return engines.map((engine) => {
    const engineStats = stats[engine];
    if (!engineStats) return '-';
    if ((engineStats.ok || 0) === 0) return 'no ok sample';
    return formatMicros(engineStats.medianMicros);
  });
}

function ratioCell(geomean, numerator, denominator) {
  if (numerator === denominator) return '1.000';
  const ratio = geomean[`${numerator}_over_${denominator}`];
  if (typeof ratio === 'number') return Number(ratio).toFixed(3);
  const inverse = geomean[`${denominator}_over_${numerator}`];
  if (typeof inverse === 'number' && inverse !== 0) return (1 / Number(inverse)).toFixed(3);
  return '-';
}

function cleanEngineVersion(engine, version) {
  if (typeof version !== 'string') return '';
  let cleaned = version.trim();
  if (engine === 'qjs') {
    cleaned = cleaned.replace(/^QuickJS\s+version\s+/i, '');
    cleaned = cleaned.replace(/^QuickJS\s+/i, '');
  }
  return cleaned;
}

function engineLabels(report) {
  const labels = { ...ENGINE_LABELS };
  const metadata = report?.metadata?.engines || [];
  for (const engine of Object.keys(ENGINE_LABELS)) {
    const version = cleanEngineVersion(
      engine,
      metadata.find((entry) => entry.name === engine)?.version,
    );
    if (version) labels[engine] = `${ENGINE_LABELS[engine]} ${version}`;
  }
  return labels;
}

function finiteNumber(value) {
  return typeof value === 'number' && Number.isFinite(value);
}

function formatDeltaPercent(delta) {
  return `${delta >= 0 ? '+' : ''}${delta.toFixed(2)}%`;
}

function gocciaComparisonClassification(target) {
  const stats = target.summary?.engineStats || {};
  const baseline = stats['goccia-baseline'];
  const candidate = stats['goccia-candidate'];
  if (
    !finiteNumber(baseline?.minMicros) ||
    !finiteNumber(baseline?.maxMicros) ||
    !finiteNumber(candidate?.minMicros) ||
    !finiteNumber(candidate?.maxMicros)
  ) {
    return null;
  }

  if (candidate.maxMicros < baseline.minMicros) return 'improved';
  if (candidate.minMicros > baseline.maxMicros) return 'regressed';
  return 'unchanged';
}

function deltaCell(target) {
  const ratio = target.summary?.ratios?.[COMPARISON_PAIR];
  if (typeof ratio !== 'number' || !Number.isFinite(ratio) || ratio <= 0) return '-';
  const delta = (1 - ratio) * 100;
  const formatted = formatDeltaPercent(delta);
  const classification = gocciaComparisonClassification(target);
  if (classification === 'improved') return `🟢 ${formatted}`;
  if (classification === 'regressed') return `🔴 ${formatted}`;
  if (classification === 'unchanged') return `~ overlap (${formatted})`;
  return formatted;
}

// Whether the report has perf_event counters for both Goccia binaries.
function reportHasCounterComparison(report) {
  return (report?.targets || []).some((target) => target.summary?.counterRatios?.[COMPARISON_PAIR]);
}

// The PR's change in a counter's median against main: negative is fewer
// instructions (less work) or fewer misses.
function counterDeltaCell(target, key) {
  const ratio = target.summary?.counterRatios?.[COMPARISON_PAIR]?.[key];
  if (!finiteNumber(ratio) || ratio <= 0) return '-';
  return formatDeltaPercent((ratio - 1) * 100);
}

function counterNote(report, hasCounters) {
  const counters = report?.metadata?.counters;
  if (!counters) return '';
  if (!counters.available) return `Hardware counters were requested but not recorded: ${String(counters.reason).replace(/\.$/, '')}. `;
  if (!hasCounters) return 'Hardware counters were requested but no sample of both Goccia binaries recorded them. ';
  return 'The Instructions and L1i misses columns are the PR\'s change in the median perf_event count against main, '
    + 'so there negative is fewer (Δ PR vs main is the time saved, so there positive is faster). Instructions do not depend '
    + 'on code placement: a time Δ with no instruction Δ and a moved L1i miss count is layout, not work. ';
}

function buildGeomeanTable(geomean, labels = ENGINE_LABELS, engines = DEFAULT_COMMENT_ENGINES) {
  if (Object.keys(geomean).length === 0) return '';

  let body = '\n### Geomean Ratios\n\n';
  body += `| Ratio (row / column) | ${engines.map((engine) => labels[engine] || ENGINE_LABELS[engine] || engine).join(' | ')} |\n`;
  body += `|----------------------|${engines.map(() => '--------').join('|')}|\n`;
  for (const numerator of engines) {
    const cells = engines.map((denominator) => ratioCell(geomean, numerator, denominator));
    body += `| ${labels[numerator] || ENGINE_LABELS[numerator] || numerator} | ${cells.join(' | ')} |\n`;
  }
  return body;
}

function reportSampleCount(report) {
  const repetitions = report.metadata?.options?.repetitions;
  if (typeof repetitions === 'number') return repetitions;

  const counts = new Set();
  for (const target of report.targets || []) {
    for (const engineStats of Object.values(target.summary?.engineStats || {})) {
      if (typeof engineStats.rawCount === 'number') counts.add(engineStats.rawCount);
    }
  }
  return counts.size === 1 ? [...counts][0] : null;
}

function buildAwfyReportComment(report) {
  let body = `${MARKER}\n## AWFY Results\n\n`;
  const labels = engineLabels(report);
  const engines = commentEngines(report);
  const hasComparison = reportHasGocciaComparison(report);
  const hasCounters = hasComparison && reportHasCounterComparison(report);

  body += `| Target | Status | ${engines.map((engine) => labels[engine]).join(' | ')}`;
  if (hasComparison) body += ' | Δ PR vs main';
  if (hasCounters) body += ' | Instructions | L1i misses';
  body += ' |\n';
  body += `|--------|--------|${engines.map(() => '--------').join('|')}`;
  if (hasComparison) body += '|-------------';
  if (hasCounters) body += '|-------------|-----------';
  body += '|\n';
  for (const target of report.targets || []) {
    const cells = engineCells(target, engines);
    body += `| ${target.name} | ${targetStatus(target, engines)} | ${cells.join(' | ')}`;
    if (hasComparison) body += ` | ${deltaCell(target)}`;
    if (hasCounters) body += ` | ${counterDeltaCell(target, 'instructions')} | ${counterDeltaCell(target, 'icacheMisses')}`;
    body += ' |\n';
  }

  body += buildGeomeanTable(report.geomeanRatios || {}, labels, engines);

  const awfyCount = (report.targets || []).filter((target) => target.kind === 'awfy').length;
  const probeCount = (report.targets || []).filter((target) => target.kind === 'probe').length;
  const countParts = [plural(awfyCount, 'pinned AWFY benchmark')];
  if (probeCount > 0) countParts.push(plural(probeCount, 'diagnostic probe'));
  const sampleCount = reportSampleCount(report);
  body += `\n<sub>${countParts.join(' plus ')}. `;
  if (sampleCount !== null) body += `Medians from ${plural(sampleCount, 'interleaved sample')} per engine; `;
  body += 'raw JSON includes min/max/CV and is attached as the `awfy-report` artifact. ';
  if (hasComparison) body += 'Δ compares PR Goccia median against the same-runner main Goccia median; overlapping min/max ranges are treated as unchanged noise. ';
  if (hasComparison) body += counterNote(report, hasCounters);
  body += 'Rows below 0.5ms are timer-floor sensitive.</sub>\n';
  return body;
}

function unavailableComment(message) {
  return [
    MARKER,
    '## AWFY Results',
    '',
    `_AWFY results were not produced for this run: ${message}._`,
    '',
    '<sub>See the workflow run for the failing step.</sub>',
    '',
  ].join('\n');
}

function main(argv) {
  const [fileName = 'awfy-report.json'] = argv;
  if (!fs.existsSync(fileName)) {
    process.stdout.write(unavailableComment(`${fileName} is missing`));
    return 0;
  }

  try {
    const report = JSON.parse(fs.readFileSync(fileName, 'utf8'));
    process.stdout.write(buildAwfyReportComment(report));
    return 0;
  } catch (error) {
    process.stdout.write(unavailableComment(error.message));
    return 0;
  }
}

if (require.main === module) {
  process.exitCode = main(process.argv.slice(2));
}

module.exports = {
  buildGeomeanTable,
  buildAwfyReportComment,
  buildAwfySmokeComment: buildAwfyReportComment,
  cleanEngineVersion,
  commentEngines,
  counterDeltaCell,
  deltaCell,
  engineLabels,
  LEGACY_MARKER,
  MARKER,
  formatMicros,
  targetStatus,
  unavailableComment,
};
