#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- awfy-results -->';
const LEGACY_MARKER = '<!-- awfy-smoke -->';
const COMMENT_ENGINES = ['goccia', 'qjs', 'node'];
const ENGINE_LABELS = {
  goccia: 'Goccia',
  qjs: 'QuickJS',
  node: 'Node',
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

function targetStatus(target) {
  const stats = target.summary?.engineStats || {};
  const bad = [];
  for (const [engine, engineStats] of Object.entries(stats)) {
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

function engineCells(target) {
  const stats = target.summary?.engineStats || {};
  return COMMENT_ENGINES.map((engine) => {
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

function buildGeomeanTable(geomean) {
  if (Object.keys(geomean).length === 0) return '';

  let body = '\n### Geomean Ratios\n\n';
  body += '| Ratio (row / column) | Goccia | QuickJS | Node |\n';
  body += '|----------------------|--------|---------|------|\n';
  for (const numerator of COMMENT_ENGINES) {
    const cells = COMMENT_ENGINES.map((denominator) => ratioCell(geomean, numerator, denominator));
    body += `| ${ENGINE_LABELS[numerator]} | ${cells.join(' | ')} |\n`;
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

  body += '| Target | Status | Goccia | QuickJS | Node |\n';
  body += '|--------|--------|--------|---------|------|\n';
  for (const target of report.targets || []) {
    const [goccia, qjs, node] = engineCells(target);
    body += `| ${target.name} | ${targetStatus(target)} | ${goccia} | ${qjs} | ${node} |\n`;
  }

  body += buildGeomeanTable(report.geomeanRatios || {});

  const awfyCount = (report.targets || []).filter((target) => target.kind === 'awfy').length;
  const probeCount = (report.targets || []).filter((target) => target.kind === 'probe').length;
  const countParts = [plural(awfyCount, 'pinned AWFY benchmark')];
  if (probeCount > 0) countParts.push(plural(probeCount, 'diagnostic probe'));
  const sampleCount = reportSampleCount(report);
  body += `\n<sub>${countParts.join(' plus ')}. `;
  if (sampleCount !== null) body += `Medians from ${plural(sampleCount, 'interleaved sample')} per engine; `;
  body += 'raw JSON includes min/max/CV and is attached as the `awfy-report` artifact. ';
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
  LEGACY_MARKER,
  MARKER,
  formatMicros,
  targetStatus,
  unavailableComment,
};
