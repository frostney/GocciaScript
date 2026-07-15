#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- web-tooling-results -->';

function formatRunsPerSecond(value) {
  if (typeof value !== 'number') return '-';
  if (value >= 100) return `${value.toFixed(0)} runs/s`;
  if (value >= 10) return `${value.toFixed(1)} runs/s`;
  return `${value.toFixed(2)} runs/s`;
}

function plural(count, singular) {
  return `${count} ${singular}${count === 1 ? '' : 's'}`;
}

function targetStatus(target) {
  if (target.build?.outcome !== 'ok') return target.build?.outcome || 'build failed';
  const summary = target.summary || {};
  const failureParts = [];
  if (summary.timeout > 0) failureParts.push(`timeout ${summary.timeout}`);
  if (summary.crash > 0) failureParts.push(`crash ${summary.crash}`);
  if (summary.syntaxError > 0) failureParts.push(`syntax error ${summary.syntaxError}`);
  if (summary.runtimeError > 0) failureParts.push(`runtime error ${summary.runtimeError}`);
  if (summary.oom > 0) failureParts.push(`oom ${summary.oom}`);
  if (summary.missingResult > 0) failureParts.push(`missing ${summary.missingResult}`);
  const failures =
    (summary.timeout || 0) +
    (summary.crash || 0) +
    (summary.syntaxError || 0) +
    (summary.runtimeError || 0) +
    (summary.oom || 0) +
    (summary.missingResult || 0);
  if ((summary.ok || 0) === 0 && failures === 0) return 'not run';
  if ((summary.ok || 0) === 0) return failureParts.join(', ');
  if (failures > 0) return `${summary.ok}/${summary.ok + failures} ok (${failureParts.join(', ')})`;
  return 'pass';
}

function buildWebToolingReportComment(report) {
  let body = `${MARKER}\n## Web Tooling Benchmark\n\n`;
  body += '| Workload | Status | Goccia |\n';
  body += '|----------|--------|--------|\n';

  for (const target of report.targets || []) {
    body += `| ${target.name} | ${targetStatus(target)} | ${formatRunsPerSecond(target.summary?.medianRunsPerSecond)} |\n`;
  }

  const summary = report.summary || {};
  const sampleCount = report.metadata?.options?.repetitions;
  body += `\n<sub>${plural(summary.workloadCount || 0, 'pinned Web Tooling workload')}; `;
  body += `${plural(summary.completedCount || 0, 'workload')} produced at least one Goccia sample. `;
  if (typeof sampleCount === 'number') body += `Raw results from ${plural(sampleCount, 'sample')} per workload; `;
  body += 'full stdout/stderr for failures and min/max/CV stay in the `web-tooling-report` artifact. ';
  body += 'This lane is Goccia-only by design.</sub>\n';
  return body;
}

function unavailableComment(message) {
  return [
    MARKER,
    '## Web Tooling Benchmark',
    '',
    `_Web Tooling results were not produced for this run: ${message}._`,
    '',
    '<sub>See the workflow run for the failing step.</sub>',
    '',
  ].join('\n');
}

function main(argv) {
  const [fileName = 'web-tooling-report.json'] = argv;
  if (!fs.existsSync(fileName)) {
    process.stdout.write(unavailableComment(`${fileName} is missing`));
    return 0;
  }

  try {
    const report = JSON.parse(fs.readFileSync(fileName, 'utf8'));
    process.stdout.write(buildWebToolingReportComment(report));
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
  MARKER,
  buildWebToolingReportComment,
  formatRunsPerSecond,
  targetStatus,
  unavailableComment,
};
