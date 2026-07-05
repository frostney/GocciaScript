#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- awfy-smoke -->';

function formatMicros(value) {
  if (typeof value !== 'number') return '-';
  if (value >= 1000000) return `${(value / 1000000).toFixed(2)}s`;
  if (value >= 1000) return `${(value / 1000).toFixed(2)}ms`;
  return `${value.toFixed(2)}us`;
}

function targetOutcome(target) {
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
  if (target.summary?.checksumAgreement?.ok === false) bad.push('checksum');
  return bad.length === 0 ? 'ok' : `check ${bad.join(', ')}`;
}

function engineCells(target) {
  const stats = target.summary?.engineStats || {};
  return ['goccia', 'qjs', 'node'].map((engine) => {
    const engineStats = stats[engine];
    if (!engineStats) return '-';
    if ((engineStats.ok || 0) === 0) return 'no ok sample';
    return formatMicros(engineStats.medianMicros);
  });
}

function buildAwfySmokeComment(report) {
  const awfy = report.metadata?.corpus?.awfy || {};
  const shortSha = typeof awfy.commit === 'string' ? awfy.commit.slice(0, 8) : 'unknown';
  const repo = awfy.repository || 'https://github.com/smarr/are-we-fast-yet';
  const commitUrl = awfy.commit ? `${repo}/commit/${awfy.commit}` : repo;

  let body = `${MARKER}\n## AWFY Smoke\n\n`;
  body += `Pinned corpus: [\`${shortSha}\`](${commitUrl})  \n`;
  body += `Driver version: ${report.metadata?.driver?.version ?? 'unknown'}  \n`;
  body += `Generated: ${report.generatedAt ?? 'unknown'}\n\n`;

  body += '| Target | Kind | Outcome | Checksum | Goccia | QuickJS | Node |\n';
  body += '|--------|------|---------|----------|--------|---------|------|\n';
  for (const target of report.targets || []) {
    const [goccia, qjs, node] = engineCells(target);
    const checksums = target.summary?.checksumAgreement?.checksums || [];
    const checksum = target.summary?.checksumAgreement?.ok === false
      ? `mismatch: ${checksums.join(', ')}`
      : (checksums.join(', ') || '-');
    body += `| ${target.name} | ${target.kind} | ${targetOutcome(target)} | ${checksum} | ${goccia} | ${qjs} | ${node} |\n`;
  }

  const geomean = report.geomeanRatios || {};
  if (Object.keys(geomean).length > 0) {
    body += '\n### Geomean Ratios\n\n';
    body += '| Pair | Ratio |\n|------|-------|\n';
    for (const [pair, ratio] of Object.entries(geomean).sort()) {
      body += `| ${pair.replace(/_/g, ' ')} | ${Number(ratio).toFixed(3)} |\n`;
    }
  }

  body += '\n<sub>Smoke only: one pinned AWFY benchmark plus one diagnostic probe. ';
  body += 'Full-corpus AWFY timing remains a handover/manual or scheduled lane, not a PR gate.</sub>\n';
  return body;
}

function unavailableComment(message) {
  return [
    MARKER,
    '## AWFY Smoke',
    '',
    `_AWFY smoke results were not produced for this run: ${message}._`,
    '',
    '<sub>Smoke only. See the workflow run for the failing step.</sub>',
    '',
  ].join('\n');
}

function main(argv) {
  const [fileName = 'awfy-smoke.json'] = argv;
  if (!fs.existsSync(fileName)) {
    process.stdout.write(unavailableComment(`${fileName} is missing`));
    return 0;
  }

  try {
    const report = JSON.parse(fs.readFileSync(fileName, 'utf8'));
    process.stdout.write(buildAwfySmokeComment(report));
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
  buildAwfySmokeComment,
  formatMicros,
  targetOutcome,
  unavailableComment,
};
