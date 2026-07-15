#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- jetstream-results -->';
const ENGINE_LABELS = {
  goccia: 'Goccia',
  'goccia-baseline': 'Goccia main',
  'goccia-candidate': 'Goccia PR',
  qjs: 'QuickJS',
  node: 'Node.js',
};

function reportEngines(report) {
  const names = (report.metadata?.engines || []).map((engine) => engine.name);
  return names.length > 0 ? names : ['goccia', 'qjs', 'node'];
}

function formatScore(value) {
  return typeof value === 'number' && Number.isFinite(value) ? value.toFixed(2) : '-';
}

function formatRatio(value) {
  return typeof value === 'number' && Number.isFinite(value) ? `${value.toFixed(2)}×` : '-';
}

function targetStatus(target, engines) {
  const bad = [];
  for (const engine of engines) {
    const stats = target.summary?.engineStats?.[engine];
    const failures = stats
      ? (stats.timeout || 0) + (stats.crash || 0) + (stats.oom || 0) +
        (stats.verificationFailed || 0) + (stats.missingResult || 0)
      : 1;
    if (!stats || (stats.ok || 0) === 0 || failures > 0) bad.push(engine);
  }
  if (bad.length > 0) return `engine issue: ${bad.join(', ')}`;
  if (target.summary?.checksumAgreement?.ok === false) return 'verification mismatch';
  return 'pass';
}

function ratioFor(target, gocciaEngine, referenceEngine) {
  return target.summary?.ratios?.[`${gocciaEngine}_over_${referenceEngine}`];
}

function buildJetStreamComment(report) {
  const engines = reportEngines(report);
  const gocciaEngine = engines.includes('goccia-candidate') ? 'goccia-candidate' : 'goccia';
  let body = `${MARKER}\n## JetStream 3 Performance Barometer\n\n`;
  body += `| Workload | Status | ${engines.map((engine) => ENGINE_LABELS[engine] || engine).join(' | ')} | vs QuickJS | vs Node.js |\n`;
  body += `|---|---|${engines.map(() => '---').join('|')}|---|---|\n`;
  for (const target of report.targets || []) {
    const scores = engines.map((engine) => formatScore(target.summary?.engineStats?.[engine]?.medianScore));
    body += `| ${target.name} | ${targetStatus(target, engines)} | ${scores.join(' | ')} | `;
    body += `${formatRatio(ratioFor(target, gocciaEngine, 'qjs'))} | ${formatRatio(ratioFor(target, gocciaEngine, 'node'))} |\n`;
  }
  const geomean = report.geomeanRatios || {};
  body += `\n**Geomean reference ratio:** QuickJS ${formatRatio(geomean[`${gocciaEngine}_over_qjs`])}; `;
  body += `Node.js ${formatRatio(geomean[`${gocciaEngine}_over_node`])}.\n`;
  body += '\n<sub>1.00× means aligned; values above 1.00× mean Goccia was proportionally slower after normalizing JetStream’s higher-is-better score. This is a directional barometer across runtimes with different goals, not a product ranking. Raw samples and failure details remain in the `jetstream-report` artifact.</sub>\n';
  return body;
}

function unavailableComment(message) {
  return `${MARKER}\n## JetStream 3 Performance Barometer\n\n_Report unavailable: ${message}._\n`;
}

function main(argv) {
  const [fileName = 'jetstream-report.json'] = argv;
  if (!fs.existsSync(fileName)) {
    process.stdout.write(unavailableComment(`${fileName} is missing`));
    return 0;
  }
  try {
    process.stdout.write(buildJetStreamComment(JSON.parse(fs.readFileSync(fileName, 'utf8'))));
  } catch (error) {
    process.stdout.write(unavailableComment(error.message));
  }
  return 0;
}

if (require.main === module) process.exitCode = main(process.argv.slice(2));

module.exports = { MARKER, buildJetStreamComment, formatRatio, formatScore, targetStatus };
