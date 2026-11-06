#!/usr/bin/env node

const fs = require('fs');

const MARKER = '<!-- jetstream-results -->';
const ENGINE_LABELS = {
  goccia: 'Goccia',
  'goccia-baseline': 'Goccia main',
  'goccia-candidate': 'Goccia PR',
  qjs: 'QuickJS',
  'node-jitless': 'Node.js --jitless',
  jsc: 'JSC (no JIT)',
  node: 'Node.js',
};
// The comparators Goccia gets a ratio against, each in a column of its own:
// QuickJS for the first performance target, the interpreter-class context
// engines, then JIT-enabled Node.js as the outside reference (ADR 0178).
const COMPARATOR_ENGINES = ['qjs', 'node-jitless', 'jsc', 'node'];

function reportEngines(report) {
  const names = (report.metadata?.engines || []).map((engine) => engine.name);
  return names.length > 0 ? names : ['goccia', 'qjs', 'node'];
}

function comparatorEngines(engines) {
  return COMPARATOR_ENGINES.filter((engine) => engines.includes(engine));
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
  const comparators = comparatorEngines(engines);
  const label = (engine) => ENGINE_LABELS[engine] || engine;
  let body = `${MARKER}\n## JetStream 3 Performance Barometer\n\n`;
  body += `| Workload | Status | ${engines.map(label).join(' | ')} | ${comparators.map((engine) => `vs ${label(engine)}`).join(' | ')} |\n`;
  body += `|---|---|${engines.map(() => '---').join('|')}|${comparators.map(() => '---').join('|')}|\n`;
  for (const target of report.targets || []) {
    const scores = engines.map((engine) => formatScore(target.summary?.engineStats?.[engine]?.medianScore));
    const ratios = comparators.map((engine) => formatRatio(ratioFor(target, gocciaEngine, engine)));
    body += `| ${target.name} | ${targetStatus(target, engines)} | ${scores.join(' | ')} | ${ratios.join(' | ')} |\n`;
  }
  const geomean = report.geomeanRatios || {};
  body += `\n**Geomean reference ratio:** ${comparators
    .map((engine) => `${label(engine)} ${formatRatio(geomean[`${gocciaEngine}_over_${engine}`])}`)
    .join('; ')}.\n`;
  body += '\n<sub>1.00× means aligned; values above 1.00× mean Goccia was proportionally slower after normalizing JetStream’s higher-is-better score. QuickJS is the bar for the interpreter-speed target; Node.js --jitless and JSC with its JIT off are interpreter-class context that the target does not count, and Node.js, with its JIT, is shown for scale. Engines alternate their order from one repetition to the next. Raw samples, each with its place in that order, and failure details remain in the `jetstream-report` artifact.</sub>\n';
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

module.exports = { COMPARATOR_ENGINES, MARKER, buildJetStreamComment, comparatorEngines, formatRatio, formatScore, targetStatus };
