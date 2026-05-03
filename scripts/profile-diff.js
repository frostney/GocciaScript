#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const RELATIVE_THRESHOLD = 0.01;
const SMALL_COUNT = 1000;
const ABSOLUTE_THRESHOLD = 100;
const MAX_LINES_PER_BENCHMARK = 25;
const ARROW = '\u2192';
const RED = '\u{1F534}';
const NEW_ICON = '\u{1F195}';

function formatInteger(value) {
  return Math.round(value).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
}

function formatPercent(baseValue, currentValue) {
  if (baseValue === 0) return null;
  const percent = ((currentValue - baseValue) / baseValue) * 100;
  const abs = Math.abs(percent);
  const digits = abs >= 1000 ? 0 : 1;
  return `${percent >= 0 ? '+' : ''}${percent.toFixed(digits)}%`;
}

function formatLabel(entry) {
  if (entry.kind === 'new') return 'NEW';
  if (entry.kind === 'removed') return 'REMOVED';
  return formatChangeLabel(entry.baseValue, entry.currentValue);
}

function formatChangeLabel(baseValue, currentValue) {
  const percent = formatPercent(baseValue, currentValue);
  if (percent) return percent;
  const delta = currentValue - baseValue;
  return `${delta >= 0 ? '+' : ''}${formatInteger(delta)}`;
}

function hasSignificantDelta(baseValue, currentValue) {
  if (baseValue === currentValue) return false;
  const delta = Math.abs(currentValue - baseValue);
  if (Math.min(Math.abs(baseValue), Math.abs(currentValue)) < SMALL_COUNT) {
    return delta > ABSOLUTE_THRESHOLD;
  }
  return delta / Math.abs(baseValue) > RELATIVE_THRESHOLD;
}

function readProfile(filePath) {
  return JSON.parse(fs.readFileSync(filePath, 'utf8'));
}

function listProfileFiles(dir) {
  const files = [];
  if (!fs.existsSync(dir)) return files;

  function walk(currentDir) {
    for (const entry of fs.readdirSync(currentDir, { withFileTypes: true })) {
      const fullPath = path.join(currentDir, entry.name);
      if (entry.isDirectory()) {
        walk(fullPath);
      } else if (entry.isFile() && entry.name.endsWith('.json')) {
        files.push(path.relative(dir, fullPath).split(path.sep).join('/'));
      }
    }
  }

  walk(dir);
  files.sort();
  return files;
}

function opcodeMap(profile) {
  const map = new Map();
  for (const entry of profile.opcodes || []) {
    if (entry && entry.opcode && typeof entry.count === 'number' && entry.count > 0) {
      map.set(entry.opcode, entry.count);
    }
  }
  return map;
}

function totalOpcodes(profile) {
  return (profile.opcodes || []).reduce((sum, entry) => sum + (entry.count || 0), 0);
}

function functionLocation(entry) {
  if (entry.location) return entry.location;
  if (entry.sourceFile) {
    return entry.line ? `${entry.sourceFile}:${entry.line}` : entry.sourceFile;
  }
  return '';
}

function functionLabel(entry) {
  const rawName = entry.name || '<anonymous>';
  return `${rawName}()`;
}

function functionKey(entry) {
  return `${entry.name || '<anonymous>'}\u0000${functionLocation(entry)}`;
}

function allocationMap(profile) {
  const map = new Map();
  for (const entry of profile.functions || []) {
    const allocations = entry.allocations ?? entry.allocs ?? 0;
    if (typeof allocations !== 'number') continue;
    const key = functionKey(entry);
    map.set(key, {
      value: allocations,
      label: `${functionLabel(entry)} allocs`,
    });
  }
  return map;
}

function compareMaps(baseMap, currentMap, metric) {
  const entries = [];
  const keys = new Set([...baseMap.keys(), ...currentMap.keys()]);

  for (const key of [...keys].sort()) {
    const baseRaw = baseMap.get(key);
    const currentRaw = currentMap.get(key);
    const baseValue = typeof baseRaw === 'object' ? baseRaw.value : baseRaw;
    const currentValue = typeof currentRaw === 'object' ? currentRaw.value : currentRaw;
    const label = (typeof currentRaw === 'object' && currentRaw.label) ||
      (typeof baseRaw === 'object' && baseRaw.label) ||
      key;

    if (baseValue == null && currentValue != null) {
      if (currentValue !== 0) {
        entries.push({
          metric,
          label,
          kind: 'new',
          baseValue: 0,
          currentValue,
        });
      }
    } else if (baseValue != null && currentValue == null) {
      if (baseValue !== 0) {
        entries.push({
          metric,
          label,
          kind: 'removed',
          baseValue,
          currentValue: 0,
        });
      }
    } else if (hasSignificantDelta(baseValue, currentValue)) {
      entries.push({
        metric,
        label,
        kind: 'changed',
        baseValue,
        currentValue,
      });
    }
  }

  return entries;
}

function compareProfile(baseProfile, currentProfile) {
  const entries = [];
  entries.push(...compareMaps(opcodeMap(baseProfile), opcodeMap(currentProfile), 'opcode'));
  entries.push(...compareMaps(allocationMap(baseProfile), allocationMap(currentProfile), 'allocation'));

  const baseTotal = totalOpcodes(baseProfile);
  const currentTotal = totalOpcodes(currentProfile);
  const totalChanged = hasSignificantDelta(baseTotal, currentTotal);

  entries.sort((a, b) => {
    const kindRank = { new: 0, removed: 1, changed: 2 };
    const kindDelta = kindRank[a.kind] - kindRank[b.kind];
    if (kindDelta !== 0) return kindDelta;
    const magnitudeDelta = Math.abs(b.currentValue - b.baseValue) - Math.abs(a.currentValue - a.baseValue);
    if (magnitudeDelta !== 0) return magnitudeDelta;
    return a.label.localeCompare(b.label);
  });

  return {
    entries,
    total: totalChanged
      ? {
          label: 'Total instructions',
          baseValue: baseTotal,
          currentValue: currentTotal,
        }
      : null,
  };
}

function formatEntry(entry) {
  return `- ${RED} ${entry.label}: ${formatInteger(entry.baseValue)} ${ARROW} ${formatInteger(entry.currentValue)} (${formatLabel(entry)})`;
}

function benchmarkName(relativePath) {
  return relativePath.replace(/\.json$/, '');
}

function buildProfileDiffSection(options) {
  const baselineDir = options.baselineDir;
  const currentDir = options.currentDir;

  if (options.skip) {
    return {
      markdown: '## Deterministic profile diff\n\nDeterministic profile diff: skipped by `[skip-profile-check]`.',
      signalCount: 0,
    };
  }

  const baselineFiles = listProfileFiles(baselineDir);
  const currentFiles = listProfileFiles(currentDir);

  if (baselineFiles.length === 0) {
    return {
      markdown: '## Deterministic profile diff\n\nDeterministic profile diff: no cached baseline was restored.',
      signalCount: 0,
    };
  }

  if (currentFiles.length === 0) {
    return {
      markdown: '## Deterministic profile diff\n\nDeterministic profile diff: no current profile artifact was found.',
      signalCount: 0,
    };
  }

  const allFiles = [...new Set([...baselineFiles, ...currentFiles])].sort();
  const blocks = [];
  let signalCount = 0;

  for (const relativePath of allFiles) {
    const basePath = path.join(baselineDir, relativePath);
    const currentPath = path.join(currentDir, relativePath);
    const name = benchmarkName(relativePath);

    if (!fs.existsSync(basePath)) {
      signalCount++;
      blocks.push(`### ${name}\n- ${NEW_ICON} Profile added in current run.`);
      continue;
    }

    if (!fs.existsSync(currentPath)) {
      signalCount++;
      blocks.push(`### ${name}\n- ${RED} Profile missing from current run.`);
      continue;
    }

    const comparison = compareProfile(readProfile(basePath), readProfile(currentPath));
    if (comparison.entries.length === 0 && !comparison.total) continue;

    signalCount += comparison.entries.length + (comparison.total ? 1 : 0);
    const lines = [`### ${name}`];
    for (const entry of comparison.entries.slice(0, MAX_LINES_PER_BENCHMARK)) {
      lines.push(formatEntry(entry));
    }
    if (comparison.entries.length > MAX_LINES_PER_BENCHMARK) {
      lines.push(`- ... ${comparison.entries.length - MAX_LINES_PER_BENCHMARK} more profile changes suppressed.`);
    }
    if (comparison.total) {
      const total = comparison.total;
      lines.push(`- Total instructions: ${formatInteger(total.baseValue)} ${ARROW} ${formatInteger(total.currentValue)} (${formatChangeLabel(total.baseValue, total.currentValue)})`);
    }
    blocks.push(lines.join('\n'));
  }

  if (blocks.length === 0) {
    return {
      markdown: '## Deterministic profile diff\n\nDeterministic profile diff: no significant changes.',
      signalCount: 0,
    };
  }

  return {
    markdown: `## Deterministic profile diff\n\n${blocks.join('\n\n')}`,
    signalCount,
  };
}

function parseArgs(argv) {
  const args = {
    baselineDir: 'profile-baseline',
    currentDir: 'profile-current',
    skip: false,
  };

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === '--baseline') {
      args.baselineDir = argv[++i];
    } else if (arg.startsWith('--baseline=')) {
      args.baselineDir = arg.slice('--baseline='.length);
    } else if (arg === '--current') {
      args.currentDir = argv[++i];
    } else if (arg.startsWith('--current=')) {
      args.currentDir = arg.slice('--current='.length);
    } else if (arg === '--skip') {
      args.skip = true;
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }

  return args;
}

if (require.main === module) {
  try {
    const result = buildProfileDiffSection(parseArgs(process.argv.slice(2)));
    process.stdout.write(`${result.markdown}\n`);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = {
  buildProfileDiffSection,
  compareProfile,
};
