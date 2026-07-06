#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const DRIVER_VERSION = 1;
const RESULT_MARKER = 'GOCCIA_AWFY_RESULT ';
const DEFAULT_MANIFEST = path.join('perf', 'awfy', 'manifest.json');
const DEFAULT_TIMEOUT_MS = 120000;
const DEFAULT_REPETITIONS = 3;
const DEFAULT_INNER_ITERATIONS = 1;
const DEFAULT_GOCCIA_FLAGS = [
  '--mode=bytecode',
  '--compat-traditional-for-loop',
  '--compat-while-loops',
  '--compat-for-in-loop',
  '--compat-function',
];

function usage() {
  return [
    'Usage: node scripts/awfy-driver.js [options]',
    '',
    'Targets:',
    '  --benchmark <AWFY name>       Run an upstream AWFY JavaScript benchmark from --awfy-dir',
    '  --probe <probe name>          Run a Goccia diagnostic probe from perf/probes',
    '  --list                       List known AWFY benchmark names and probes',
    '',
    'Inputs and engines:',
    '  --awfy-dir <path>             Path to are-we-fast-yet/benchmarks/JavaScript',
    '  --manifest <path>             Manifest path (default: perf/awfy/manifest.json)',
    '  --engines <csv>               Engine order (default: goccia,qjs,node)',
    '  --goccia <path>               GocciaScriptLoader path (default: build/GocciaScriptLoader)',
    '  --goccia-baseline <path>      Optional baseline Goccia binary, interleaved as goccia-baseline',
    '  --goccia-candidate <path>     Optional candidate Goccia binary, interleaved as goccia-candidate',
    '  --qjs <path>                  QuickJS shell path (default: qjs)',
    '  --node <path>                 Node.js path (default: current node)',
    '  --goccia-flag <flag>          Extra Goccia flag; repeatable',
    '',
    'Measurement:',
    '  --repetitions <n>             Raw samples per engine/target (default: 3)',
    '  --inner-iterations <n>        Override target inner iterations',
    '  --timeout-ms <n>              Per-sample process timeout (default: 120000)',
    '  --profile <mode>              Add Goccia --profile=opcodes|functions|all for diagnostic runs',
    '  --profile-dir <path>          Directory for Goccia profile outputs',
    '  --output <path>               Write normalized JSON report',
    '  --keep-temp                   Keep generated portable JS bundles',
  ].join('\n');
}

function parseArgs(argv) {
  const options = {
    manifestPath: DEFAULT_MANIFEST,
    awfyDir: '',
    benchmarks: [],
    probes: [],
    engines: null,
    goccia: path.join('build', 'GocciaScriptLoader'),
    gocciaBaseline: '',
    gocciaCandidate: '',
    qjs: 'qjs',
    node: process.execPath,
    gocciaFlags: [],
    repetitions: DEFAULT_REPETITIONS,
    innerIterations: null,
    timeoutMs: DEFAULT_TIMEOUT_MS,
    output: '',
    profile: '',
    profileDir: '',
    keepTemp: false,
    list: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const nextValue = () => {
      if (arg.includes('=')) return arg.slice(arg.indexOf('=') + 1);
      i += 1;
      if (i >= argv.length) throw new Error(`Missing value for ${arg}`);
      return argv[i];
    };

    if (arg === '--help' || arg === '-h') {
      options.help = true;
    } else if (arg === '--list') {
      options.list = true;
    } else if (arg === '--keep-temp') {
      options.keepTemp = true;
    } else if (arg === '--manifest' || arg.startsWith('--manifest=')) {
      options.manifestPath = nextValue();
    } else if (arg === '--awfy-dir' || arg.startsWith('--awfy-dir=')) {
      options.awfyDir = nextValue();
    } else if (arg === '--benchmark' || arg.startsWith('--benchmark=')) {
      options.benchmarks.push(nextValue());
    } else if (arg === '--probe' || arg.startsWith('--probe=')) {
      options.probes.push(nextValue());
    } else if (arg === '--engines' || arg.startsWith('--engines=')) {
      options.engines = nextValue().split(',').map((engine) => engine.trim()).filter(Boolean);
    } else if (arg === '--goccia' || arg.startsWith('--goccia=')) {
      options.goccia = nextValue();
    } else if (arg === '--goccia-baseline' || arg.startsWith('--goccia-baseline=')) {
      options.gocciaBaseline = nextValue();
    } else if (arg === '--goccia-candidate' || arg.startsWith('--goccia-candidate=')) {
      options.gocciaCandidate = nextValue();
    } else if (arg === '--qjs' || arg.startsWith('--qjs=')) {
      options.qjs = nextValue();
    } else if (arg === '--node' || arg.startsWith('--node=')) {
      options.node = nextValue();
    } else if (arg === '--goccia-flag' || arg.startsWith('--goccia-flag=')) {
      options.gocciaFlags.push(nextValue());
    } else if (arg === '--repetitions' || arg.startsWith('--repetitions=')) {
      options.repetitions = parsePositiveInteger(nextValue(), '--repetitions');
    } else if (arg === '--inner-iterations' || arg.startsWith('--inner-iterations=')) {
      options.innerIterations = parsePositiveInteger(nextValue(), '--inner-iterations');
    } else if (arg === '--timeout-ms' || arg.startsWith('--timeout-ms=')) {
      options.timeoutMs = parsePositiveInteger(nextValue(), '--timeout-ms');
    } else if (arg === '--profile' || arg.startsWith('--profile=')) {
      options.profile = nextValue();
    } else if (arg === '--profile-dir' || arg.startsWith('--profile-dir=')) {
      options.profileDir = nextValue();
    } else if (arg === '--output' || arg.startsWith('--output=')) {
      options.output = nextValue();
    } else {
      throw new Error(`Unknown option: ${arg}`);
    }
  }

  return options;
}

function parsePositiveInteger(value, optionName) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed) || parsed < 1) {
    throw new Error(`${optionName} must be a positive integer, got ${value}`);
  }
  return parsed;
}

function parseTargetSpec(value, optionName) {
  const marker = value.lastIndexOf(':');
  if (marker <= 0) return { name: value, innerIterations: null };
  const suffix = value.slice(marker + 1);
  if (!/^[0-9]+$/.test(suffix)) return { name: value, innerIterations: null };
  return {
    name: value.slice(0, marker),
    innerIterations: parsePositiveInteger(suffix, optionName),
  };
}

function readJSON(fileName) {
  return JSON.parse(fs.readFileSync(fileName, 'utf8'));
}

function normalizeModuleId(request) {
  let normalized = request;
  if (!normalized.startsWith('./')) normalized = `./${normalized}`;
  normalized = path.posix.normalize(normalized);
  if (!normalized.startsWith('./')) normalized = `./${normalized}`;
  if (normalized.endsWith('.js')) normalized = normalized.slice(0, -3);
  return normalized;
}

function collectAwfyModules(awfyDir, entryId) {
  const modules = new Map();
  const visit = (id) => {
    const normalized = normalizeModuleId(id);
    if (modules.has(normalized)) return;
    const fileName = path.join(awfyDir, `${normalized.slice(2)}.js`);
    const source = fs.readFileSync(fileName, 'utf8');
    modules.set(normalized, source);

    const requirePattern = /\brequire\(['"](\.\/[^'"]+)['"]\)/g;
    let match = requirePattern.exec(source);
    while (match) {
      visit(match[1]);
      match = requirePattern.exec(source);
    }
  };
  visit(entryId);
  return modules;
}

function buildAwfyBundle({ awfyDir, benchmarkName, moduleName, innerIterations }) {
  const entryId = normalizeModuleId(moduleName);
  const modules = collectAwfyModules(awfyDir, entryId);
  const chunks = [
    '"use strict";',
    `const __awfyConfig = ${JSON.stringify({ benchmarkName, innerIterations })};`,
    'const __awfyNowMicros = () => (typeof performance !== "undefined" && performance.now ? performance.now() * 1000 : Date.now() * 1000);',
    'const process = { stdout: { write: (value) => { const text = String(value); if (text.length > 0) { const lines = text.split("\\n"); for (let i = 0; i < lines.length; i += 1) { if (lines[i].length > 0) console.log(lines[i]); } } } } };',
    'const __modules = Object.create(null);',
    'const __cache = Object.create(null);',
    'const __define = (id, factory) => { __modules[id] = factory; };',
    'const __awfyRequire = (id) => { const normalized = id.endsWith(".js") ? id.slice(0, -3) : id; const key = normalized.startsWith("./") ? normalized : "./" + normalized; if (__cache[key]) return __cache[key].exports; if (!__modules[key]) throw new Error("Missing AWFY module " + key); const module = { exports: {} }; __cache[key] = module; __modules[key](module, module.exports, __awfyRequire); return module.exports; };',
  ];

  for (const [id, source] of modules) {
    chunks.push(`__define(${JSON.stringify(id)}, (module, exports, require) => {\n${source}\n});`);
  }

  chunks.push([
    `const __suite = __awfyRequire(${JSON.stringify(entryId)});`,
    'const __bench = __suite.newInstance();',
    'const __started = __awfyNowMicros();',
    'const __verificationPassed = __bench.innerBenchmarkLoop(__awfyConfig.innerIterations) === true;',
    'const __durationMicros = __awfyNowMicros() - __started;',
    'const __result = { kind: "awfy", name: __awfyConfig.benchmarkName, durationMicros: __durationMicros, checksum: __verificationPassed ? "verified" : "failed", verificationPassed: __verificationPassed, innerIterations: __awfyConfig.innerIterations };',
    `console.log(${JSON.stringify(RESULT_MARKER)} + JSON.stringify(__result));`,
    'if (!__verificationPassed) throw new Error("AWFY verification failed for " + __awfyConfig.benchmarkName);',
  ].join('\n'));

  return `${chunks.join('\n\n')}\n`;
}

function buildProbeBundle({ probe, innerIterations }) {
  const source = fs.readFileSync(probe.file, 'utf8');
  return [
    '"use strict";',
    `const __probeConfig = ${JSON.stringify({ name: probe.name, innerIterations })};`,
    'const __probeNowMicros = () => (typeof performance !== "undefined" && performance.now ? performance.now() * 1000 : Date.now() * 1000);',
    'let __probe = null;',
    'const __gocciaRegisterProbe = (probe) => { __probe = probe; };',
    source,
    'if (__probe === null) throw new Error("Probe did not register itself: " + __probeConfig.name);',
    'const __started = __probeNowMicros();',
    'const __checksum = __probe.run(__probeConfig.innerIterations);',
    'const __durationMicros = __probeNowMicros() - __started;',
    'const __verificationPassed = __probe.verify ? (__probe.verify(__checksum, __probeConfig.innerIterations) === true) : true;',
    'const __result = { kind: "probe", name: __probeConfig.name, durationMicros: __durationMicros, checksum: String(__checksum), verificationPassed: __verificationPassed, innerIterations: __probeConfig.innerIterations };',
    `console.log(${JSON.stringify(RESULT_MARKER)} + JSON.stringify(__result));`,
    'if (!__verificationPassed) throw new Error("Probe verification failed for " + __probeConfig.name);',
    '',
  ].join('\n');
}

function writeBundle(tempDir, target, source) {
  const fileName = path.join(tempDir, `${target.kind}-${target.name}.js`);
  fs.writeFileSync(fileName, source);
  return fileName;
}

function resolveTargets(options, manifest) {
  const targets = [];
  const benchmarkMap = manifest.awfy?.benchmarks || {};
  const probes = new Map((manifest.diagnosticProbes || []).map((probe) => [probe.name, probe]));

  for (const benchmarkSpec of options.benchmarks) {
    const parsed = parseTargetSpec(benchmarkSpec, '--benchmark');
    const benchmarkName = parsed.name;
    const moduleName = benchmarkMap[benchmarkName];
    if (!moduleName) throw new Error(`Unknown AWFY benchmark: ${benchmarkName}`);
    if (!options.awfyDir) throw new Error('--awfy-dir is required when --benchmark is used');
    targets.push({
      kind: 'awfy',
      name: benchmarkName,
      moduleName,
      innerIterations: options.innerIterations || parsed.innerIterations || DEFAULT_INNER_ITERATIONS,
      focusAreas: ['awfy'],
      tags: ['awfy'],
      source: `${options.awfyDir}/${moduleName}.js`,
    });
  }

  for (const probeSpec of options.probes) {
    const parsed = parseTargetSpec(probeSpec, '--probe');
    const probeName = parsed.name;
    const probe = probes.get(probeName);
    if (!probe) throw new Error(`Unknown diagnostic probe: ${probeName}`);
    targets.push({
      kind: 'probe',
      name: probe.name,
      probe,
      innerIterations: options.innerIterations || parsed.innerIterations || probe.defaultInnerIterations || DEFAULT_INNER_ITERATIONS,
      focusAreas: probe.focusAreas || [],
      tags: probe.tags || [],
      source: probe.file,
      engines: probe.engines || null,
      gocciaFlags: probe.gocciaFlags || [],
    });
  }

  return targets;
}

function resolveEngines(options) {
  const requested = options.engines || ['goccia', 'qjs', 'node'];
  const engines = [];

  const addGoccia = (name, command) => {
    engines.push({
      name,
      kind: 'goccia',
      command,
      baseArgs: DEFAULT_GOCCIA_FLAGS.concat(options.gocciaFlags),
    });
  };

  for (const name of requested) {
    if (name === 'goccia') {
      if (options.gocciaBaseline || options.gocciaCandidate) {
        if (options.gocciaBaseline) addGoccia('goccia-baseline', options.gocciaBaseline);
        if (options.gocciaCandidate) addGoccia('goccia-candidate', options.gocciaCandidate);
      } else {
        addGoccia('goccia', options.goccia);
      }
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

function runEngineSample({ engine, target, fileName, repetition, options }) {
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
  const parsed = parseResult(stdout);
  const outcome = classifyProcessOutcome(proc, parsed, stdout, stderr);

  return {
    engine: engine.name,
    repetition,
    startedAt,
    outcome,
    durationMicros: parsed?.durationMicros ?? null,
    checksum: parsed?.checksum ?? null,
    verificationPassed: parsed?.verificationPassed ?? false,
    innerIterations: target.innerIterations,
    exitCode: proc.status,
    signal: proc.signal || null,
    command: [engine.command, ...args],
    stderr: outcome === 'ok' ? '' : stderr.slice(0, 4000),
  };
}

function parseResult(stdout) {
  const lines = stdout.split(/\r?\n/);
  for (let i = lines.length - 1; i >= 0; i -= 1) {
    const line = lines[i];
    if (line.startsWith(RESULT_MARKER)) {
      return JSON.parse(line.slice(RESULT_MARKER.length));
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

function median(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
}

function iqrFiltered(values) {
  if (values.length < 4) return [...values];
  const sorted = [...values].sort((a, b) => a - b);
  const q1 = sorted[Math.floor(sorted.length / 4)];
  const q3 = sorted[Math.floor((sorted.length * 3) / 4)];
  const iqr = q3 - q1;
  if (iqr <= 0) return sorted;
  const lower = q1 - 1.5 * iqr;
  const upper = q3 + 1.5 * iqr;
  return sorted.filter((value) => value >= lower && value <= upper);
}

function coefficientOfVariation(values) {
  if (values.length === 0) return null;
  const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
  if (mean === 0) return null;
  const variance = values.reduce((sum, value) => sum + ((value - mean) * (value - mean)), 0) / values.length;
  return (Math.sqrt(variance) / mean) * 100;
}

function summarizeSamples(samples) {
  const values = samples
    .filter((sample) => sample.outcome === 'ok' && typeof sample.durationMicros === 'number' && sample.durationMicros >= 0)
    .map((sample) => sample.durationMicros);
  const filtered = iqrFiltered(values);
  return {
    ok: samples.filter((sample) => sample.outcome === 'ok').length,
    timeout: samples.filter((sample) => sample.outcome === 'timeout').length,
    crash: samples.filter((sample) => sample.outcome === 'crash').length,
    oom: samples.filter((sample) => sample.outcome === 'oom').length,
    verificationFailed: samples.filter((sample) => sample.outcome === 'verification-failed').length,
    missingResult: samples.filter((sample) => sample.outcome === 'missing-result').length,
    rawCount: values.length,
    medianMicros: median(values),
    iqrMedianMicros: median(filtered),
    minMicros: values.length ? Math.min(...values) : null,
    maxMicros: values.length ? Math.max(...values) : null,
    coefficientOfVariation: coefficientOfVariation(values),
  };
}

function checksumAgreement(samples) {
  const checksums = new Set(samples
    .filter((sample) => sample.outcome === 'ok')
    .map((sample) => sample.checksum));
  return {
    ok: checksums.size <= 1,
    checksums: [...checksums].sort(),
  };
}

function buildTargetSummary(samples) {
  const byEngine = new Map();
  for (const sample of samples) {
    if (!byEngine.has(sample.engine)) byEngine.set(sample.engine, []);
    byEngine.get(sample.engine).push(sample);
  }

  const engineStats = {};
  for (const [engine, engineSamples] of byEngine.entries()) {
    engineStats[engine] = summarizeSamples(engineSamples);
  }

  const ratios = {};
  const engines = Object.keys(engineStats);
  for (const left of engines) {
    for (const right of engines) {
      if (left === right) continue;
      const leftMedian = engineStats[left].medianMicros;
      const rightMedian = engineStats[right].medianMicros;
      if (leftMedian && rightMedian) {
        ratios[`${left}_over_${right}`] = leftMedian / rightMedian;
      }
    }
  }

  return {
    checksumAgreement: checksumAgreement(samples),
    engineStats,
    ratios,
  };
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

function collectMetadata(options, manifest, engines) {
  return {
    driver: {
      version: DRIVER_VERSION,
      script: 'scripts/awfy-driver.js',
    },
    goccia: {
      commit: commandText('git', ['rev-parse', 'HEAD']),
      shortCommit: commandText('git', ['rev-parse', '--short', 'HEAD']),
      status: commandText('git', ['status', '--short']),
      buildMode: 'external-binary',
      fpcVersion: commandText('fpc', ['-iV']),
    },
    platform: {
      os: os.platform(),
      release: os.release(),
      arch: os.arch(),
      cpus: os.cpus().length,
    },
    corpus: {
      awfy: manifest.awfy || null,
    },
    engines: engines.map((engine) => ({
      name: engine.name,
      kind: engine.kind,
      command: engine.command,
      version: engineVersion(engine),
    })),
    options: {
      repetitions: options.repetitions,
      timeoutMs: options.timeoutMs,
      profile: options.profile || null,
      profileDir: options.profileDir || null,
    },
  };
}

function commandText(command, args) {
  const proc = spawnSync(command, args, { encoding: 'utf8' });
  if (proc.status !== 0) return '';
  return (proc.stdout || '').trim();
}

function engineVersion(engine) {
  if (engine.kind === 'node') return commandText(engine.command, ['--version']);
  if (engine.kind === 'qjs') {
    const proc = spawnSync(engine.command, ['-h'], { encoding: 'utf8' });
    const output = `${proc.stdout || ''}${proc.stderr || ''}`.split(/\r?\n/)[0];
    return output.trim();
  }
  const proc = spawnSync(engine.command, ['--version'], { encoding: 'utf8' });
  if (proc.status === 0) return (proc.stdout || proc.stderr || '').trim();
  return '';
}

function listTargets(manifest) {
  const benchmarks = Object.keys(manifest.awfy?.benchmarks || {}).sort();
  const probes = (manifest.diagnosticProbes || []).map((probe) => probe.name).sort();
  console.log('AWFY benchmarks:');
  for (const benchmark of benchmarks) console.log(`  ${benchmark}`);
  console.log('\nDiagnostic probes:');
  for (const probe of probes) console.log(`  ${probe}`);
}

function main(argv = process.argv.slice(2)) {
  const options = parseArgs(argv);
  if (options.help) {
    console.log(usage());
    return 0;
  }

  const manifest = readJSON(options.manifestPath);
  if (options.list) {
    listTargets(manifest);
    return 0;
  }

  const targets = resolveTargets(options, manifest);
  if (targets.length === 0) {
    throw new Error('No targets selected. Use --benchmark, --probe, or --list.');
  }

  const engines = resolveEngines(options);
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'goccia-awfy-'));
  const targetReports = [];

  try {
    for (const target of targets) {
      const bundle = target.kind === 'awfy'
        ? buildAwfyBundle({
          awfyDir: options.awfyDir,
          benchmarkName: target.name,
          moduleName: target.moduleName,
          innerIterations: target.innerIterations,
        })
        : buildProbeBundle({
          probe: target.probe,
          innerIterations: target.innerIterations,
        });
      const bundleFile = writeBundle(tempDir, target, bundle);
      const samples = [];

      for (let repetition = 0; repetition < options.repetitions; repetition += 1) {
        for (const engine of engines) {
          if (!engineAllowedForTarget(engine, target)) continue;
          const sample = runEngineSample({
            engine,
            target,
            fileName: bundleFile,
            repetition,
            options,
          });
          samples.push(sample);
          console.error(`${target.name} ${engine.name} rep ${repetition + 1}/${options.repetitions}: ${sample.outcome}${sample.durationMicros ? ` ${sample.durationMicros}us` : ''}`);
        }
      }

      targetReports.push({
        kind: target.kind,
        name: target.name,
        source: target.source,
        generatedBundle: options.keepTemp ? bundleFile : null,
        focusAreas: target.focusAreas,
        tags: target.tags,
        innerIterations: target.innerIterations,
        samples,
        summary: buildTargetSummary(samples),
      });
    }
  } finally {
    if (!options.keepTemp) fs.rmSync(tempDir, { recursive: true, force: true });
  }

  const report = {
    schemaVersion: 1,
    generatedAt: new Date().toISOString(),
    metadata: collectMetadata(options, manifest, engines),
    targets: targetReports,
    geomeanRatios: buildGeomeanRatios(targetReports),
  };

  const json = `${JSON.stringify(report, null, 2)}\n`;
  if (options.output) {
    const outputDir = path.dirname(options.output);
    if (outputDir && outputDir !== '.') fs.mkdirSync(outputDir, { recursive: true });
    fs.writeFileSync(options.output, json);
  } else {
    process.stdout.write(json);
  }

  return 0;
}

if (require.main === module) {
  try {
    process.exitCode = main();
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = {
  RESULT_MARKER,
  buildAwfyBundle,
  buildGeomeanRatios,
  buildProbeBundle,
  classifyProcessOutcome,
  checksumAgreement,
  collectAwfyModules,
  coefficientOfVariation,
  iqrFiltered,
  median,
  parseArgs,
  parseResult,
  summarizeSamples,
};
