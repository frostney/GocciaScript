// The benchmark API (goccia:microbench) for G2: bench, group, run, summary and boxplot, as GocciaBenchmarkRunner
// gives them (Goccia.Builtins.Benchmark). The warm-up, calibration, measurement rounds, outlier filter and
// percentiles are the same steps in the same order, so a file reports comparable numbers on both engines.
// G2.HostAPI calls this function with the global object, the preludes' shared internals and the natives (n): now()
// (a monotonic clock in nanoseconds), collect() (a full collection), forget() (the rejections the bodies left, no
// longer tracked), discardJobs(), progress(suite, name, index, total), entry(...) (one result for the host), the
// settings (warmup, calibrationMs, calibrationBatch, rounds) and progressOn. It keeps the object this returns, whose
// report(deterministic, done) measures the registered benchmarks once, or reuses the run the script made itself with
// run(), and calls done(total, durationNanoseconds) or done(undefined, undefined, error).
(function (global, shared, n) {
  'use strict';
  // the intrinsics as the realm made them: a benchmark file can replace or pollute the globals it measures
  const { Object, TypeError, Float64Array, Promise, String } = global;
  const defineProperty = Object.defineProperty;
  const trunc = global.Math.trunc, sqrt = global.Math.sqrt;
  const isObject = (v) => v !== null && (typeof v === 'object' || typeof v === 'function');
  const now = n.now, collect = n.collect, forget = n.forget, discardJobs = n.discardJobs, entry = n.entry;

  const WARMUP_ITERATIONS = n.warmup;
  const MIN_CALIBRATION_MS = n.calibrationMs;
  const CALIBRATION_BATCH = n.calibrationBatch;
  const MEASUREMENT_ROUNDS = n.rounds;
  const MAX_PERCENTILE_SAMPLES = 10000;
  const IQR_MULTIPLIER = 1.5;

  // an element added as CreateDataPropertyOrThrow adds it: an index setter on Array.prototype is never called
  const append = (list, value) => {
    defineProperty(list, list.length, { value, writable: true, enumerable: true, configurable: true });
  };

  const cases = [];
  let currentSuite = '';
  let summaryScope = 0, boxplotScope = 0, nextSummaryScope = 0, nextBoxplotScope = 0;
  // the last completed run and whether it was the deterministic one: the host's report reuses a run of the same
  // kind the script already made, so nothing is measured twice
  let lastRun = null, lastDeterministic = false;
  // each failed result's thrown value, which the host describes as it describes an uncaught one
  const thrown = new global.WeakMap();

  // a result that is a promise or another thenable is awaited (AwaitBenchmarkResult)
  const isThenable = (v) => isObject(v) && typeof v.then === 'function';

  // count calls of fn: undefined when every result was synchronous, else a promise that finishes the batch from the
  // first thenable on, awaiting each one
  const batch = (fn, count) => {
    for (let i = 0; i < count; i++) {
      const r = fn();
      if (isThenable(r)) return finishBatch(fn, count, i + 1, r);
    }
    return undefined;
  };
  async function finishBatch(fn, count, i, pending) {
    await pending;
    for (; i < count; i++) {
      const r = fn();
      if (isThenable(r)) await r;
    }
  }

  function requireNameAndCallback(args, kind) {
    if (args.length < 2 || typeof args[0] !== 'string') throw new TypeError(`${kind} requires a name string and callback`);
    if (typeof args[1] !== 'function') throw new TypeError(`${kind} requires a callback function`);
  }

  function group(...args) {
    requireNameAndCallback(args, 'group');
    const previous = currentSuite;
    currentSuite = args[0];
    try {
      args[1]();
    } finally {
      currentSuite = previous;
    }
  }

  // a generator function's benchmark: the code before its first yield is the setup, the function it yields the
  // body, and the code after it the teardown
  function isGeneratorFunction(fn) {
    const ctor = fn.constructor;
    return isObject(ctor) && ctor.name === 'GeneratorFunction';
  }

  function bench(...args) {
    requireNameAndCallback(args, 'bench');
    const fn = args[1];
    const generator = isGeneratorFunction(fn);
    append(cases, {
      name: args[0], suite: currentSuite, summaryScope, boxplotScope,
      run: generator ? null : fn, generator: generator ? fn : null,
    });
  }

  // summary(fn) and boxplot(fn): the benchmarks fn registers share a new scope of the kind, also those it
  // registers after an await (its promise settled before the scope ends)
  function scoped(args, kind, enter, leave) {
    if (args.length < 1 || typeof args[0] !== 'function') throw new TypeError('benchmark wrapper requires a callback function');
    const previous = enter();
    let result;
    try {
      result = args[0]();
    } catch (e) {
      leave(previous);
      throw e;
    }
    if (!isThenable(result)) {
      leave(previous);
      return undefined;
    }
    return Promise.resolve(result).then(() => { leave(previous); }, (e) => { leave(previous); throw e; });
  }
  function summary(...args) {
    return scoped(args, 'summary',
      () => { const p = summaryScope; summaryScope = ++nextSummaryScope; return p; },
      (p) => { summaryScope = p; });
  }
  function boxplot(...args) {
    return scoped(args, 'boxplot',
      () => { const p = boxplotScope; boxplotScope = ++nextBoxplotScope; return p; },
      (p) => { boxplotScope = p; });
  }

  // the generator's iterator and the function its first yield gave
  function startGenerator(c) {
    const it = c.generator();
    if (!isObject(it) || typeof it.next !== 'function') throw new TypeError('bench generator callback must return a sync iterator');
    let step;
    try {
      step = it.next();
      forget();
      if (!isObject(step) || step.done || typeof step.value !== 'function')
        throw new TypeError('bench generator callback must yield a function');
    } catch (e) {
      closeKeepingError(it);
      throw e;
    }
    return { it, fn: step.value };
  }
  // the teardown: the generator returns, and what its finally throws is the benchmark's error
  function close(it) {
    const ret = it.return;
    if (ret !== undefined && ret !== null) ret.call(it);
  }
  // the teardown after the body failed: the body's error is the one reported
  function closeKeepingError(it) {
    try { close(it); } catch {}
  }

  async function calibrate(fn) {
    const target = MIN_CALIBRATION_MS * 1000000;
    let size = CALIBRATION_BATCH;
    for (;;) {
      const start = now();
      const p = batch(fn, size);
      if (p !== undefined) await p;
      forget();
      const elapsed = now() - start;
      if (elapsed >= target) {
        const iterations = trunc(size / 10) * 10;
        return iterations < 10 ? 10 : iterations;
      }
      size = elapsed === 0 ? size * 10 : size * (trunc(target / elapsed) + 1);
      if (size > 10000000) return trunc(size / 10) * 10;
    }
  }

  function insertionSort(a, count) {
    for (let k = 1; k < count; k++) {
      const v = a[k];
      let j = k - 1;
      while (j >= 0 && a[j] > v) { a[j + 1] = a[j]; j--; }
      a[j + 1] = v;
    }
  }

  function result(c) {
    return {
      name: c.name, suite: c.suite, opsPerSec: 0, meanMs: 0, iterations: 0, totalMs: 0, variancePercentage: 0,
      setupMs: 0, teardownMs: 0, minOpsPerSec: 0, maxOpsPerSec: 0, sampleCount: 0, minSampleMs: 0, p25Ms: 0,
      medianMs: 0, p75Ms: 0, p99Ms: 0, p999Ms: 0, maxSampleMs: 0, summaryScope: c.summaryScope,
      boxplotScope: c.boxplotScope,
    };
  }

  async function measure(c) {
    const r = result(c);
    let fn = c.run, it = null;
    try {
      if (c.generator !== null) {
        const start = now();
        ({ it, fn } = startGenerator(c));
        r.setupMs = (now() - start) / 1000000;
      }
      collect();
      let p = batch(fn, WARMUP_ITERATIONS);
      if (p !== undefined) await p;
      forget();
      const iterations = await calibrate(fn);
      collect();
      const ops = new Float64Array(MEASUREMENT_ROUNDS), means = new Float64Array(MEASUREMENT_ROUNDS);
      for (let round = 0; round < MEASUREMENT_ROUNDS; round++) {
        const start = now();
        p = batch(fn, iterations);
        if (p !== undefined) await p;
        forget();
        const elapsed = now() - start;
        if (elapsed > 0) {
          ops[round] = (iterations / elapsed) * 1000000000;
          means[round] = (elapsed / 1000000) / iterations;
        }
      }
      insertionSort(ops, MEASUREMENT_ROUNDS);
      insertionSort(means, MEASUREMENT_ROUNDS);
      // the rounds outside 1.5 interquartile ranges of the quartiles are left out (FilterOutliersIQR)
      let first = 0, last = MEASUREMENT_ROUNDS - 1;
      if (MEASUREMENT_ROUNDS >= 4) {
        const q1 = ops[trunc(MEASUREMENT_ROUNDS / 4)], q3 = ops[trunc((MEASUREMENT_ROUNDS * 3) / 4)];
        const iqr = q3 - q1;
        if (iqr > 0) {
          const lower = q1 - IQR_MULTIPLIER * iqr, upper = q3 + IQR_MULTIPLIER * iqr;
          for (let k = 0; k < MEASUREMENT_ROUNDS; k++) if (ops[k] >= lower) { first = k; break; }
          for (let k = MEASUREMENT_ROUNDS - 1; k >= 0; k--) if (ops[k] <= upper) { last = k; break; }
        }
      }
      const kept = last - first + 1;
      let mean = 0;
      for (let k = first; k <= last; k++) mean += ops[k];
      mean /= kept;
      let variance = 0;
      for (let k = first; k <= last; k++) variance += (ops[k] - mean) * (ops[k] - mean);
      variance /= kept;
      r.variancePercentage = mean > 0 ? (sqrt(variance) / mean) * 100 : 0;
      r.iterations = iterations;
      r.opsPerSec = ops[first + trunc(kept / 2)];
      r.meanMs = means[first + trunc(kept / 2)];
      r.minOpsPerSec = ops[first];
      r.maxOpsPerSec = ops[last];
      // each call of a last pass timed on its own, for the percentiles
      const count = iterations > MAX_PERCENTILE_SAMPLES ? MAX_PERCENTILE_SAMPLES : iterations;
      const samples = new Float64Array(count);
      for (let k = 0; k < count; k++) {
        const start = now();
        const v = fn();
        if (isThenable(v)) await v;
        samples[k] = now() - start;
      }
      forget();
      samples.sort();
      const percentile = (q) => count === 0 ? 0 : samples[trunc(q * (count - 1))] / 1000000;
      r.sampleCount = count;
      r.minSampleMs = percentile(0);
      r.p25Ms = percentile(0.25);
      r.medianMs = percentile(0.5);
      r.p75Ms = percentile(0.75);
      r.p99Ms = percentile(0.99);
      r.p999Ms = percentile(0.999);
      r.maxSampleMs = percentile(1);
    } catch (e) {
      if (it !== null) closeKeepingError(it);
      throw e;
    }
    if (it !== null) {
      const start = now();
      close(it);
      forget();
      r.teardownMs = (now() - start) / 1000000;
    }
    return r;
  }

  // --profile-deterministic: each benchmark once, setup and teardown included, nothing timed
  async function measureOnce(c) {
    const r = result(c);
    r.iterations = 1;
    let fn = c.run, it = null;
    try {
      if (c.generator !== null) ({ it, fn } = startGenerator(c));
      const v = fn();
      if (isThenable(v)) await v;
      forget();
    } catch (e) {
      if (it !== null) closeKeepingError(it);
      throw e;
    }
    if (it !== null) {
      close(it);
      forget();
    }
    return r;
  }

  // a thrown value as the report names it: an error's "Name: message", anything else as text
  function describe(e) {
    try {
      if (isObject(e)) {
        const name = e.name, message = e.message;
        if (typeof name === 'string' && typeof message === 'string') return message === '' ? name : name + ': ' + message;
      }
      return String(e);
    } catch {
      return 'benchmark failed';
    }
  }

  async function runAll(deterministic, progress) {
    if (lastRun !== null && lastDeterministic === deterministic) return lastRun;
    const start = now();
    const results = [];
    const total = cases.length;
    for (let i = 0; i < total; i++) {
      const c = cases[i];
      if (progress) n.progress(c.suite, c.name, i + 1, total);
      let r;
      try {
        r = deterministic ? await measureOnce(c) : await measure(c);
      } catch (e) {
        // the next benchmark does not inherit this one's queued work
        discardJobs();
        r = { name: c.name, suite: c.suite, summaryScope: c.summaryScope, boxplotScope: c.boxplotScope, error: describe(e) };
        thrown.set(r, e);
      }
      append(results, r);
    }
    const run = { totalBenchmarks: total, results, durationNanoseconds: now() - start };
    lastRun = run;
    lastDeterministic = deterministic;
    return run;
  }

  // the script's own run(): measured as the host would, and the host's report then reuses it
  function run() {
    return runAll(false, n.progressOn);
  }

  const api = { bench, group, run, summary, boxplot };
  const namespace = { bench, group, run, summary, boxplot };
  if (shared && shared.hostModules) shared.hostModules['goccia:microbench'] = () => ({ ...api, default: namespace });
  return {
    report(deterministic, done) {
      return runAll(deterministic, n.progressOn).then((r) => {
        const results = r.results;
        for (let i = 0; i < results.length; i++) {
          const x = results[i];
          if (x.error !== undefined) entry(x.suite, x.name, x.summaryScope, x.boxplotScope, true, thrown.get(x));
          else entry(x.suite, x.name, x.summaryScope, x.boxplotScope, false, undefined, x.iterations, x.opsPerSec, x.meanMs,
            x.variancePercentage, x.setupMs, x.teardownMs, x.minOpsPerSec, x.maxOpsPerSec, x.sampleCount, x.minSampleMs,
            x.p25Ms, x.medianMs, x.p75Ms, x.p99Ms, x.p999Ms, x.maxSampleMs);
        }
        done(r.totalBenchmarks, r.durationNanoseconds);
      }, (e) => done(undefined, undefined, describe(e)));
    }
  };
})
