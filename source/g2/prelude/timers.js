// setTimeout, setInterval, clearTimeout and clearInterval over GocciaScript's deterministic virtual timer queue,
// and the goccia:timers control surface (docs/adr/0113-deterministic-virtual-timer-queue.md). No timer waits on
// wall time: a timer has a due time on a virtual clock, and fires only when something moves that clock.
//
// Real timers: the clock jumps to the next due timer whenever the engine runs out of work (G2.HostAPI's event loop
// calls shared.timers.fireNext once the microtasks drain), and the test runner drains a test's timers when its body
// ends. Fake timers: the clock moves only when the suite moves it (advanceTimersByTime and the rest), with the
// semantics probed against Vitest 4.1.10's fake timers that the ADR tabulates.
//
// Called once per realm with the global object, the host's natives (n) and the preludes' shared internals; sets
// shared.timers and shared.hostModules['goccia:timers'].
(function (G, n, shared) {
  'use strict';
  const hidden = (o, k, v) => Object.defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true });
  const LOOP_LIMIT = 10000;
  // one advance may fire a great many timers (a 10ms interval over an hour fires 360000); the bound only stops a
  // zero-period interval, which re-arms at the instant it ran so the clock cannot move past it
  const ADVANCE_LIMIT = 1000000;
  const ADVANCE_LIMIT_MESSAGE = `Aborting after firing ${ADVANCE_LIMIT} timers in a single advance: a timer keeps ` +
    'rescheduling itself at the current instant, so the clock cannot move past it. A setInterval with a period of 0 does this.';
  const NON_FINITE = 'The system time must be a finite number of milliseconds or a valid Date.';
  // the mocked clock is published as an Int64 nanosecond count: the largest whole milliseconds that fit
  const MAX_CLOCK = 9223372036854;
  const OUT_OF_RANGE = 'The resulting time is too large to represent as a nanosecond clock.';
  function requireFinite(v) { if (!Number.isFinite(v)) throw new TypeError(NON_FINITE); }
  function requireInRange(ms) { if (!(Math.abs(ms) <= MAX_CLOCK)) throw new RangeError(OUT_OF_RANGE); }
  const NOT_MOCKED = 'A function to advance timers was called but the timers APIs are not mocked. Call `vi.useFakeTimers()` in the test file first.';

  // ---------------- the queue ----------------
  const queue = new Map();   // id -> { id, callAt, createdAt, fn, args, interval }
  let nextId = 1;
  let fake = false;
  let now = 0;               // the clock: virtual ms under real timers, epoch ms under fake timers
  let installedAt = 0;       // fake timers: the clock when installed (performance.now counts from it)
  let installOrigin = 0;     // fake timers: the instant they were installed at, which clearAllTimers rewinds to
  let frozenDate = null;     // setSystemTime without fake timers: the frozen epoch ms
  let ticking = 0;           // > 0 while an advance runs (a zero delay is then due at now + 1)
  let parked = null;         // real timers: the first exception a callback threw, for the runner
  let banked = 0;            // fake timers: the fraction of a millisecond the advances have not yet moved
  const hostTasks = [];      // resolvers of the async advances' yields, run once the microtasks drain

  function syncClocks() {
    if (fake) n.setClock(now, now - installedAt);
    else if (frozenDate !== null) n.setClock(frozenDate, NaN);
    else n.setClock(NaN, NaN);
  }

  function delayOf(ms) {
    let d = Number(ms);
    if (!(d > 0)) d = 0;   // NaN, negative and -0 alike
    if (d > 2147483647) d = 1;   // past the 32-bit range: as browsers and Node, almost at once
    return Math.trunc(d);
  }
  function add(handler, ms, args, repeat) {
    if (typeof handler !== 'function') throw new TypeError('The "callback" argument must be of type function');
    const id = nextId++;
    let delay = delayOf(ms);
    if (delay === 0 && ticking > 0) delay = 1;
    queue.set(id, { id, callAt: now + delay, createdAt: now, fn: handler, args, interval: repeat ? delayOf(ms) : -1,
      ctx: n.asyncGet() });
    return id;
  }
  function clear(id) {
    if (typeof id === 'number') queue.delete(id);
    else if (id !== null && typeof id === 'object' && typeof id.valueOf === 'function') {
      const v = id.valueOf();
      if (typeof v === 'number') queue.delete(v);
    }
  }
  // the earliest timer due at or before limit (all of them without one): due time, then creation, then id
  function earliest(limit = Infinity, intervals = true) {
    let best;
    for (const t of queue.values()) {
      if (t.callAt > limit || (!intervals && t.interval >= 0)) continue;
      if (best === undefined || t.callAt < best.callAt || (t.callAt === best.callAt &&
          (t.createdAt < best.createdAt || (t.createdAt === best.createdAt && t.id < best.id)))) best = t;
    }
    return best;
  }
  // runs timer t at its due time; an interval is rescheduled first, so it does not drift
  function run(t) {
    if (t.callAt > now) now = t.callAt;
    if (t.interval >= 0) t.callAt += t.interval;
    else queue.delete(t.id);
    syncClocks();
    // in the async context it was scheduled in
    const saved = n.asyncGet();
    n.asyncSet(t.ctx);
    try { Reflect.apply(t.fn, undefined, t.args); } finally { n.asyncSet(saved); }
  }

  // ---------------- real timers: the event loop and the runner ----------------
  // the next host task, else under real timers the next timer (intervals too when asked); false when neither runs
  let budget = LOOP_LIMIT;
  function fireNext(intervals) {
    if (hostTasks.length > 0) { hostTasks.shift()(); return true; }
    if (fake) return false;
    const t = earliest(Infinity, intervals);
    if (t === undefined || budget <= 0) return false;
    budget--;
    try { run(t); } catch (e) { if (parked === null) parked = { error: e }; }
    return true;
  }
  // the test runner, after a test body: its timers (not its intervals) in due order, leftovers dropped
  async function drainTest() {
    budget = LOOP_LIMIT;
    if (!fake) {
      for (let i = 0; i < LOOP_LIMIT; i++) {
        const t = earliest(Infinity, false);
        if (t === undefined) break;
        try { run(t); } catch (e) { if (parked === null) parked = { error: e }; }
        await yieldTask();
      }
      queue.clear();
    }
    const p = parked;
    parked = null;
    return p;
  }
  function yieldTask() { return new Promise((resolve) => { hostTasks.push(resolve); }); }

  // ---------------- fake timers ----------------
  function requireFake() { if (!fake) throw new Error(NOT_MOCKED); }
  function useFakeTimers(start = undefined) {
    let at;
    if (start !== undefined && !Number.isNaN(Number(start))) at = Number(start);
    else if (fake) at = now;
    else if (frozenDate !== null) at = frozenDate;
    else at = n.realNow();
    requireFinite(at);
    requireInRange(at);
    queue.clear();
    banked = 0;
    fake = true;
    now = at;
    installedAt = at;
    installOrigin = at;
    frozenDate = null;
    syncClocks();
  }
  function useRealTimers() {
    if (fake) queue.clear();
    fake = false;
    now = 0;
    frozenDate = null;
    syncClocks();
  }
  function setSystemTime(ms = undefined) {
    let t = ms === undefined ? n.realNow() : Number(ms);
    requireFinite(t);
    requireInRange(t);
    t = Math.trunc(t);
    if (fake) {
      const delta = t - now;
      now = t;
      for (const e of queue.values()) { e.callAt += delta; e.createdAt += delta; }
      installedAt += delta;   // performance.now is not moved by setSystemTime
    } else frozenDate = t;
    syncClocks();
  }
  // moves the clock by ms: every timer due by then, no microtasks between (async: drained before the first and
  // after each); the first exception recorded and rethrown at the end
  // the advance's target: whole milliseconds move the clock, the fraction is banked; checked before anything moves
  function tickTarget(ms) {
    requireFake();
    const d = Number(ms);
    requireFinite(d);
    if (d < 0) throw new TypeError('Negative ticks are not supported');
    let target = now + Math.trunc(d);
    let bank = banked + (d - Math.trunc(d));
    if (bank >= 1 - 1e-9) { target += 1; bank -= 1; }
    requireInRange(target);
    requireInRange(target - installedAt);
    banked = bank;
    return target;
  }
  function tickSync(ms) {
    const target = tickTarget(ms);
    let error = null, count = 0;
    ticking++;
    try {
      for (;;) {
        const t = earliest(target);
        if (t === undefined) break;
        if (++count > ADVANCE_LIMIT) throw new Error(ADVANCE_LIMIT_MESSAGE);
        try { run(t); } catch (e) { if (error === null) error = { e }; }
      }
    } finally { ticking--; }
    if (target > now) now = target;
    syncClocks();
    if (error !== null) throw error.e;
  }
  async function tickAsync(ms) {
    const target = tickTarget(ms);
    let error = null, count = 0;
    await yieldTask();
    for (;;) {
      const t = earliest(target);
      if (t === undefined) break;
      if (++count > ADVANCE_LIMIT) throw new Error(ADVANCE_LIMIT_MESSAGE);
      ticking++;
      try { run(t); } catch (e) { if (error === null) error = { e }; } finally { ticking--; }
      await yieldTask();
    }
    if (target > now) now = target;
    syncClocks();
    if (error !== null) throw error.e;
  }
  // the next timer, refused before it fires when its due time is past what the clock can hold
  function nextChecked() {
    const t = earliest();
    if (t !== undefined) { requireInRange(t.callAt); requireInRange(t.callAt - installedAt); }
    return t;
  }
  function next() {
    requireFake();
    const t = nextChecked();
    if (t === undefined) return;
    ticking++;
    try { run(t); } finally { ticking--; }
    tickSync(0);
  }
  async function nextAsync() {
    requireFake();
    await yieldTask();
    const t = nextChecked();
    if (t === undefined) return;
    ticking++;
    try { run(t); } finally { ticking--; }
    await tickAsync(0);
  }
  function runAll() {
    requireFake();
    for (let i = 0; i < LOOP_LIMIT; i++) {
      const t = nextChecked();
      if (t === undefined) return;
      ticking++;
      try { run(t); } finally { ticking--; }
    }
    if (queue.size > 0) throw new Error(`Aborting after running ${LOOP_LIMIT} timers, assuming an infinite loop!`);
  }
  async function runAllAsync() {
    requireFake();
    await yieldTask();
    for (let i = 0; i < LOOP_LIMIT; i++) {
      const t = nextChecked();
      if (t === undefined) return;
      ticking++;
      try { run(t); } finally { ticking--; }
      await yieldTask();
    }
    if (queue.size > 0) throw new Error(`Aborting after running ${LOOP_LIMIT} timers, assuming an infinite loop!`);
  }
  function lastPendingDue() {
    let last = now;
    for (const t of queue.values()) if (t.callAt > last) last = t.callAt;
    return last;
  }
  function runOnlyPending() { requireFake(); tickSync(lastPendingDue() - now); }
  async function runOnlyPendingAsync() { requireFake(); await tickAsync(lastPendingDue() - now); }

  // ---------------- the globals and goccia:timers ----------------
  function setTimeout(handler, timeout = 0, ...args) { return add(handler, timeout, args, false); }
  function setInterval(handler, timeout = 0, ...args) { return add(handler, timeout, args, true); }
  function clearTimeout(id = undefined) { clear(id); }
  function clearInterval(id = undefined) { clear(id); }
  // a host without timers (GocciaRunner) keeps the queue for its own users (AbortSignal.timeout) but exposes
  // neither the globals nor goccia:timers
  const exposed = shared.exposeTimers !== false;
  if (exposed) {
    hidden(G, 'setTimeout', setTimeout);
    hidden(G, 'setInterval', setInterval);
    hidden(G, 'clearTimeout', clearTimeout);
    hidden(G, 'clearInterval', clearInterval);
  }

  const timersModule = {
    useFakeTimers(now = undefined) { useFakeTimers(now); },
    useRealTimers() { useRealTimers(); },
    isFakeTimers() { return fake; },
    setSystemTime(ms = undefined) { setSystemTime(ms); },
    getMockedSystemTime() { return fake ? now : frozenDate; },
    getRealSystemTime() { return n.realNow(); },
    advanceTimersByTime(ms = 0) { tickSync(ms); },
    advanceTimersByTimeAsync(ms = 0) { return tickAsync(ms); },
    advanceTimersToNextTimer(steps = 1) { for (let i = 0; i < steps; i++) next(); },
    async advanceTimersToNextTimerAsync(steps = 1) { for (let i = 0; i < steps; i++) await nextAsync(); },
    runAllTimers() { runAll(); },
    runAllTimersAsync() { return runAllAsync(); },
    runOnlyPendingTimers() { runOnlyPending(); },
    runOnlyPendingTimersAsync() { return runOnlyPendingAsync(); },
    clearAllTimers() {
      // the fake clock's reset: nothing pending, and the clock back at the instant fake timers were installed
      queue.clear();
      if (fake) { now = installOrigin; installedAt = installOrigin; banked = 0; syncClocks(); }
    },
    getTimerCount() { requireFake(); return queue.size; }
  };
  if (exposed) shared.hostModules['goccia:timers'] = () => timersModule;
  shared.timers = { fireNext, drainTest, add, clear, count: () => queue.size };
  if (exposed) shared.runtimeGlobals.push('setTimeout', 'setInterval', 'clearTimeout', 'clearInterval');
})
