{ The vitest compatibility shim's module source (docs/testing-api.md, Vitest compatibility): the body of a
  function returning it, with LB the line break. Shared by Goccia.RuntimeExtensions.VitestCompat and Goccia.HostAPI. }
  Result :=
    '// GocciaScript vitest compatibility shim. Shipped inside the runner.' + LB +
    'import {' + LB +
    '  describe,' + LB +
    '  test,' + LB +
    '  it,' + LB +
    '  expect,' + LB +
    '  beforeAll,' + LB +
    '  beforeEach,' + LB +
    '  afterEach,' + LB +
    '  afterAll,' + LB +
    '  onTestFinished,' + LB +
    '  mock,' + LB +
    '  spyOn,' + LB +
    '} from "goccia:test";' + LB +
    LB +
    '// The virtual timer queue. `goccia:timers` speaks in epoch milliseconds;' + LB +
    '// wrapping those in Date and returning `vi` for chaining is this shim.' + LB +
    'import {' + LB +
    '  useFakeTimers as engineUseFakeTimers,' + LB +
    '  useRealTimers as engineUseRealTimers,' + LB +
    '  isFakeTimers as engineIsFakeTimers,' + LB +
    '  setSystemTime as engineSetSystemTime,' + LB +
    '  getMockedSystemTime as engineGetMockedSystemTime,' + LB +
    '  getRealSystemTime as engineGetRealSystemTime,' + LB +
    '  advanceTimersByTime as engineAdvanceTimersByTime,' + LB +
    '  advanceTimersByTimeAsync as engineAdvanceTimersByTimeAsync,' + LB +
    '  advanceTimersToNextTimer as engineAdvanceTimersToNextTimer,' + LB +
    '  advanceTimersToNextTimerAsync as engineAdvanceTimersToNextTimerAsync,' + LB +
    '  runAllTimers as engineRunAllTimers,' + LB +
    '  runAllTimersAsync as engineRunAllTimersAsync,' + LB +
    '  runOnlyPendingTimers as engineRunOnlyPendingTimers,' + LB +
    '  runOnlyPendingTimersAsync as engineRunOnlyPendingTimersAsync,' + LB +
    '  clearAllTimers as engineClearAllTimers,' + LB +
    '  getTimerCount as engineGetTimerCount,' + LB +
    '} from "goccia:timers";' + LB +
    LB +
    'export {' + LB +
    '  describe,' + LB +
    '  test,' + LB +
    '  it,' + LB +
    '  expect,' + LB +
    '  beforeAll,' + LB +
    '  beforeEach,' + LB +
    '  afterEach,' + LB +
    '  afterAll,' + LB +
    '  onTestFinished,' + LB +
    '  mock,' + LB +
    '  spyOn,' + LB +
    '};' + LB +
    LB +
    'const DOCS =' + LB +
    '  "See docs/testing-api.md (Vitest compatibility) for the supported surface.";' + LB +
    LB +
    'const unsupported = (member, reason) => () => {' + LB +
    '  throw new Error(' + LB +
    '    "vi." + member + " is not supported by the GocciaScript vitest " +' + LB +
    '      "compatibility shim. " + reason + " " + DOCS,' + LB +
    '  );' + LB +
    '};' + LB +
    LB +
    '// vi.mock and vi.unmock are hoisted out of the test file before it is' + LB +
    '// parsed, so by the time the call runs the module graph is already' + LB +
    '// wired: the call itself has nothing left to do.' + LB +
    'const hoistedDirective = (specifier, factory) => undefined;' + LB +
    LB +
    'const DYNAMIC_MOCKING =' + LB +
    '  "vi.mock with a factory is supported and is applied before the module " +' + LB +
    '  "graph is loaded; this member instead has to re-point a module after " +' + LB +
    '  "loading, which needs the module-cache eviction the GocciaScript " +' + LB +
    '  "module loader does not provide.";' + LB +
    'const NO_ACTUAL_MODULE =' + LB +
    '  "vi.mock with a factory is supported, but the real module is not " +' + LB +
    '  "reachable once it is mocked: GocciaScript serves exactly one module " +' + LB +
    '  "per resolved address, so the original cannot be imported alongside " +' + LB +
    '  "the mock. Partial mocks that spread the actual module are therefore " +' + LB +
    '  "not supported either.";' + LB +
    'const FACTORY_SCOPE =' + LB +
    '  "vi.mock with a factory is supported, but the factory is relocated " +' + LB +
    '  "into its own module scope, so there is no shared hoisted-variable " +' + LB +
    '  "scope for it to read.";' + LB +
    'const NO_FRAME_CLOCK =' + LB +
    '  "the timer queue is supported, but this member drives ' +
      'requestAnimationFrame, " +' + LB +
    '  "which GocciaScript does not provide: there is no display to pace a " +' + LB +
    '  "frame against.";' + LB +
    'const NO_NEXT_TICK =' + LB +
    '  "the timer queue is supported, but this member drains " +' + LB +
    '  "process.nextTick, and GocciaScript has no process. Promise jobs run " +' + LB +
    '  "on the engine microtask queue, which the async advance members " +' + LB +
    '  "already drain.";' + LB +
    'const NO_AUTO_ADVANCE =' + LB +
    '  "the timer queue is supported, and the manual mode is already how it " +' + LB +
    '  "behaves, so that one is accepted. Every other mode advances the " +' + LB +
    '  "clock against real elapsed time, which no GocciaScript clock " +' + LB +
    '  "measures. Advance the timers explicitly instead.";' + LB +
    'const ASYNC_POLLING =' + LB +
    '  "this member polls asynchronously, retrying its callback until the " +' + LB +
    '  "condition holds or a timeout elapses, which needs execution to " +' + LB +
    '  "suspend and resume between attempts. GocciaScript has no such " +' + LB +
    '  "primitive: await is a synchronous drain, and the virtual timer queue " +' + LB +
    '  "only moves when a test moves it, so a poll loop would spin without " +' + LB +
    '  "anything being able to change the condition.";' + LB +
    'const CONFIG =' + LB +
    '  "GocciaScript has no runtime-mutable test configuration.";' + LB +
    LB +
    '// vi.fn and vi.spyOn are the registry Vitest drains in clearAllMocks,' + LB +
    '// resetAllMocks and restoreAllMocks, so registration happens here rather' + LB +
    '// than inside `mock` and `spyOn` themselves: those are also GocciaScript' + LB +
    '// globals, and a suite that never touches `vi` should not pay for a' + LB +
    '// registry it cannot drain.' + LB +
    'const createdMocks = [];' + LB +
    'const createdSpies = [];' + LB +
    LB +
    'const registerMock = (...args) => {' + LB +
    '  const created = mock(...args);' + LB +
    '  createdMocks.push(created);' + LB +
    '  return created;' + LB +
    '};' + LB +
    LB +
    '// The spy records the descriptor it displaced, because restoreAllMocks' + LB +
    '// puts that descriptor back WITHOUT touching the recorded calls. Calling' + LB +
    '// mockRestore() here instead would clear them, which is what the direct' + LB +
    '// member does and the bulk one does not.' + LB +
    'const registerSpy = (target, key, ...rest) => {' + LB +
    '  const previous = Object.getOwnPropertyDescriptor(target, key);' + LB +
    '  const created = spyOn(target, key, ...rest);' + LB +
    '  createdSpies.push({ mock: created, target, key, previous });' + LB +
    '  return created;' + LB +
    '};' + LB +
    LB +
    '// Vitest semantics: clear drops recorded calls, reset also drops the' + LB +
    '// implementations added after creation, and restore reverts a spy to the' + LB +
    '// method it replaced while leaving bare vi.fn mocks alone.' + LB +
    'const everyMock = () => [' + LB +
    '  ...createdMocks,' + LB +
    '  ...createdSpies.map((entry) => entry.mock),' + LB +
    '];' + LB +
    LB +
    'const clearAllMocks = () => {' + LB +
    '  everyMock().forEach((entry) => entry.mockClear());' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    'const resetAllMocks = () => {' + LB +
    '  everyMock().forEach((entry) => entry.mockReset());' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    '// Descriptor-only, and deliberately not entry.mock.mockRestore(): the' + LB +
    '// bulk member reverts the target and leaves the spy reporting the calls' + LB +
    '// it recorded, so a suite can still assert on them afterwards. The spies' + LB +
    '// stay registered, as they do in Vitest, so a later clearAllMocks still' + LB +
    '// reaches that history.' + LB +
    'const restoreAllMocks = () => {' + LB +
    '  createdSpies.forEach((entry) => {' + LB +
    '    if (entry.previous) {' + LB +
    '      Object.defineProperty(entry.target, entry.key, entry.previous);' + LB +
    '    } else {' + LB +
    '      delete entry.target[entry.key];' + LB +
    '    }' + LB +
    '  });' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    '// Global stubs. The first stub of a name records the value that was there' + LB +
    '// before it, so restubbing the same name repeatedly still unwinds to the' + LB +
    '// original — and a name that did not exist is deleted rather than left' + LB +
    '// behind as an undefined global.' + LB +
    'const globalStubs = [];' + LB +
    LB +
    'const stubGlobal = (name, value) => {' + LB +
    '  if (!globalStubs.some((stub) => stub.name === name)) {' + LB +
    '    globalStubs.push({' + LB +
    '      name,' + LB +
    '      existed: Object.prototype.hasOwnProperty.call(globalThis, name),' + LB +
    '      value: globalThis[name],' + LB +
    '    });' + LB +
    '  }' + LB +
    '  globalThis[name] = value;' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    '// Environment stubs. GocciaScript has no process of its own, so the' + LB +
    '// environment is whatever the host injected — `--global` or `--globals`' + LB +
    '// put it there, and it is an ordinary object this writes to. Vitest' + LB +
    '// coerces the value with String() and treats undefined as a deletion,' + LB +
    '// so the same source sets and clears a variable either way.' + LB +
    'const envStubs = [];' + LB +
    LB +
    'const environment = () => {' + LB +
    '  const found = typeof process === "undefined" ? undefined : process.env;' + LB +
    '  if (found === undefined || found === null) {' + LB +
    '    throw new Error(' + LB +
    '      "vi.stubEnv needs a process.env to write to, and GocciaScript has " +' + LB +
    '        "no process of its own. Inject one with --global or --globals, " +' + LB +
    '        "as a process key holding an env object. " + DOCS,' + LB +
    '    );' + LB +
    '  }' + LB +
    '  return found;' + LB +
    '};' + LB +
    LB +
    'const stubEnv = (name, value) => {' + LB +
    '  const env = environment();' + LB +
    '  if (!envStubs.some((stub) => stub.name === name)) {' + LB +
    '    envStubs.push({' + LB +
    '      name,' + LB +
    '      existed: Object.prototype.hasOwnProperty.call(env, name),' + LB +
    '      value: env[name],' + LB +
    '    });' + LB +
    '  }' + LB +
    '  if (value === undefined) {' + LB +
    '    delete env[name];' + LB +
    '  } else {' + LB +
    '    env[name] = String(value);' + LB +
    '  }' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    'const unstubAllEnvs = () => {' + LB +
    '  const env = environment();' + LB +
    '  [...envStubs].reverse().forEach((stub) => {' + LB +
    '    if (stub.existed) {' + LB +
    '      env[stub.name] = stub.value;' + LB +
    '    } else {' + LB +
    '      delete env[stub.name];' + LB +
    '    }' + LB +
    '  });' + LB +
    '  envStubs.splice(0, envStubs.length);' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    'const unstubAllGlobals = () => {' + LB +
    '  [...globalStubs].reverse().forEach((stub) => {' + LB +
    '    if (stub.existed) {' + LB +
    '      globalThis[stub.name] = stub.value;' + LB +
    '    } else {' + LB +
    '      delete globalThis[stub.name];' + LB +
    '    }' + LB +
    '  });' + LB +
    '  globalStubs.splice(0, globalStubs.length);' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    '// Fake timers. The clock and the queue live in the engine; everything' + LB +
    '// here is the shape Vitest exposes them in — Date instead of epoch' + LB +
    '// milliseconds, and `vi` back for chaining.' + LB +
    '// Vitest passes anything that is not already a Date through the Date' + LB +
    '// constructor, so a date STRING is supported API — `Number(value)` on' + LB +
    '// one produces NaN instead of an instant. The engine refuses a' + LB +
    '// non-finite result rather than installing a NaN clock the way Vitest' + LB +
    '// does; see docs/testing-api.md.' + LB +
    'const toEpochMilliseconds = (value) =>' + LB +
    '  (value instanceof Date ? value : new Date(value)).getTime();' + LB +
    LB +
    '// Only `now` is honoured. `toFake` has nothing to select from — there is' + LB +
    '// exactly one timer queue and it is always the faked one — and the' + LB +
    '// auto-advance options describe real elapsed time, which no GocciaScript' + LB +
    '// clock ever measures.' + LB +
    'const useFakeTimers = (config) => {' + LB +
    '  engineUseFakeTimers(' + LB +
    '    config && config.now !== undefined' + LB +
    '      ? toEpochMilliseconds(config.now)' + LB +
    '      : undefined,' + LB +
    '  );' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    'const setSystemTime = (value) => {' + LB +
    '  engineSetSystemTime(' + LB +
    '    value === undefined ? undefined : toEpochMilliseconds(value),' + LB +
    '  );' + LB +
    '  return vi;' + LB +
    '};' + LB +
    LB +
    'const getMockedSystemTime = () => {' + LB +
    '  const milliseconds = engineGetMockedSystemTime();' + LB +
    '  return milliseconds === null ? null : new Date(milliseconds);' + LB +
    '};' + LB +
    LB +
    'export const vi = {' + LB +
    '  fn: registerMock,' + LB +
    '  spyOn: registerSpy,' + LB +
    LB +
    '  // A TypeScript type helper: at runtime it is the identity function, and' + LB +
    '  // Vitest defines it that way too.' + LB +
    '  mocked: (value) => value,' + LB +
    LB +
    '  mock: hoistedDirective,' + LB +
    '  unmock: hoistedDirective,' + LB +
    LB +
    '  doMock: unsupported("doMock", DYNAMIC_MOCKING),' + LB +
    '  doUnmock: unsupported("doUnmock", DYNAMIC_MOCKING),' + LB +
    '  resetModules: unsupported("resetModules", DYNAMIC_MOCKING),' + LB +
    '  importActual: unsupported("importActual", NO_ACTUAL_MODULE),' + LB +
    '  importMock: unsupported("importMock", NO_ACTUAL_MODULE),' + LB +
    '  hoisted: unsupported("hoisted", FACTORY_SCOPE),' + LB +
    LB +
    '  useFakeTimers: useFakeTimers,' + LB +
    '  useRealTimers: () => { engineUseRealTimers(); return vi; },' + LB +
    '  isFakeTimers: () => engineIsFakeTimers(),' + LB +
    '  setSystemTime: setSystemTime,' + LB +
    '  getMockedSystemTime: getMockedSystemTime,' + LB +
    '  getRealSystemTime: () => engineGetRealSystemTime(),' + LB +
    '  getTimerCount: () => engineGetTimerCount(),' + LB +
    '  clearAllTimers: () => { engineClearAllTimers(); return vi; },' + LB +
    LB +
    '  advanceTimersByTime: (ms) => {' + LB +
    '    engineAdvanceTimersByTime(ms);' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    '  advanceTimersByTimeAsync: async (ms) => {' + LB +
    '    await engineAdvanceTimersByTimeAsync(ms);' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    '  advanceTimersToNextTimer: () => {' + LB +
    '    engineAdvanceTimersToNextTimer();' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    '  advanceTimersToNextTimerAsync: async () => {' + LB +
    '    await engineAdvanceTimersToNextTimerAsync();' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    '  runAllTimers: () => { engineRunAllTimers(); return vi; },' + LB +
    '  runAllTimersAsync: async () => { await engineRunAllTimersAsync(); return vi; },' + LB +
    '  runOnlyPendingTimers: () => {' + LB +
    '    engineRunOnlyPendingTimers();' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    '  runOnlyPendingTimersAsync: async () => {' + LB +
    '    await engineRunOnlyPendingTimersAsync();' + LB +
    '    return vi;' + LB +
    '  },' + LB +
    LB +
    '  // The three timer members the queue cannot honour. Named errors rather' + LB +
    '  // than absent properties, so a suite reaching for one is told which' + LB +
    '  // part of the family is missing and why.' + LB +
    '  advanceTimersToNextFrame:' + LB +
    '    unsupported("advanceTimersToNextFrame", NO_FRAME_CLOCK),' + LB +
    '  runAllTicks: unsupported("runAllTicks", NO_NEXT_TICK),' + LB +
    LB +
    '  // "manual" is not a mode this has to implement — it is a description' + LB +
    '  // of the only behaviour there is, so asking for it is satisfied by' + LB +
    '  // doing nothing. The auto-advancing modes are the unsupported ones.' + LB +
    '  setTimerTickMode: (mode) => {' + LB +
    '    if (mode === "manual") return vi;' + LB +
    '    return unsupported("setTimerTickMode", NO_AUTO_ADVANCE)();' + LB +
    '  },' + LB +
    LB +
    '  stubGlobal: stubGlobal,' + LB +
    '  unstubAllGlobals: unstubAllGlobals,' + LB +
    '  stubEnv: stubEnv,' + LB +
    '  unstubAllEnvs: unstubAllEnvs,' + LB +
    LB +
    '  clearAllMocks: clearAllMocks,' + LB +
    '  resetAllMocks: resetAllMocks,' + LB +
    '  restoreAllMocks: restoreAllMocks,' + LB +
    LB +
    '  waitFor: unsupported("waitFor", ASYNC_POLLING),' + LB +
    '  waitUntil: unsupported("waitUntil", ASYNC_POLLING),' + LB +
    '  setConfig: unsupported("setConfig", CONFIG),' + LB +
    '  resetConfig: unsupported("resetConfig", CONFIG),' + LB +
    '};' + LB +
    LB +
    '// Vitest exports the same namespace object under both names, and hoists' + LB +
    '// `vitest.mock(...)` exactly as it hoists `vi.mock(...)`.' + LB +
    'export const vitest = vi;' + LB;
