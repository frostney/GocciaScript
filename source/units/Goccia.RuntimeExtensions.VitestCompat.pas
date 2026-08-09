unit Goccia.RuntimeExtensions.VitestCompat;

{$I Goccia.inc}

{ Vitest compatibility shim.

  Resolves the bare `vitest` specifier to a small module that re-exports
  `goccia:test` and assembles the `vi` namespace on top of it. The shim ships
  inside the binary as source text rather than as a file on disk, so a suite
  written against Vitest imports the same way under GocciaScript.

  `vi` lives only here. The engine never grows a `vi` namespace, and every
  member the engine cannot honestly provide is a function that throws a named,
  actionable error instead of silently doing nothing. }

interface

uses
  Goccia.Runtime;

const
  VITEST_COMPAT_SPECIFIER = 'vitest';

type
  TGocciaVitestCompatRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

function VitestCompatShimSource: string;

implementation

uses
  SysUtils;

function VitestCompatShimSource: string;
const
  LB = sLineBreak;
begin
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
    'const MODULE_REGISTRY =' + LB +
    '  "GocciaScript resolves every import once, at load time, and keeps no " +' + LB +
    '  "module registry to intercept, so module mocking cannot be emulated.";' + LB +
    'const FAKE_TIMERS =' + LB +
    '  "GocciaScript has no fake-timer clock; timers run on the real event loop.";' + LB +
    'const GLOBAL_STUBS =' + LB +
    '  "GocciaScript does not snapshot globals or environment variables, so a " +' + LB +
    '  "stub could not be unwound safely.";' + LB +
    'const MOCK_REGISTRY =' + LB +
    '  "GocciaScript keeps no registry of created mocks; call mockClear, " +' + LB +
    '  "mockReset or mockRestore on the mock itself.";' + LB +
    LB +
    'export const vi = {' + LB +
    '  fn: mock,' + LB +
    '  spyOn: spyOn,' + LB +
    LB +
    '  mock: unsupported("mock", MODULE_REGISTRY),' + LB +
    '  unmock: unsupported("unmock", MODULE_REGISTRY),' + LB +
    '  doMock: unsupported("doMock", MODULE_REGISTRY),' + LB +
    '  doUnmock: unsupported("doUnmock", MODULE_REGISTRY),' + LB +
    '  mocked: unsupported("mocked", MODULE_REGISTRY),' + LB +
    '  importActual: unsupported("importActual", MODULE_REGISTRY),' + LB +
    '  importMock: unsupported("importMock", MODULE_REGISTRY),' + LB +
    '  hoisted: unsupported("hoisted", MODULE_REGISTRY),' + LB +
    LB +
    '  useFakeTimers: unsupported("useFakeTimers", FAKE_TIMERS),' + LB +
    '  useRealTimers: unsupported("useRealTimers", FAKE_TIMERS),' + LB +
    '  isFakeTimers: unsupported("isFakeTimers", FAKE_TIMERS),' + LB +
    '  setSystemTime: unsupported("setSystemTime", FAKE_TIMERS),' + LB +
    '  getMockedSystemTime: unsupported("getMockedSystemTime", FAKE_TIMERS),' + LB +
    '  getRealSystemTime: unsupported("getRealSystemTime", FAKE_TIMERS),' + LB +
    '  advanceTimersByTime: unsupported("advanceTimersByTime", FAKE_TIMERS),' + LB +
    '  advanceTimersByTimeAsync:' + LB +
    '    unsupported("advanceTimersByTimeAsync", FAKE_TIMERS),' + LB +
    '  advanceTimersToNextTimer:' + LB +
    '    unsupported("advanceTimersToNextTimer", FAKE_TIMERS),' + LB +
    '  runAllTimers: unsupported("runAllTimers", FAKE_TIMERS),' + LB +
    '  runOnlyPendingTimers: unsupported("runOnlyPendingTimers", FAKE_TIMERS),' + LB +
    LB +
    '  stubEnv: unsupported("stubEnv", GLOBAL_STUBS),' + LB +
    '  stubGlobal: unsupported("stubGlobal", GLOBAL_STUBS),' + LB +
    '  unstubAllEnvs: unsupported("unstubAllEnvs", GLOBAL_STUBS),' + LB +
    '  unstubAllGlobals: unsupported("unstubAllGlobals", GLOBAL_STUBS),' + LB +
    LB +
    '  clearAllMocks: unsupported("clearAllMocks", MOCK_REGISTRY),' + LB +
    '  resetAllMocks: unsupported("resetAllMocks", MOCK_REGISTRY),' + LB +
    '  restoreAllMocks: unsupported("restoreAllMocks", MOCK_REGISTRY),' + LB +
    '  resetModules: unsupported("resetModules", MODULE_REGISTRY),' + LB +
    LB +
    '  waitFor: unsupported("waitFor", FAKE_TIMERS),' + LB +
    '  waitUntil: unsupported("waitUntil", FAKE_TIMERS),' + LB +
    '  setConfig: unsupported("setConfig", GLOBAL_STUBS),' + LB +
    '  resetConfig: unsupported("resetConfig", GLOBAL_STUBS),' + LB +
    '};' + LB;
end;

procedure TGocciaVitestCompatRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  Runtime.Engine.ModuleLoader.InjectModule(VITEST_COMPAT_SPECIFIER,
    VitestCompatShimSource);
end;

end.
