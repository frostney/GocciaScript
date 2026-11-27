program ModuleOnlyVitestTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  Goccia.Embed,
  Goccia.HostAPI;

const
  UnresolvedVitest = 'Cannot resolve bare module specifier "vitest"';
  { every name the test runner installs as a global; an import of vitest must publish none of them }
  NoTestGlobals = '["describe", "test", "it", "expect", "mock", "spyOn", "beforeAll", "beforeEach", "afterEach",' +
    ' "afterAll", "onTestFinished", "runTests", "__gocciaTest262Describe", "__gocciaTest262Test", "vi"]' +
    '.every(name => globalThis[name] === undefined)';

procedure Check(const ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

{ vitest stays unresolvable, with the resolver's own diagnostic, unless the embedder opts in with timers on }
procedure CheckUnresolved(const AConfigJson: UnicodeString; const AExposeTimers, ABare: Boolean;
  const AWhat: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    if AConfigJson <> '' then Engine.ConfigJson := AConfigJson;
    Engine.ExposeTimers := AExposeTimers;
    Engine.Bare := ABare;
    RunResult := Engine.Run('vitest-unresolved.mjs', 'import "vitest";');
    Check(not RunResult.Ok and (Pos(UnresolvedVitest, RunResult.ErrorMessage) > 0),
      AWhat + ': ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;
end;

procedure CheckOptIn;
const
  Source = 'import { describe, expect, test, vi } from "vitest";' +
    'import { runTests } from "goccia:test";' +
    'describe("embedded", () => {' +
    '  test("async assertion", async () => expect(await Promise.resolve(42)).toBe(42));' +
    '  test("failed assertion", () => expect(1).toBe(2));' +
    '});' +
    { vi.mock and vi.unmock give the hoisting reason; the other members keep their own, after the host's note }
    'const refused = (member, reason) => { try { vi[member]("./dep.mjs"); return false; }' +
    '  catch (e) { return e.message.includes("vi." + member + " ") && e.message.includes(reason) &&' +
    '    (member === "mock" || member === "unmock" || e.message.includes("This host applies no module mocks")); } };' +
    'const summary = runTests({ showTestResults: false });' +
    'summary.passed === 1 && summary.failed === 1 && summary.totalRunTests === 2 &&' +
    '  summary.failedTests[0].includes("failed assertion") && ' +
    '  refused("mock", "this host does not") && refused("unmock", "this host does not") &&' +
    '  refused("doMock", "module-cache eviction") && refused("doUnmock", "module-cache eviction") &&' +
    '  refused("resetModules", "module-cache eviction") && refused("importActual", "real module is not") &&' +
    '  refused("importMock", "real module is not") && refused("hoisted", "own module scope") && ' +
    NoTestGlobals + ';';
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.ConfigJson := '{"vitest-compat": "true"}';
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('vitest-opt-in.mjs', Source);
    Check(RunResult.Ok and (RunResult.ResultText = 'true'),
      'expected a synchronous test report, refused module mocks and no test globals: ' + RunResult.ResultText +
      RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;
end;

{ TestMode with the opt-in keeps the test host's shim, whose vi.mock is a hoisted directive, not a refusal }
procedure CheckTestModeKeepsRunnerShim;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.TestMode := True;
    Engine.ConfigJson := '{"vitest-compat": "true"}';
    Engine.ResultForm := grfString;
    Check(Engine.PrepareHost(ErrorText) and InstallTestingAPI(ErrorText), ErrorText);
    RunResult := Engine.Run('vitest-test-mode.mjs',
      'import { vi } from "vitest"; String(vi.mock("./dep.mjs", () => ({})));');
    Check(RunResult.Ok and (RunResult.ResultText = 'undefined'),
      'TestMode lost the hoisted vi.mock: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.EndTests;
    Engine.Free;
  end;
end;

begin
  CheckUnresolved('', True, False, 'Vitest was installed by default');
  CheckUnresolved('{"vitest-compat": "true"}', False, False, 'Vitest was installed without timers');
  CheckUnresolved('{"vitest-compat": "true"}', True, True, 'Vitest was installed on a bare host');
  CheckOptIn;
  CheckTestModeKeepsRunnerShim;
  WriteLn('Embedded module-only Vitest: passed');
end.
