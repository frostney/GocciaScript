program ModuleOnlyVitestTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  Goccia.Embed;

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
procedure CheckUnresolved(const AConfigJson: UnicodeString; const AExposeTimers: Boolean; const AWhat: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    if AConfigJson <> '' then Engine.ConfigJson := AConfigJson;
    Engine.ExposeTimers := AExposeTimers;
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
    'const refused = (member) => { try { vi[member]("./dep.mjs"); return false; }' +
    '  catch (e) { return e.message.includes("vi." + member + " ") && e.message.includes("this host does not"); } };' +
    'const summary = runTests({ showTestResults: false });' +
    'summary.passed === 1 && summary.failed === 1 && summary.totalRunTests === 2 &&' +
    '  summary.failedTests[0].includes("failed assertion") && ' +
    '  ["mock", "unmock", "doMock", "doUnmock", "resetModules", "importActual", "importMock", "hoisted"].every(refused) && ' +
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

begin
  CheckUnresolved('', True, 'Vitest was installed by default');
  CheckUnresolved('{"vitest-compat": "true"}', False, 'Vitest was installed without timers');
  CheckOptIn;
  WriteLn('Embedded module-only Vitest: passed');
end.
