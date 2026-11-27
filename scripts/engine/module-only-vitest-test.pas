program ModuleOnlyVitestTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Embed;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckDefault;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.Run('vitest-default.mjs', 'import "vitest";');
    Check(not RunResult.Ok, 'Vitest was installed by default');
  finally
    Engine.Free;
  end;
end;

procedure CheckOptIn;
const
  Source = 'import { describe, expect, test } from "vitest";' +
    'import { runTests } from "goccia:test";' +
    'describe("embedded", () => {' +
    '  test("async assertion", async () => expect(await Promise.resolve(42)).toBe(42));' +
    '  test("failed assertion", () => expect(1).toBe(2));' +
    '});' +
    'const summary = runTests({ showTestResults: false });' +
    'summary.passed === 1 && summary.failed === 1 && summary.totalRunTests === 2 &&' +
    '  summary.failedTests[0].includes("failed assertion") && typeof globalThis.describe === "undefined";';
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
      'expected a synchronous test report and only imported helpers: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;
end;

begin
  CheckDefault;
  CheckOptIn;
  WriteLn('Embedded module-only Vitest: passed');
end.
