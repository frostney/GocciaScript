program EmbeddingInstructionCountsTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.Instructions,
  Goccia.Profiler,
  Goccia.VM;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function MeasureEntry(const ASource: UnicodeString; ACap: Int64; AMeasure: Boolean): TGocciaRunResult;
var
  Engine: TGocciaEngine;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.PrepareHost(ErrorText), 'host setup failed: ' + ErrorText);
    Engine.MaxInstructions := ACap;
    Engine.MeasureInstructions := AMeasure;
    Engine.ResultForm := grfString;
    Result := Engine.Run('measure.mjs', ASource);
  finally
    Engine.Free;
  end;
end;

const
  Source = 'const cells = await Promise.resolve().then(() => new Array(256).fill(42)); cells[13];';

procedure CheckNativeRefusalCount;
var
  Engine: TGocciaEngine;
  Count: QWord;
begin
  Engine := TGocciaEngine.Create;
  try
    LimitsArm(0, 1);
    Check(PollInterrupt, 'the allowed charge was refused');
    Check(not PollInterrupt, 'a charge after the cap was allowed');
    Check(not PollInterrupt, 'a later cleanup charge was allowed');
    Check(ReadInstructionCount(Count) and (Count = 2), 'cleanup changed the exhausted count');
  finally
    Engine.Free;
  end;
end;

procedure CheckFailureAndReuse;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.MaxInstructions := 3;
    RunResult := Engine.Run('count-failure.js', 'throw 1n;');
    Check((RunResult.Aborted = gaInstructionLimit) and RunResult.HasInstructionCount and
      (RunResult.InstructionCount = 4), 'a charge refused during throw formatting was lost');
    Check(Pos('Execution exceeded instruction limit of 3', RunResult.ErrorMessage) > 0,
      'the refused formatting charge did not report the limit');

    Engine.MaxInstructions := 0;
    Engine.MeasureInstructions := True;
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('count-reuse.mjs', Source);
    Check(RunResult.Ok and (RunResult.ResultText = '42') and RunResult.HasInstructionCount and
      (RunResult.InstructionCount >= 256), 'measurement did not restart after refusal');

    Engine.MeasureInstructions := False;
    RunResult := Engine.Run('count-unmeasured-reuse.mjs', Source);
    Check(RunResult.Ok and not RunResult.HasInstructionCount and (RunResult.InstructionCount = 0),
      'measurement carried into the next unmeasured run');
  finally
    Engine.Free;
  end;
end;

procedure CheckSuiteLimitScope;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Count: QWord;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.TestMode := True;
    Engine.TimeoutMs := 5000;
    Engine.MeasureInstructions := True;
    RunResult := Engine.Run('count-suite.js', '1;');
    Check(RunResult.Ok and RunResult.HasInstructionCount, 'suite entry measurement was missing');
    Check(not ReadInstructionCount(Count) and (LimitRemainingMs > 0), 'measurement outlived the entry');

    Engine.MaxInstructions := 500;
    RunResult := Engine.Run('count-capped-suite.js', '1;');
    Check(RunResult.Ok and ReadInstructionCount(Count) and (Count = RunResult.InstructionCount) and
      (LimitRemainingMs > 0), 'the suite lost its real instruction or time limit');
  finally
    Engine.Free;
  end;
end;

function CountBenchmarkXors(AMeasure: Boolean): Int64;
var
  Engine: TGocciaEngine;
  Profiler: TGocciaProfiler;
  RunResult: TGocciaRunResult;
  Entries: TGocciaBenchEntries;
  Total: Integer;
  Duration, BeforeSuite: Int64;
  ErrorText: UnicodeString;
  Aborted: TGocciaAbort;
begin
  Profiler := TGocciaProfiler.Create;
  Engine := TGocciaEngine.Create;
  try
    TGocciaProfiler.Adopt(Profiler);
    Profiler.Enabled := True;
    Profiler.Mode := [pmOpcodes];
    Engine.BenchMode := True;
    Engine.ProfileOpcodes := True;
    Engine.MeasureInstructions := AMeasure;
    Check(Engine.PrepareHost(ErrorText), 'benchmark host setup failed: ' + ErrorText);
    Check(InstallBenchmarkAPI(ErrorText), 'benchmark API setup failed: ' + ErrorText);
    RunResult := Engine.Run('profile-bench.mjs',
      'import { bench } from "goccia:microbench";' +
      'bench("opcode count", () => new Array(64).fill(7).forEach(value => value ^ 3));');
    Check(RunResult.Ok, 'benchmark entry failed: ' + RunResult.ErrorMessage);
    BeforeSuite := Profiler.GetOpcodeCount(Ord(opBitXorI));
    Check(Engine.RunBenchmarks(True, Entries, Total, Duration, ErrorText, Aborted), 'benchmark failed: ' + ErrorText);
    Check(Total = 1, 'expected one executed benchmark');
    Result := Profiler.GetOpcodeCount(Ord(opBitXorI)) - BeforeSuite;
  finally
    TGocciaProfiler.Adopt(nil);
    Engine.Free;
    Profiler.Free;
  end;
end;

procedure CheckBenchmarkProfiling;
var
  Ordinary, Measured: Int64;
begin
  Ordinary := CountBenchmarkXors(False);
  Measured := CountBenchmarkXors(True);
  Check((Ordinary = 64) and (Measured = Ordinary),
    'benchmark profile lost opcodes: ordinary ' + IntToStr(Ordinary) + ', measured ' + IntToStr(Measured));
end;

var
  Unmeasured, Uncapped, Capped, Refused: TGocciaRunResult;
begin
  Unmeasured := MeasureEntry(Source, 0, False);
  Check(Unmeasured.Ok and (Unmeasured.ResultText = '42'), 'ordinary uncapped entry failed');
  Check(not Unmeasured.HasInstructionCount and (Unmeasured.InstructionCount = 0), 'unrequested count was reported');

  Uncapped := MeasureEntry(Source, 0, True);
  Capped := MeasureEntry(Source, 1000000, False);
  Check(Uncapped.Ok and (Uncapped.ResultText = '42') and Capped.Ok and (Capped.ResultText = '42'),
    'counting changed the result');
  Check(Uncapped.HasInstructionCount and Capped.HasInstructionCount, 'requested count was missing');
  Check((Uncapped.InstructionCount >= 256) and (Uncapped.InstructionCount = Capped.InstructionCount),
    'dispatch and native work charges differed between capped and uncapped execution');

  Refused := MeasureEntry(Source, 10, False);
  Check((Refused.Aborted = gaInstructionLimit) and Refused.HasInstructionCount and
    (Refused.InstructionCount = 11), 'exhaustion did not report the refused charge');
  CheckNativeRefusalCount;
  CheckFailureAndReuse;
  CheckSuiteLimitScope;
  CheckBenchmarkProfiling;
  WriteLn('Embedding instruction counts: passed');
end.
