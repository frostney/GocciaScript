program Goccia.Profiler.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Profiler,
  Goccia.TestSetup;

type
  TProfilerTests = class(TTestSuite)
  private
    function Profiler: TGocciaProfiler;
    function TotalOf(const AIndex: Integer): Int64;
    function SelfOf(const AIndex: Integer): Int64;
    function StackWeight(const APath: string): Int64;
    procedure TestDirectRecursionCountsOutermostFrameOnce;
    procedure TestMutualRecursionCountsEachFunctionOnce;
    procedure TestSequentialCallsEachAddTheirTime;
    procedure TestReentryBetweenOtherFunctionsCountsOnce;
    procedure TestResetWithFramesOnTheStack;
    procedure TestCollapsedStacksKeepSelfTime;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TProfilerTests.BeforeEach;
begin
  inherited BeforeEach;
  TGocciaProfiler.Initialize;
end;

procedure TProfilerTests.AfterEach;
begin
  TGocciaProfiler.Shutdown;
  inherited AfterEach;
end;

procedure TProfilerTests.SetupTests;
begin
  Test('Direct recursion adds the outermost frame''s time once',
    TestDirectRecursionCountsOutermostFrameOnce);
  Test('Mutual recursion adds each function''s outermost frame once',
    TestMutualRecursionCountsEachFunctionOnce);
  Test('Calls that do not overlap each add their own time',
    TestSequentialCallsEachAddTheirTime);
  Test('Re-entry through another function adds the outer frame only',
    TestReentryBetweenOtherFunctionsCountsOnce);
  Test('A reset with frames on the stack leaves the counters consistent',
    TestResetWithFramesOnTheStack);
  Test('Collapsed stacks keep their self-time weights',
    TestCollapsedStacksKeepSelfTime);
end;

function TProfilerTests.Profiler: TGocciaProfiler;
begin
  Result := TGocciaProfiler.Instance;
end;

function TProfilerTests.TotalOf(const AIndex: Integer): Int64;
begin
  Result := Profiler.GetFunctionProfile(AIndex).TotalTimeNanoseconds;
end;

function TProfilerTests.SelfOf(const AIndex: Integer): Int64;
begin
  Result := Profiler.GetFunctionProfile(AIndex).SelfTimeNanoseconds;
end;

function TProfilerTests.StackWeight(const APath: string): Int64;
begin
  if not Profiler.CollapsedStacks.TryGetValue(APath, Result) then
    Result := -1;
end;

// <module> 0..100 calls f 10..80, which recurses twice: f 20..70, f 30..60.
procedure TProfilerTests.TestDirectRecursionCountsOutermostFrameOnce;
var
  Module, F: Integer;
begin
  Module := Profiler.RegisterTemplate('<module>', 'script.js', 1);
  F := Profiler.RegisterTemplate('f', 'script.js', 1);
  Profiler.PushFunction(Module, 0);
  Profiler.PushFunction(F, 10);
  Profiler.PushFunction(F, 20);
  Profiler.PushFunction(F, 30);
  Profiler.PopFunction(F, 60);
  Profiler.PopFunction(F, 70);
  Profiler.PopFunction(F, 80);
  Profiler.PopFunction(Module, 100);

  Expect<Int64>(TotalOf(F)).ToBe(70);
  Expect<Int64>(SelfOf(F)).ToBe(70);
  Expect<Int64>(Profiler.GetFunctionProfile(F).CallCount).ToBe(3);
  Expect<Integer>(Profiler.GetFunctionProfile(F).ActiveFrames).ToBe(0);
  Expect<Int64>(TotalOf(Module)).ToBe(100);
  Expect<Int64>(SelfOf(Module)).ToBe(30);
  Expect<Int64>(SelfOf(Module) + SelfOf(F)).ToBe(TotalOf(Module));
end;

// a 0..70 -> b 10..60 -> a 20..50 -> b 30..40.
procedure TProfilerTests.TestMutualRecursionCountsEachFunctionOnce;
var
  A, B: Integer;
begin
  A := Profiler.RegisterTemplate('a', 'script.js', 1);
  B := Profiler.RegisterTemplate('b', 'script.js', 2);
  Profiler.PushFunction(A, 0);
  Profiler.PushFunction(B, 10);
  Profiler.PushFunction(A, 20);
  Profiler.PushFunction(B, 30);
  Profiler.PopFunction(B, 40);
  Profiler.PopFunction(A, 50);
  Profiler.PopFunction(B, 60);
  Profiler.PopFunction(A, 70);

  Expect<Int64>(TotalOf(A)).ToBe(70);
  Expect<Int64>(TotalOf(B)).ToBe(50);
  Expect<Int64>(SelfOf(A)).ToBe(40);
  Expect<Int64>(SelfOf(B)).ToBe(30);
  Expect<Int64>(SelfOf(A) + SelfOf(B)).ToBe(TotalOf(A));
end;

// f 0..10, then f 20..35: two outermost activations.
procedure TProfilerTests.TestSequentialCallsEachAddTheirTime;
var
  F: Integer;
begin
  F := Profiler.RegisterTemplate('f', 'script.js', 1);
  Profiler.PushFunction(F, 0);
  Profiler.PopFunction(F, 10);
  Profiler.PushFunction(F, 20);
  Profiler.PopFunction(F, 35);

  Expect<Int64>(TotalOf(F)).ToBe(25);
  Expect<Int64>(SelfOf(F)).ToBe(25);
  Expect<Int64>(Profiler.GetFunctionProfile(F).CallCount).ToBe(2);
end;

// f 0..100 -> callback 10..90 -> f 20..80 -> callback 30..70 -> f 40..60, as
// a recursion through a native callback (arr.map(f)) records it.
procedure TProfilerTests.TestReentryBetweenOtherFunctionsCountsOnce;
var
  F, Callback: Integer;
begin
  F := Profiler.RegisterTemplate('f', 'script.js', 1);
  Callback := Profiler.RegisterTemplate('<arrow>', 'script.js', 2);
  Profiler.PushFunction(F, 0);
  Profiler.PushFunction(Callback, 10);
  Profiler.PushFunction(F, 20);
  Profiler.PushFunction(Callback, 30);
  Profiler.PushFunction(F, 40);
  Profiler.PopFunction(F, 60);
  Profiler.PopFunction(Callback, 70);
  Profiler.PopFunction(F, 80);
  Profiler.PopFunction(Callback, 90);
  Profiler.PopFunction(F, 100);

  Expect<Int64>(TotalOf(F)).ToBe(100);
  Expect<Int64>(TotalOf(Callback)).ToBe(80);
  Expect<Int64>(SelfOf(F) + SelfOf(Callback)).ToBe(100);
end;

// Two frames of f are on the stack when the counts are reset, as with
// BenchmarkRunner's --profile-deterministic; the frames pushed after it are
// counted on their own, and the stale frames' pops find an empty stack.
procedure TProfilerTests.TestResetWithFramesOnTheStack;
var
  F: Integer;
begin
  F := Profiler.RegisterTemplate('f', 'script.js', 1);
  Profiler.PushFunction(F, 0);
  Profiler.PushFunction(F, 5);
  Profiler.ResetCounts;
  Expect<Integer>(Profiler.GetFunctionProfile(F).ActiveFrames).ToBe(0);

  Profiler.PushFunction(F, 10);
  Profiler.PushFunction(F, 12);
  Profiler.PopFunction(F, 15);
  Profiler.PopFunction(F, 20);
  Expect<Int64>(TotalOf(F)).ToBe(10);
  Expect<Int64>(SelfOf(F)).ToBe(10);

  Profiler.PopFunction(F, 30);
  Profiler.PopFunction(F, 40);
  Expect<Int64>(TotalOf(F)).ToBe(10);
  Expect<Integer>(Profiler.GetFunctionProfile(F).ActiveFrames).ToBe(0);
end;

procedure TProfilerTests.TestCollapsedStacksKeepSelfTime;
var
  F: Integer;
begin
  F := Profiler.RegisterTemplate('f', 'script.js', 1);
  Profiler.PushFunction(F, 0);
  Profiler.PushFunction(F, 10);
  Profiler.PushFunction(F, 20);
  Profiler.PopFunction(F, 50);
  Profiler.PopFunction(F, 70);
  Profiler.PopFunction(F, 100);

  Expect<Int64>(StackWeight('f;f;f')).ToBe(30);
  Expect<Int64>(StackWeight('f;f')).ToBe(30);
  Expect<Int64>(StackWeight('f')).ToBe(40);
  Expect<Int64>(TotalOf(F)).ToBe(100);
end;

begin
  TestRunnerProgram.AddSuite(TProfilerTests.Create('Goccia profiler'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
