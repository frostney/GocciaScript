program Goccia.Profiler.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Profiler,
  Goccia.TestSetup;

type
  TClockedProfiler = class(TGocciaProfiler)
  private
    FSample: Integer;
  protected
    function ReadTimestamp: Int64; override;
  end;

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
    procedure TestBookkeepingIsExcluded;
    procedure TestCollapsedAliasesAndRefresh;
    procedure TestIndependentEntriesHaveNoOpcodePair;
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
  Test('Registration, pushes and pops are excluded from active frames',
    TestBookkeepingIsExcluded);
  Test('Collapsed aliases merge and a later return refreshes the export',
    TestCollapsedAliasesAndRefresh);
  Test('Independent entries do not invent an opcode pair',
    TestIndependentEntriesHaveNoOpcodePair);
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

function TClockedProfiler.ReadTimestamp: Int64;
const
  // Registration, module entry, child registration, child entry, child
  // return, module return. Only the gaps between these pairs are user time.
  SAMPLES: array[0..11] of Int64 =
    (0, 10, 10, 30, 50, 90, 90, 120, 170, 200, 240, 300);
begin
  Result := SAMPLES[FSample];
  Inc(FSample);
end;

procedure TProfilerTests.TestBookkeepingIsExcluded;
var
  Clocked: TClockedProfiler;
  Module, Child: Integer;
begin
  Clocked := TClockedProfiler.Create;
  try
    Module := Clocked.RegisterTemplate('<module>', 'script.js', 1);
    Clocked.PushFunction(Module);
    Child := Clocked.RegisterTemplate('child', 'script.js', 2);
    Clocked.PushFunction(Child);
    Clocked.PopFunction(Child);
    Clocked.PopFunction(Module);
    Expect<Int64>(Clocked.GetFunctionProfile(Child).SelfTimeNanoseconds).ToBe(50);
    Expect<Int64>(Clocked.GetFunctionProfile(Module).SelfTimeNanoseconds).ToBe(60);
    Expect<Int64>(Clocked.GetFunctionProfile(Module).TotalTimeNanoseconds).ToBe(110);
  finally
    Clocked.Free;
  end;
end;

procedure TProfilerTests.TestCollapsedAliasesAndRefresh;
var
  Parent, Child, Alias, Anonymous: Integer;
begin
  Parent := Profiler.RegisterTemplate('párent', 'a.js', 1);
  Child := Profiler.RegisterTemplate('child', 'a.js', 2);
  Alias := Profiler.RegisterTemplate('child', 'b.js', 2);
  Anonymous := Profiler.RegisterTemplate('', 'a.js', 3);
  Profiler.PushFunction(Parent, 0);
  Profiler.PushFunction(Child, 10);
  Profiler.PopFunction(Child, 30);
  Expect<Int64>(StackWeight('párent;child')).ToBe(20);
  Profiler.PushFunction(Alias, 40);
  Profiler.PopFunction(Alias, 70);
  Profiler.PushFunction(Anonymous, 70);
  Profiler.PopFunction(Anonymous, 80);
  Profiler.PopFunction(Parent, 100);
  Expect<Integer>(Profiler.CollapsedStacks.Count).ToBe(3);
  Expect<string>(Profiler.CollapsedStacks.EntryAt(0).Key).ToBe('párent;child');
  Expect<string>(Profiler.CollapsedStacks.EntryAt(1).Key).ToBe('párent;<anonymous>');
  Expect<string>(Profiler.CollapsedStacks.EntryAt(2).Key).ToBe('párent');
  Expect<Int64>(StackWeight('párent;child')).ToBe(50);
  Expect<Int64>(StackWeight('párent;<anonymous>')).ToBe(10);
  Expect<Int64>(StackWeight('párent')).ToBe(40);
  Profiler.ResetCounts;
  Expect<Integer>(Profiler.CollapsedStacks.Count).ToBe(0);
  Profiler.PushFunction(Child, 0);
  Profiler.PopFunction(Child, 5);
  Expect<Int64>(StackWeight('child')).ToBe(5);
  Expect<Integer>(Profiler.CollapsedStacks.Count).ToBe(1);
end;

procedure TProfilerTests.TestIndependentEntriesHaveNoOpcodePair;
begin
  Profiler.RecordOpcode(1);
  Profiler.RecordOpcode(2);
  Profiler.BeginEntry;
  Profiler.RecordOpcode(3);
  Profiler.RecordOpcode(4);
  Expect<Int64>(Profiler.GetOpcodePairCount(1, 2)).ToBe(1);
  Expect<Int64>(Profiler.GetOpcodePairCount(2, 3)).ToBe(0);
  Expect<Int64>(Profiler.GetOpcodePairCount(3, 4)).ToBe(1);
  Expect<Int64>(Profiler.GetOpcodeCount(3)).ToBe(1);
end;

begin
  TestRunnerProgram.AddSuite(TProfilerTests.Create('Goccia profiler'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
