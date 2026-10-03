{ Pins the per-thread shim program cache in Goccia.Shims.

  Every engine used to lex and parse each shim it loaded. A parsed AST is never
  released — the functions a shim defines point into it, and
  TGocciaProgram.Free frees only the program node — so each engine leaked a
  full shim AST. The test runner builds one engine per file and the testing
  library loads the Date shim in every one, which made the interpreted suite
  grow its heap by about 1.1 MB per file on 64-bit and run a 32-bit process out
  of address space. }

program Goccia.Shims.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.Shims,
  Goccia.TestSetup,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.Primitives;

type
  TTestShims = class(TTestSuite)
  private
    function RunNumber(const ASource: string): Double;
    function RunString(const ASource: string): string;
    function LiveHeapBytes: Int64;
  public
    procedure SetupTests; override;

    procedure TestEnginesDoNotGrowTheHeapByAShimProgramEach;
    procedure TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm;
    procedure TestEnginesStillLoadShimsAfterTheCacheIsReleased;
    procedure TestCacheReleaseIsRegistered;
    procedure TestNoDefaultShimContainsATemplateLiteral;
  end;

procedure TTestShims.SetupTests;
begin
  Test('engines built one after another do not grow the heap by a shim program each',
    TestEnginesDoNotGrowTheHeapByAShimProgramEach);
  Test('each engine evaluates the shared Date shim program into its own realm',
    TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm);
  Test('engines still load shims after the thread releases its cached programs',
    TestEnginesStillLoadShimsAfterTheCacheIsReleased);
  Test('the cached shim programs are released by the thread cleanup registry',
    TestCacheReleaseIsRegistered);
  Test('no default shim contains a template literal',
    TestNoDefaultShimContainsATemplateLiteral);
end;

function TTestShims.RunNumber(const ASource: string): Double;
begin
  Result := (TGocciaEngine.RunScript(ASource, '<shims-test>').Result as
    TGocciaNumberLiteralValue).Value;
end;

function TTestShims.RunString(const ASource: string): string;
begin
  Result := (TGocciaEngine.RunScript(ASource, '<shims-test>').Result as
    TGocciaStringLiteralValue).Value;
end;

function TTestShims.LiveHeapBytes: Int64;
begin
  TGarbageCollector.Instance.Collect;
  Result := Int64(GetHeapStatus.TotalAllocated);
end;

procedure TTestShims.TestEnginesDoNotGrowTheHeapByAShimProgramEach;
const
  WARM_UP_ENGINES = 3;
  MEASURED_ENGINES = 10;
  { The Date shim's AST alone is about 700 KB on 64-bit, and an engine also
    runs the eager Object.prototype shims, so the per-engine leak this pins
    was well above 800 KB. What an engine still leaves behind is a few KB of
    allocator and cache bookkeeping. }
  MAX_GROWTH_PER_ENGINE_BYTES = 64 * 1024;
  SOURCE = 'new Date(Date.UTC(2020, 0, 2)).getUTCDate();';
var
  I: Integer;
  Before, After: Int64;
begin
  for I := 1 to WARM_UP_ENGINES do
    Expect<Double>(RunNumber(SOURCE)).ToBe(2);
  Before := LiveHeapBytes;
  for I := 1 to MEASURED_ENGINES do
    Expect<Double>(RunNumber(SOURCE)).ToBe(2);
  After := LiveHeapBytes;
  Expect<Boolean>((After - Before) div MEASURED_ENGINES <
    MAX_GROWTH_PER_ENGINE_BYTES).ToBe(True);
end;

procedure TTestShims.TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm;
begin
  Expect<Double>(RunNumber(
    'Date.prototype.poisoned = 1;' +
    'Date.poisoned = 2;' +
    'Date.prototype.poisoned + Date.poisoned;')).ToBe(3);
  { The second engine evaluates the program the first one parsed. Its Date,
    and Date.prototype, must be new objects in its own realm. }
  Expect<string>(RunString(
    'typeof Date.prototype.poisoned + "," + typeof Date.poisoned + "," +' +
    'String(new Date(0).getTime());')).ToBe('undefined,undefined,0');
  { The eager shims install methods on each realm's Object.prototype. }
  Expect<string>(RunString(
    'typeof Object.prototype.__defineGetter__;')).ToBe('function');
end;

procedure TTestShims.TestEnginesStillLoadShimsAfterTheCacheIsReleased;
begin
  Expect<Double>(RunNumber('new Date(86400000).getUTCDate();')).ToBe(2);
  ReleaseShimProgramCache;
  Expect<Double>(RunNumber('new Date(86400000).getUTCDate();')).ToBe(2);
  { Releasing an already-empty cache is a no-op. }
  ReleaseShimProgramCache;
  ReleaseShimProgramCache;
  Expect<Double>(RunNumber('new Date(0).getUTCFullYear();')).ToBe(1970);
end;

procedure TTestShims.TestCacheReleaseIsRegistered;
begin
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ReleaseShimProgramCache))
    .ToBe(True);
end;

procedure TTestShims.TestNoDefaultShimContainsATemplateLiteral;
var
  I: Integer;
begin
  { A tagged template caches its template object on the AST node, and the
    template object belongs to one realm. A shared shim program must not
    carry one from an earlier engine into a later one. }
  for I := 0 to DefaultShimCount - 1 do
    Expect<Integer>(Pos('`', DefaultShim(I).Source)).ToBe(0);
end;

begin
  TestRunnerProgram.AddSuite(TTestShims.Create('Shims'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
