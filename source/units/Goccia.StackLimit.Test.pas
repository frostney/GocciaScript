program Goccia.StackLimit.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.StackLimit,
  Goccia.TestSetup;

const
  MEBIBYTE = 1024 * 1024;
  // Bigger than the RTL's own stack check margin, so that the guard is what
  // stops the recursion below in a development build too.
  FRAME_BYTES = 2048;

type
  TStackLimitTests = class(TTestSuite)
  private
    procedure TestMainThreadHasItsStackBounds;
    procedure TestEachThreadHasItsOwnLimit;
    procedure TestSmallStackKeepsAQuarterFree;
    procedure TestShallowEntriesAreNotChecked;
    procedure TestRecursionStopsBeforeTheStackEnds;
  public
    procedure SetupTests; override;
  end;

  // Records NativeStackLimit and NativeStackPosition on a thread of a given
  // stack size, and optionally recurses until CheckNativeStackHeadroom stops
  // it.
  TProbeThread = class(TThread)
  private
    FRecurse: Boolean;
    CachedLimit: NativeUInt;
    procedure Recurse(const ADepth: Integer);
  public
    Limit: NativeUInt;
    Position: NativeUInt;
    Depth: Integer;
    Stopped: Boolean;
    StoppedBy: string;
    constructor Create(const AStackSize: SizeUInt; const ARecurse: Boolean);
    procedure Execute; override;
  end;

constructor TProbeThread.Create(const AStackSize: SizeUInt;
  const ARecurse: Boolean);
begin
  FRecurse := ARecurse;
  inherited Create(True, AStackSize);
end;

procedure TProbeThread.Recurse(const ADepth: Integer);
var
  Frame: array[0..FRAME_BYTES - 1] of Byte;
begin
  FillChar(Frame, SizeOf(Frame), Byte(ADepth));
  Depth := ADepth;
  CheckNativeStackHeadroom(ADepth, CachedLimit);
  // Reading the frame keeps the compiler from dropping it.
  if Frame[ADepth mod FRAME_BYTES] = Byte(ADepth) then
    Recurse(ADepth + 1);
end;

procedure TProbeThread.Execute;
begin
  Limit := NativeStackLimit;
  Position := NativeStackPosition;
  CachedLimit := NATIVE_STACK_LIMIT_UNSET;
  if not FRecurse then
    Exit;
  try
    Recurse(1);
  except
    on E: Exception do
    begin
      Stopped := True;
      StoppedBy := E.ClassName;
    end;
  end;
end;

procedure TStackLimitTests.SetupTests;
begin
  Test('The main thread finds the bounds of its stack',
    TestMainThreadHasItsStackBounds);
  Test('Each thread finds the bounds of its own stack',
    TestEachThreadHasItsOwnLimit);
  Test('A stack smaller than four reserves keeps a quarter of it free',
    TestSmallStackKeepsAQuarterFree);
  Test('Native re-entries nested no deeper than the unchecked depth pass',
    TestShallowEntriesAreNotChecked);
  Test('Recursion is stopped before it reaches the end of the stack',
    TestRecursionStopsBeforeTheStackEnds);
end;

procedure TStackLimitTests.TestMainThreadHasItsStackBounds;
var
  Limit, Position: NativeUInt;
begin
  Limit := NativeStackLimit;
  Position := NativeStackPosition;
  Expect<Boolean>(Limit <> 0).ToBe(True);
  Expect<Boolean>(Limit < Position).ToBe(True);
  // Every supported platform gives its main thread at least 1 MiB.
  Expect<Boolean>(Position - Limit > MEBIBYTE - NATIVE_STACK_RESERVE).ToBe(True);
  // Found once and kept.
  Expect<Boolean>(NativeStackLimit = Limit).ToBe(True);
end;

procedure TStackLimitTests.TestEachThreadHasItsOwnLimit;
var
  Probe: TProbeThread;
  Room: NativeUInt;
begin
  Probe := TProbeThread.Create(4 * MEBIBYTE, False);
  try
    Probe.Start;
    Probe.WaitFor;
    Expect<Boolean>(Probe.Limit <> 0).ToBe(True);
    Expect<Boolean>(Probe.Limit <> NativeStackLimit).ToBe(True);
    Expect<Boolean>(Probe.Limit < Probe.Position).ToBe(True);
    // A 4 MiB stack less the reserve, less what the thread has used.
    Room := Probe.Position - Probe.Limit;
    Expect<Boolean>(Room <= 4 * MEBIBYTE - NATIVE_STACK_RESERVE).ToBe(True);
    Expect<Boolean>(Room > 3 * MEBIBYTE).ToBe(True);
  finally
    Probe.Free;
  end;
end;

procedure TStackLimitTests.TestSmallStackKeepsAQuarterFree;
var
  Probe: TProbeThread;
  Room: NativeUInt;
begin
  Probe := TProbeThread.Create(512 * 1024, False);
  try
    Probe.Start;
    Probe.WaitFor;
    Expect<Boolean>(Probe.Limit <> 0).ToBe(True);
    Room := Probe.Position - Probe.Limit;
    Expect<Boolean>(Room <= 384 * 1024).ToBe(True);
    Expect<Boolean>(Room > 256 * 1024).ToBe(True);
  finally
    Probe.Free;
  end;
end;

procedure TStackLimitTests.TestShallowEntriesAreNotChecked;
var
  Depth: Integer;
  Limit: NativeUInt;
  Passed: Boolean;
begin
  Limit := NATIVE_STACK_LIMIT_UNSET;
  Passed := False;
  try
    for Depth := 0 to NATIVE_REENTRY_UNCHECKED_DEPTH do
      CheckNativeStackHeadroom(Depth, Limit);
    // Nothing nested deeply enough to need the limit yet.
    Expect<Boolean>(Limit = NATIVE_STACK_LIMIT_UNSET).ToBe(True);
    // Far from the end of the main thread's stack, a checked depth passes
    // too, even past the count used where the bounds are unknown.
    CheckNativeStackHeadroom(NATIVE_REENTRY_UNCHECKED_DEPTH + 1, Limit);
    Expect<Boolean>(Limit = NativeStackLimit).ToBe(True);
    CheckNativeStackHeadroom(MAX_NATIVE_REENTRY_DEPTH + 1, Limit);
    Passed := True;
  except
    on Exception do
      Passed := False;
  end;
  Expect<Boolean>(Passed).ToBe(True);
end;

procedure TStackLimitTests.TestRecursionStopsBeforeTheStackEnds;
var
  Probe: TProbeThread;
begin
  // Without the check this recursion overflows a 1 MiB stack after about
  // 500 levels.
  Probe := TProbeThread.Create(MEBIBYTE, True);
  try
    Probe.Start;
    Probe.WaitFor;
    Expect<Boolean>(Probe.Stopped).ToBe(True);
    Expect<string>(Probe.StoppedBy).ToBe('TGocciaThrowValue');
    // It ran until a reserve's worth of stack was left, not earlier.
    Expect<Boolean>(Probe.Depth > (MEBIBYTE - NATIVE_STACK_RESERVE) div
      FRAME_BYTES div 2).ToBe(True);
    Expect<Boolean>(Probe.Depth < MEBIBYTE div FRAME_BYTES).ToBe(True);
  finally
    Probe.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TStackLimitTests.Create('Stack limit'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
