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
  // A thread can get a little more stack than it asks for. On arm64 Darwin,
  // libpthread adds PTHREAD_T_OFFSET (12 KiB) to the requested size, and
  // pthread_get_stacksize_np reports the sum, which the thread may use.
  STACK_SIZE_SLACK = 64 * 1024;

type
  TStackLimitTests = class(TTestSuite)
  private
    procedure TestMainThreadHasItsStackBounds;
    procedure TestEachThreadHasItsOwnLimit;
    procedure TestSmallStackKeepsAQuarterFree;
    procedure TestShallowEntriesAreNotChecked;
    procedure TestRecursionStopsBeforeTheStackEnds;
    procedure Check(const ACondition: Boolean; const AWhat: string;
      const AProbe: TObject);
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
    // What the system reports for the thread's stack, where the test can ask
    // for it (Darwin), for the failure message.
    SystemTop: NativeUInt;
    SystemSize: NativeUInt;
    RequestedSize: SizeUInt;
    constructor Create(const AStackSize: SizeUInt; const ARecurse: Boolean);
    procedure Execute; override;
    function Describe: string;
  end;

{$IFDEF DARWIN}
function pthread_self: Pointer; cdecl; external 'c' name 'pthread_self';
function Pthread_get_stackaddr_np(AThread: Pointer): Pointer; cdecl;
  external 'c' name 'pthread_get_stackaddr_np';
function Pthread_get_stacksize_np(AThread: Pointer): NativeUInt; cdecl;
  external 'c' name 'pthread_get_stacksize_np';
{$ENDIF}

constructor TProbeThread.Create(const AStackSize: SizeUInt;
  const ARecurse: Boolean);
begin
  FRecurse := ARecurse;
  RequestedSize := AStackSize;
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
  {$IFDEF DARWIN}
  SystemTop := NativeUInt(Pthread_get_stackaddr_np(pthread_self));
  SystemSize := Pthread_get_stacksize_np(pthread_self);
  {$ENDIF}
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

function TProbeThread.Describe: string;
var
  Used: Int64;
begin
  Used := 0;
  if SystemTop <> 0 then
    Used := Int64(SystemTop) - Int64(Position);
  Result := Format('requested %d, limit $%x, position $%x, room %d, ' +
    'system top $%x, system size %d, used before the probe %d, depth %d',
    [Int64(RequestedSize), Int64(Limit), Int64(Position),
     Int64(Position) - Int64(Limit), Int64(SystemTop), Int64(SystemSize),
     Used, Depth]);
end;

procedure TStackLimitTests.Check(const ACondition: Boolean;
  const AWhat: string; const AProbe: TObject);
begin
  if not ACondition then
    Fail(AWhat + ' (' + TProbeThread(AProbe).Describe + ')');
  // Counts the assertion.
  Expect<Boolean>(ACondition).ToBe(True);
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
    Check(Probe.Limit <> 0, 'limit found', Probe);
    Check(Probe.Limit <> NativeStackLimit, 'limit differs from the main thread''s', Probe);
    Check(Probe.Limit < Probe.Position, 'limit below the position', Probe);
    Room := Probe.Position - Probe.Limit;
    Check(Room > 3 * MEBIBYTE, 'room above 3 MiB', Probe);
    {$IFNDEF MSWINDOWS}
    // A 4 MiB stack less the reserve, less what the thread has used. Windows
    // takes a thread's stack size as the memory to commit, and reserves the
    // executable's default stack size if that is larger.
    Check(Room <= 4 * MEBIBYTE - NATIVE_STACK_RESERVE + STACK_SIZE_SLACK,
      'room within 4 MiB less the reserve', Probe);
    {$ENDIF}
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
    Check(Probe.Limit <> 0, 'limit found', Probe);
    Room := Probe.Position - Probe.Limit;
    // More than a full reserve would leave.
    Check(Room > 256 * 1024, 'room above 256 KiB', Probe);
    {$IFNDEF MSWINDOWS}
    // Windows reserves at least the executable's default stack size.
    Check(Room <= 384 * 1024 + STACK_SIZE_SLACK,
      'room within three quarters of 512 KiB', Probe);
    {$ENDIF}
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
    Check(Probe.Stopped, 'recursion stopped', Probe);
    Expect<string>(Probe.StoppedBy).ToBe('TGocciaThrowValue');
    // It ran until a reserve's worth of stack was left, not earlier.
    Check(Probe.Depth > (MEBIBYTE - NATIVE_STACK_RESERVE) div
      FRAME_BYTES div 2, 'recursion used most of the stack', Probe);
    {$IFNDEF MSWINDOWS}
    // Windows reserves at least the executable's default stack size.
    Check(Probe.Depth < MEBIBYTE div FRAME_BYTES,
      'recursion stopped within 1 MiB', Probe);
    {$ENDIF}
  finally
    Probe.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TStackLimitTests.Create('Stack limit'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
