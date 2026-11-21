program Goccia.StackLimit.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF LINUX}BaseUnix, UnixType,{$ENDIF}
  Classes,
  SyncObjs,
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
    procedure TestSmallStackBetweenOtherStacksKeepsAQuarterFree;
    {$IFDEF LINUX}
    procedure TestStackInsideALargerMappingKeepsAQuarterFree;
    {$ENDIF}
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
    // Set, when assigned, once the thread runs; the thread then waits for
    // Go before it looks its limit up.
    FRunning: TEvent;
    FGo: TEvent;
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
    constructor Create(const AStackSize: SizeUInt; const ARecurse: Boolean;
      const ARunning: TEvent = nil; const AGo: TEvent = nil);
    procedure Execute; override;
    function Describe: string;
  end;

  // Holds a stack of a given size until Release is set, allocating nothing,
  // so that nothing of its own is mapped next to its stack.
  THeldThread = class(TThread)
  private
    FRunning: TEvent;
    FRelease: TEvent;
  public
    constructor Create(const AStackSize: SizeUInt;
      const ARunning, ARelease: TEvent);
    procedure Execute; override;
  end;

{$IFDEF DARWIN}
function pthread_self: Pointer; cdecl; external 'c' name 'pthread_self';
function Pthread_get_stackaddr_np(AThread: Pointer): Pointer; cdecl;
  external 'c' name 'pthread_get_stackaddr_np';
function Pthread_get_stacksize_np(AThread: Pointer): NativeUInt; cdecl;
  external 'c' name 'pthread_get_stacksize_np';
{$ENDIF}

{$IFDEF LINUX}
type
  // What a thread started on a stack of the test's own records.
  TOwnStackProbe = record
    Limit: NativeUInt;
    Position: NativeUInt;
  end;
  POwnStackProbe = ^TOwnStackProbe;

function Pthread_attr_init(AAttr: Pointer): cint; cdecl;
  external 'pthread' name 'pthread_attr_init';
function Pthread_attr_setstack(AAttr: Pointer; AStackAddr: Pointer;
  AStackSize: size_t): cint; cdecl;
  external 'pthread' name 'pthread_attr_setstack';
function Pthread_attr_destroy(AAttr: Pointer): cint; cdecl;
  external 'pthread' name 'pthread_attr_destroy';
function pthread_create(AThread: Pointer; AAttr: Pointer; AStart: Pointer;
  AArg: Pointer): cint; cdecl; external 'pthread' name 'pthread_create';
function Pthread_join(AThread: pthread_t; AResult: PPointer): cint; cdecl;
  external 'pthread' name 'pthread_join';

function ProbeOwnStack(AArg: Pointer): Pointer; cdecl;
begin
  POwnStackProbe(AArg)^.Limit := NativeStackLimit;
  POwnStackProbe(AArg)^.Position := NativeStackPosition;
  Result := nil;
end;

// Runs ProbeOwnStack on a thread whose stack is ASize bytes at AStack.
function ProbeOnOwnStack(const AStack: Pointer; const ASize: NativeUInt;
  out AProbe: TOwnStackProbe): Boolean;
var
  Attr: pthread_attr_t;
  Thread: pthread_t;
begin
  FillChar(AProbe, SizeOf(AProbe), 0);
  Result := False;
  if pthread_attr_init(@Attr) <> 0 then
    Exit;
  try
    Result := (pthread_attr_setstack(@Attr, AStack, ASize) = 0) and
      (pthread_create(@Thread, @Attr, @ProbeOwnStack, @AProbe) = 0);
  finally
    pthread_attr_destroy(@Attr);
  end;
  if Result then
    pthread_join(Thread, nil);
end;
{$ENDIF}

constructor TProbeThread.Create(const AStackSize: SizeUInt;
  const ARecurse: Boolean; const ARunning: TEvent = nil;
  const AGo: TEvent = nil);
begin
  FRecurse := ARecurse;
  FRunning := ARunning;
  FGo := AGo;
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
  if Assigned(FRunning) then
    FRunning.SetEvent;
  if Assigned(FGo) then
    FGo.WaitFor(INFINITE);
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

constructor THeldThread.Create(const AStackSize: SizeUInt;
  const ARunning, ARelease: TEvent);
begin
  FRunning := ARunning;
  FRelease := ARelease;
  inherited Create(True, AStackSize);
end;

procedure THeldThread.Execute;
begin
  FRunning.SetEvent;
  FRelease.WaitFor(INFINITE);
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
  Test('A small stack between two other threads'' stacks keeps a quarter ' +
    'of it free', TestSmallStackBetweenOtherStacksKeepsAQuarterFree);
  {$IFDEF LINUX}
  Test('A stack inside a larger mapping keeps a quarter of it free',
    TestStackInsideALargerMappingKeepsAQuarterFree);
  {$ENDIF}
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

// With glibc 2.42 and later on Linux 6.13 and later, a thread's guard page is
// inside its stack's mapping, and the kernel merges thread stacks that lie
// next to each other into one mapping. Each thread maps a few small blocks of
// its own when it starts, which would otherwise lie between the stacks; the
// holes left first in the address space take them, so the probe's stack lies
// directly below one thread's stack and directly above another's. Where the
// stacks do not merge, the test checks the same bounds.
procedure TStackLimitTests.TestSmallStackBetweenOtherStacksKeepsAQuarterFree;
const
  HOLE_COUNT = 8;
  HOLE_BYTES = 64 * 1024;
  // glibc reuses the stack of a thread that has ended for a new thread that
  // asks for no more than it and at least a quarter of it. No earlier test
  // asks for these sizes, so each of these threads gets a stack mapped for it.
  PROBE_STACK_BYTES = 640 * 1024;
  HELD_STACK_BYTES = 8 * MEBIBYTE;
var
  {$IFDEF LINUX}
  Holes: array[0..HOLE_COUNT - 1] of Pointer;
  {$ENDIF}
  I: Integer;
  Above, Below: THeldThread;
  Probe: TProbeThread;
  AboveRunning, BelowRunning, ProbeRunning, Go, Release: TEvent;
  Room: NativeUInt;
begin
  {$IFDEF LINUX}
  // Every other block is unmapped again, so each leaves a hole of its own.
  for I := 0 to HOLE_COUNT - 1 do
    Holes[I] := Fpmmap(nil, HOLE_BYTES, PROT_READ or PROT_WRITE,
      MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  for I := 0 to HOLE_COUNT - 1 do
    if (I mod 2 = 0) and (Holes[I] <> MAP_FAILED) then
      Fpmunmap(Holes[I], HOLE_BYTES);
  {$ENDIF}
  AboveRunning := TEvent.Create(nil, True, False, '');
  BelowRunning := TEvent.Create(nil, True, False, '');
  ProbeRunning := TEvent.Create(nil, True, False, '');
  Go := TEvent.Create(nil, True, False, '');
  Release := TEvent.Create(nil, True, False, '');
  Above := THeldThread.Create(HELD_STACK_BYTES, AboveRunning, Release);
  Probe := TProbeThread.Create(PROBE_STACK_BYTES, False, ProbeRunning, Go);
  Below := THeldThread.Create(HELD_STACK_BYTES, BelowRunning, Release);
  try
    Above.Start;
    AboveRunning.WaitFor(INFINITE);
    Probe.Start;
    ProbeRunning.WaitFor(INFINITE);
    Below.Start;
    BelowRunning.WaitFor(INFINITE);
    Go.SetEvent;
    Probe.WaitFor;
    Check(Probe.Limit <> 0, 'limit found', Probe);
    Room := Probe.Position - Probe.Limit;
    // More than a full reserve would leave.
    Check(Room > PROBE_STACK_BYTES - NATIVE_STACK_RESERVE,
      'room above 640 KiB less a full reserve', Probe);
    {$IFNDEF MSWINDOWS}
    Check(Room <= PROBE_STACK_BYTES div 4 * 3 + STACK_SIZE_SLACK,
      'room within three quarters of 640 KiB', Probe);
    {$ENDIF}
  finally
    Release.SetEvent;
    Above.WaitFor;
    Below.WaitFor;
    Above.Free;
    Below.Free;
    Probe.Free;
    AboveRunning.Free;
    BelowRunning.Free;
    ProbeRunning.Free;
    Go.Free;
    Release.Free;
    {$IFDEF LINUX}
    for I := 0 to HOLE_COUNT - 1 do
      if (I mod 2 = 1) and (Holes[I] <> MAP_FAILED) then
        Fpmunmap(Holes[I], HOLE_BYTES);
    {$ENDIF}
  end;
end;

{$IFDEF LINUX}
// A thread whose stack is part of a larger mapping, as a thread stack is once
// the kernel merges it with its neighbours: here the middle third of one
// mapping the test owns, so /proc/self/maps shows three times the stack
// wherever the test runs. Then the same stack with its bottom protected, as a
// host can do to a stack it supplies: the thread library still reports the
// whole buffer.
procedure TStackLimitTests.TestStackInsideALargerMappingKeepsAQuarterFree;
const
  STACK_BYTES = 640 * 1024;
  PROTECTED_BYTES = 256 * 1024;
var
  Mapping, Stack: PByte;
  Probe: TOwnStackProbe;
  Starter: TProbeThread;
  Room: NativeUInt;
  Started: Boolean;
begin
  // The RTL gives each thread its own threadvars once a thread of its own
  // has started; a thread it did not start then gets them on first use.
  if not IsMultiThread then
  begin
    Starter := TProbeThread.Create(STACK_BYTES, False);
    Starter.Start;
    Starter.WaitFor;
    Starter.Free;
  end;
  Mapping := Fpmmap(nil, 3 * STACK_BYTES, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  Expect<Boolean>(Pointer(Mapping) <> MAP_FAILED).ToBe(True);
  if Pointer(Mapping) = MAP_FAILED then
    Exit;
  Stack := Mapping + STACK_BYTES;
  try
    Started := ProbeOnOwnStack(Stack, STACK_BYTES, Probe);
    Expect<Boolean>(Started).ToBe(True);
    if not Started then
      Exit;
    Room := Probe.Position - Probe.Limit;
    if (Room <= STACK_BYTES - NATIVE_STACK_RESERVE) or
       (Room > STACK_BYTES div 4 * 3) then
      Fail(Format('room of a quarter of a 640 KiB stack (stack $%x, ' +
        'limit $%x, position $%x)', [NativeUInt(Stack), Probe.Limit,
        Probe.Position]));
    // The limit is in the thread's own stack, a quarter above its bottom.
    Expect<Boolean>(Probe.Limit = NativeUInt(Stack) + STACK_BYTES div 4).
      ToBe(True);

    // A quarter of what is left above the protected pages.
    Expect<Integer>(Fpmprotect(Stack, PROTECTED_BYTES, PROT_NONE)).ToBe(0);
    Started := ProbeOnOwnStack(Stack, STACK_BYTES, Probe);
    Expect<Boolean>(Started).ToBe(True);
    if not Started then
      Exit;
    if Probe.Limit <> NativeUInt(Stack) + PROTECTED_BYTES +
       (STACK_BYTES - PROTECTED_BYTES) div 4 then
      Fail(Format('limit a quarter above the protected pages (stack $%x, ' +
        'limit $%x, position $%x)', [NativeUInt(Stack), Probe.Limit,
        Probe.Position]));
    Expect<Boolean>(Probe.Limit = NativeUInt(Stack) + PROTECTED_BYTES +
      (STACK_BYTES - PROTECTED_BYTES) div 4).ToBe(True);
  finally
    Fpmunmap(Mapping, 3 * STACK_BYTES);
  end;
end;
{$ENDIF}

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
