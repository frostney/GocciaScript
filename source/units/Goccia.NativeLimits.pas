unit Goccia.NativeLimits;

{$I Goccia.inc}

interface

uses
  SysUtils;

const
  MAX_NATIVE_DATA_DEPTH = 256;
  // The most one native allocation may ask for on this target. FPC works out
  // a dynamic array's or string's size, its header included, in SizeInt, so
  // on a 32-bit target a larger byte count wraps to a small allocation
  // instead of failing. The margin covers the header.
  MAX_NATIVE_ALLOCATION_BYTES = Int64(High(SizeInt)) - 65536;

procedure EnterNativeDataDepth(const AOperation: string = '');
procedure LeaveNativeDataDepth;
procedure CheckNativeWork;
function CheckedNativeByteSize(const AElementCount,
  AElementSize: Int64): Int64;
procedure ReserveNativeBytes(const ABytes: Int64);
procedure ReleaseNativeBytes(const ABytes: Int64);
{ StringJoin's gate: before a native join allocates ABytes, for its tables or
  its result, raises ENativeMemoryLimit "Invalid string length" for a result
  no string can hold (TOO_LONG_RESULT_BYTES). }
procedure CheckStringResultBytes(const ABytes: Int64);
{ StringJoin's charge: reserves ABytes when positive, as ReserveNativeBytes
  does, with its catchable RangeError when they do not fit, and releases
  -ABytes when negative. }
procedure ChargeJoinBytes(const ABytes: Int64);

type
  { The engine's ledger for native bytes: AReserve charges ABytes,
    False refusing them; otherwise it releases them. While a thread has
    one, a refusal raises ENativeMemoryLimit for that engine to throw. }
  TNativeBytesHook = function(const ABytes: Int64;
    const AReserve: Boolean): Boolean;
  ENativeMemoryLimit = class(Exception);
  { The nesting guard's refusal, for the engine to throw as a RangeError. }
  ENativeDepthLimit = class(Exception);
  { The engine's charge for native work: AUnits of it counted against the
    run's limits, False when a limit or a host's cancellation ends the run
    (its abort then pending in the engine). }
  TNativeWorkHook = function(const AUnits: QWord): Boolean;
  { CheckNativeWork's refusal under a TNativeWorkHook: the run's abort is
    pending, and the native returns False without throwing anything else. }
  ENativeWorkStopped = class(Exception);

threadvar
  NativeBytesHook: TNativeBytesHook;
  { While a thread's engine has one, CheckNativeWork charges through it
    instead of the interpreter's counters (Goccia.InstructionLimit,
    Goccia.Timeout). }
  NativeWorkHook: TNativeWorkHook;

implementation

uses
  StringJoin,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.InstructionLimit,
  Goccia.Timeout;

{ FPC 3.3.1 writes a call of a procedural threadvar as a direct call of the variable in its GNU assembler output
  (`call U_$...`, for the AFL build, -Aas): called through a local copy, it is called through a register }
function CallNativeBytesHook(const ABytes: Int64; const AReserve: Boolean): Boolean;
var H: TNativeBytesHook;
begin
  H := NativeBytesHook;
  Result := H(ABytes, AReserve);
end;

function CallNativeWorkHook(const AUnits: QWord): Boolean;
var H: TNativeWorkHook;
begin
  H := NativeWorkHook;
  Result := H(AUnits);
end;


threadvar
  GNativeDataDepth: Integer;

procedure EnterNativeDataDepth(const AOperation: string);
begin
  Inc(GNativeDataDepth);
  try
    if GNativeDataDepth > MAX_NATIVE_DATA_DEPTH then
    begin
      raise ENativeDepthLimit.Create(SErrorMaxCallStackExceeded);
    end;
    CheckNativeWork;
  except
    Dec(GNativeDataDepth);
    raise;
  end;
end;

procedure LeaveNativeDataDepth;
begin
  if GNativeDataDepth > 0 then
    Dec(GNativeDataDepth);
end;

procedure CheckNativeWork;
begin
  if Assigned(NativeWorkHook) then
  begin
    if not CallNativeWorkHook(1) then
      raise ENativeWorkStopped.Create('native work stopped by the run''s limits');
    Exit;
  end;
  // Counts its own unit below: the unmetered poll keeps the budget from
  // charging this work twice.
  CheckExecutionTimeoutUnmetered;
  IncrementInstructionCounter;
  CheckInstructionLimit;
end;

function CheckedNativeByteSize(const AElementCount,
  AElementSize: Int64): Int64;
begin
  if (AElementCount < 0) or (AElementSize < 0) or
     ((AElementSize > 0) and
      (AElementCount > MAX_NATIVE_ALLOCATION_BYTES div AElementSize)) then
    raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
  Result := AElementCount * AElementSize;
end;

procedure ReserveNativeBytes(const ABytes: Int64);
begin
  if ABytes <= 0 then
    Exit;
  if Assigned(NativeBytesHook) and not CallNativeBytesHook(ABytes, True) then
    raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
end;

procedure ReleaseNativeBytes(const ABytes: Int64);
begin
  if Assigned(NativeBytesHook) and (ABytes > 0) then
    CallNativeBytesHook(ABytes, False);
end;

procedure CheckStringResultBytes(const ABytes: Int64);
begin
  if ABytes = TOO_LONG_RESULT_BYTES then
    raise ENativeMemoryLimit.Create(SErrorInvalidStringLength);
end;

procedure ChargeJoinBytes(const ABytes: Int64);
begin
  if ABytes > 0 then
    ReserveNativeBytes(ABytes)
  else if ABytes < 0 then
    ReleaseNativeBytes(-ABytes);
end;

end.
