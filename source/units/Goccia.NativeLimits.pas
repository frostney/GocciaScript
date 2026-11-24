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
  its result, raises the catchable RangeError a string's own charge raises
  past the memory budget, once a forced collection has not made room, as the
  charge's own path does; and "Invalid string length" for a result no string
  can hold (TOO_LONG_RESULT_BYTES). }
procedure CheckStringResultBytes(const ABytes: Int64);
{ StringJoin's charge: reserves ABytes when positive, as ReserveNativeBytes
  does, with its catchable RangeError when they do not fit, and releases
  -ABytes when negative. }
procedure ChargeJoinBytes(const ABytes: Int64);

type
  { Another engine's ledger for native bytes (G2): AReserve charges ABytes,
    False refusing them; otherwise it releases them. While a thread has
    one, a refusal raises ENativeMemoryLimit for that engine to throw. }
  TNativeBytesHook = function(const ABytes: Int64;
    const AReserve: Boolean): Boolean;
  ENativeMemoryLimit = class(Exception);
  { The nesting guard's refusal while another engine runs on the thread
    (NativeErrorsPlain), for that engine to throw as a RangeError. }
  ENativeDepthLimit = class(Exception);

  { The old engine's side of these checks (Goccia.Values.ErrorHelper sets it
    once, for every thread): its RangeError, and its collector's charge for
    native bytes. Without one (no old engine in the program) the checks raise
    the plain exceptions above and charge nothing. }
  TNativeLimitsRuntime = record
    { raises the engine's RangeError with AMessage (and ASuggestion) }
    ThrowRangeError: procedure(const AMessage, ASuggestion: string);
    { charges ABytes to the running thread's collector; raises its
      RangeError when they do not fit }
    ReserveBytes: procedure(const ABytes: Int64);
    ReleaseBytes: procedure(const ABytes: Int64);
    { StringJoin's gate past the TOO_LONG_RESULT_BYTES test: collects when
      that may make room for ABytes, else raises the engine's RangeError }
    CheckStringResultBytes: procedure(const ABytes: Int64);
  end;

var
  NativeLimitsRuntime: TNativeLimitsRuntime;

threadvar
  NativeBytesHook: TNativeBytesHook;
  { Another engine (G2) runs on this thread: EnterNativeDataDepth refuses
    with ENativeDepthLimit, not with this runtime's RangeError value. }
  NativeErrorsPlain: Boolean;

implementation

uses
  StringJoin,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.InstructionLimit,
  Goccia.Timeout;

threadvar
  GNativeDataDepth: Integer;

procedure EnterNativeDataDepth(const AOperation: string);
begin
  Inc(GNativeDataDepth);
  try
    if GNativeDataDepth > MAX_NATIVE_DATA_DEPTH then
    begin
      if NativeErrorsPlain or not Assigned(NativeLimitsRuntime.ThrowRangeError) then
        raise ENativeDepthLimit.Create(SErrorMaxCallStackExceeded);
      NativeLimitsRuntime.ThrowRangeError(SErrorMaxCallStackExceeded, '');
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
  begin
    if Assigned(NativeBytesHook) or
       not Assigned(NativeLimitsRuntime.ThrowRangeError) then
      raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
    NativeLimitsRuntime.ThrowRangeError(SErrorMemoryLimitExceeded, '');
  end;
  Result := AElementCount * AElementSize;
end;

procedure ReserveNativeBytes(const ABytes: Int64);
begin
  if ABytes <= 0 then
    Exit;
  if Assigned(NativeBytesHook) then
  begin
    if not NativeBytesHook(ABytes, True) then
      raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
    Exit;
  end;
  if Assigned(NativeLimitsRuntime.ReserveBytes) then
    NativeLimitsRuntime.ReserveBytes(ABytes);
end;

procedure ReleaseNativeBytes(const ABytes: Int64);
begin
  if Assigned(NativeBytesHook) then
  begin
    if ABytes > 0 then
      NativeBytesHook(ABytes, False);
    Exit;
  end;
  if (ABytes > 0) and Assigned(NativeLimitsRuntime.ReleaseBytes) then
    NativeLimitsRuntime.ReleaseBytes(ABytes);
end;

procedure CheckStringResultBytes(const ABytes: Int64);
begin
  if ABytes = TOO_LONG_RESULT_BYTES then
  begin
    if not Assigned(NativeLimitsRuntime.ThrowRangeError) then
      raise ENativeMemoryLimit.Create(SErrorInvalidStringLength);
    NativeLimitsRuntime.ThrowRangeError(SErrorInvalidStringLength, '');
  end;
  if Assigned(NativeLimitsRuntime.CheckStringResultBytes) then
    NativeLimitsRuntime.CheckStringResultBytes(ABytes);
end;

procedure ChargeJoinBytes(const ABytes: Int64);
begin
  if ABytes > 0 then
    ReserveNativeBytes(ABytes)
  else if ABytes < 0 then
    ReleaseNativeBytes(-ABytes);
end;

end.
