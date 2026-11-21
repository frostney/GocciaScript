unit Goccia.NativeLimits;

{$I Goccia.inc}

interface

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

implementation

uses
  SysUtils,

  StringJoin,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Timeout,
  Goccia.Values.ErrorHelper;

threadvar
  GNativeDataDepth: Integer;

procedure EnterNativeDataDepth(const AOperation: string);
begin
  Inc(GNativeDataDepth);
  try
    if GNativeDataDepth > MAX_NATIVE_DATA_DEPTH then
      ThrowRangeError(SErrorMaxCallStackExceeded);
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
    ThrowRangeError(SErrorMemoryLimitExceeded);
  Result := AElementCount * AElementSize;
end;

procedure ReserveNativeBytes(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  if ABytes <= 0 then
    Exit;
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  if not GC.TryReserveExternalBytes(ABytes) then
    ThrowRangeError(SErrorMemoryLimitExceeded, SSuggestMemoryLimitExceeded);
end;

procedure ReleaseNativeBytes(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (ABytes > 0) then
    GC.ReleaseExternalBytes(ABytes);
end;

procedure CheckStringResultBytes(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  if ABytes = TOO_LONG_RESULT_BYTES then
    ThrowRangeError(SErrorInvalidStringLength);
  // The joins that call this hold no value their receiver and arguments do
  // not root, so a collection frees only what nothing reaches. The stress
  // switch makes every ask the collecting safe point it is near the ceiling,
  // as RequireNativeBytes does.
  GC := TGarbageCollector.Instance;
  if (ABytes > 0) and Assigned(GC) and GC.CollectBeforeEveryReservation then
    GC.CollectForMemoryPressure(nil, True);
  if CanAllocateNativeBytes(ABytes) then
    Exit;
  if Assigned(GC) and GC.TryCollectForLimitedBytes(ABytes) then
    Exit;
  ThrowRangeError(SErrorMemoryLimitExceeded, SSuggestMemoryLimitExceeded);
end;

procedure ChargeJoinBytes(const ABytes: Int64);
begin
  if ABytes > 0 then
    ReserveNativeBytes(ABytes)
  else if ABytes < 0 then
    ReleaseNativeBytes(-ABytes);
end;

end.
