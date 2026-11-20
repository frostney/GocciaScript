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

type
  { Another engine's ledger for native bytes (G2): AReserve charges ABytes,
    False refusing them; otherwise it releases them. While a thread has
    one, a refusal raises ENativeMemoryLimit for that engine to throw. }
  TNativeBytesHook = function(const ABytes: Int64;
    const AReserve: Boolean): Boolean;
  ENativeMemoryLimit = class(Exception);

threadvar
  NativeBytesHook: TNativeBytesHook;

implementation

uses
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
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
  CheckExecutionTimeout;
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
    if Assigned(NativeBytesHook) then
      raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
    ThrowRangeError(SErrorMemoryLimitExceeded);
  end;
  Result := AElementCount * AElementSize;
end;

procedure ReserveNativeBytes(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  if ABytes <= 0 then
    Exit;
  if Assigned(NativeBytesHook) then
  begin
    if not NativeBytesHook(ABytes, True) then
      raise ENativeMemoryLimit.Create(SErrorMemoryLimitExceeded);
    Exit;
  end;
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
  if Assigned(NativeBytesHook) then
  begin
    if ABytes > 0 then
      NativeBytesHook(ABytes, False);
    Exit;
  end;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (ABytes > 0) then
    GC.ReleaseExternalBytes(ABytes);
end;

end.
