unit Goccia.NativeLimits;

{$I Goccia.inc}

interface

const
  MAX_NATIVE_DATA_DEPTH = 256;

procedure EnterNativeDataDepth(const AOperation: string = '');
procedure LeaveNativeDataDepth;
procedure CheckNativeWork;
function CheckedNativeByteSize(const AElementCount,
  AElementSize: Int64): Int64;
procedure ReserveNativeBytes(const ABytes: Int64);
procedure ReleaseNativeBytes(const ABytes: Int64);

implementation

uses
  SysUtils,

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
  if GNativeDataDepth > MAX_NATIVE_DATA_DEPTH then
  begin
    Dec(GNativeDataDepth);
    ThrowRangeError(SErrorMaxCallStackExceeded);
  end;
  CheckNativeWork;
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
      (AElementCount > High(Int64) div AElementSize)) then
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
  GC.CollectForMemoryPressure(nil);
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

end.
