unit Goccia.CLI.JSON.EngineReporter;

{ The old engine's half of the CLI JSON report (Goccia.CLI.JSON.Reporter): a
  script's result value, the old collector's memory figures, and the error
  object of a value the old engine threw. }

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.CLI.JSON.Reporter,
  Goccia.Values.Primitives;

procedure BeginCLIJSONMemoryMeasurement(
  var AMeasurement: TCLIJSONMemoryMeasurement);
function FinishCLIJSONMemoryMeasurement(
  const AMeasurement: TCLIJSONMemoryMeasurement): TCLIJSONMemoryStats;
function SerializeScriptResult(const AValue: TGocciaValue): string;
{ ExceptionToCLIJSONErrorInfo, with the name, message and location of a value
  the old engine threw }
function EngineExceptionToCLIJSONErrorInfo(
  const E: Exception): TCLIJSONErrorInfo;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Detail,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.JSON.Utils,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.VM.Exception;

function SerializeScriptResult(const AValue: TGocciaValue): string;
var
  Stringifier: TGocciaJSONStringifier;
  Serialized: string;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit('null');

  if AValue.IsCallable then
    Exit(QuoteJSONString(AValue.ToStringLiteral.Value));

  Stringifier := TGocciaJSONStringifier.Create;
  try
    Serialized := Stringifier.Stringify(AValue);
  finally
    Stringifier.Free;
  end;

  if (Serialized = 'null') and
     not (AValue is TGocciaNullLiteralValue) and
     not (AValue is TGocciaBooleanLiteralValue) and
     not (AValue is TGocciaNumberLiteralValue) then
    Result := QuoteJSONString(AValue.ToStringLiteral.Value)
  else
    Result := Serialized;
end;

procedure BeginCLIJSONMemoryMeasurement(
  var AMeasurement: TCLIJSONMemoryMeasurement);
var
  GC: TGarbageCollector;
  HeapStatus: THeapStatus;
begin
  TGarbageCollector.Initialize;
  GC := TGarbageCollector.Instance;
  HeapStatus := GetHeapStatus;
  AMeasurement.Stats := DefaultCLIJSONMemoryStats;
  AMeasurement.Stats.Enabled := True;
  AMeasurement.Stats.HeapStartAllocatedBytes := Int64(HeapStatus.TotalAllocated);
  AMeasurement.Stats.HeapEndAllocatedBytes :=
    AMeasurement.Stats.HeapStartAllocatedBytes;
  AMeasurement.Stats.HeapStartFreeBytes := Int64(HeapStatus.TotalFree);
  AMeasurement.Stats.HeapEndFreeBytes := AMeasurement.Stats.HeapStartFreeBytes;

  if not Assigned(GC) then
    Exit;

  GC.ResetPeakBytesAllocated;
  AMeasurement.Stats.GCStartBytes := GC.BytesAllocated;
  AMeasurement.Stats.GCEndBytes := GC.BytesAllocated;
  AMeasurement.Stats.GCPeakBytes := GC.PeakBytesAllocated;
  AMeasurement.Stats.GCLiveBytes := GC.BytesAllocated;
  AMeasurement.Stats.GCMaxBytes := GC.MaxBytes;
  AMeasurement.Stats.GCStartObjectCount := GC.ManagedObjectCount;
  AMeasurement.Stats.GCEndObjectCount := GC.ManagedObjectCount;
  AMeasurement.StartTotalBytesAllocated := GC.TotalBytesAllocated;
  AMeasurement.StartCollections := GC.TotalCollections;
  AMeasurement.StartCollectedObjects := GC.TotalCollected;
end;

function FinishCLIJSONMemoryMeasurement(
  const AMeasurement: TCLIJSONMemoryMeasurement): TCLIJSONMemoryStats;
var
  GC: TGarbageCollector;
  HeapStatus: THeapStatus;
begin
  Result := AMeasurement.Stats;
  HeapStatus := GetHeapStatus;
  Result.HeapEndAllocatedBytes := Int64(HeapStatus.TotalAllocated);
  Result.HeapDeltaAllocatedBytes := Result.HeapEndAllocatedBytes -
    Result.HeapStartAllocatedBytes;
  Result.HeapEndFreeBytes := Int64(HeapStatus.TotalFree);
  Result.HeapDeltaFreeBytes := Result.HeapEndFreeBytes -
    Result.HeapStartFreeBytes;

  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;

  Result.GCEndBytes := GC.BytesAllocated;
  Result.GCPeakBytes := GC.PeakBytesAllocated;
  Result.GCLiveBytes := GC.BytesAllocated;
  Result.GCDeltaBytes := Result.GCEndBytes - Result.GCStartBytes;
  Result.GCAllocatedDuringRunBytes := GC.TotalBytesAllocated -
    AMeasurement.StartTotalBytesAllocated;
  Result.GCMaxBytes := GC.MaxBytes;
  Result.GCEndObjectCount := GC.ManagedObjectCount;
  Result.GCCollections := GC.TotalCollections - AMeasurement.StartCollections;
  Result.GCCollectedObjects := GC.TotalCollected -
    AMeasurement.StartCollectedObjects;
end;

function TryPopulateErrorInfoFromValue(const AValue: TGocciaValue;
  var AErrorInfo: TCLIJSONErrorInfo): Boolean;
var
  ErrorObject: TGocciaObjectValue;
  NameText, MessageText: string;
begin
  Result := False;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  ErrorObject := TGocciaObjectValue(AValue);
  // The report is built after the run's engine was freed, so it reads string
  // data properties only: an accessor or a Proxy trap would run guest code
  // against that engine (the same rule as the human-readable renderer).
  if TryGetStringDataProperty(ErrorObject, PROP_NAME, NameText) then
    AErrorInfo.ErrorType := NameText;
  if TryGetStringDataProperty(ErrorObject, PROP_MESSAGE, MessageText) then
    AErrorInfo.Message := MessageText;

  // Where the engine recorded the error was created: the same location the
  // human-readable output's `-->` line shows. The guest-writable `stack`
  // string is never read for it.
  if (ErrorObject is TGocciaErrorObjectValue) and ErrorObject.HasErrorData and
     TGocciaErrorObjectValue(ErrorObject).HasErrorSourceLocation and
     (TGocciaErrorObjectValue(ErrorObject).ErrorSourceLine > 0) then
  begin
    if TGocciaErrorObjectValue(ErrorObject).ErrorSourcePath <> '' then
      AErrorInfo.FileName := TGocciaErrorObjectValue(ErrorObject).ErrorSourcePath;
    AErrorInfo.Line := TGocciaErrorObjectValue(ErrorObject).ErrorSourceLine;
    AErrorInfo.Column := TGocciaErrorObjectValue(ErrorObject).ErrorSourceColumn;
  end;

  Result := (AErrorInfo.ErrorType <> '') or (AErrorInfo.Message <> '');
end;

{ The message of a thrown value that carries no string name or message. An
  object is not converted to a string: that would run its toString() against
  an engine that was freed. }
function ThrownValueFallbackText(const AValue: TGocciaValue): string;
begin
  if AValue is TGocciaObjectValue then
    Result := '[object ' + TGocciaObjectValue(AValue).ToStringTag + ']'
  else if AValue is TGocciaSymbolValue then
    Result := TGocciaSymbolValue(AValue).ToDisplayString.Value
  else
    Result := AValue.ToStringLiteral.Value;
end;

function EngineExceptionToCLIJSONErrorInfo(
  const E: Exception): TCLIJSONErrorInfo;
begin
  if E is TGocciaThrowValue then
  begin
    Result := DefaultCLIJSONErrorInfo;
    if not TryPopulateErrorInfoFromValue(TGocciaThrowValue(E).Value, Result) then
    begin
      Result.ErrorType := 'Error';
      Result.Message := ThrownValueFallbackText(TGocciaThrowValue(E).Value);
    end;
  end
  else if E is EGocciaBytecodeThrow then
  begin
    Result := DefaultCLIJSONErrorInfo;
    if not TryPopulateErrorInfoFromValue(EGocciaBytecodeThrow(E).ThrownValue, Result) then
    begin
      Result.ErrorType := 'Error';
      Result.Message := EGocciaBytecodeThrow(E).Message;
    end;
  end
  else
    Result := ExceptionToCLIJSONErrorInfo(E);
end;

end.
