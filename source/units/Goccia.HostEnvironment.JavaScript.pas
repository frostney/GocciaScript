unit Goccia.HostEnvironment.JavaScript;

{$I Goccia.inc}

interface

uses
  Goccia.Engine;

procedure ConfigureHostEnvironmentFromModule(const AEngine: TGocciaEngine;
  const AModulePath: string);

implementation

uses
  Math,
  SysUtils,

  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.Constants.NumericLimits,
  Goccia.HostEnvironment,
  Goccia.Modules,
  Goccia.Temporal.TimeZone,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

const
  STREAM_HASH_INCREMENT = QWord($9E3779B97F4A7C15);
  STREAM_HASH_MULTIPLIER = QWord($BF58476D1CE4E5B9);

type
  TGocciaJavaScriptHostClock = class(TInterfacedObject, IGocciaHostClock)
  private
    FEpochNanosecondsProvider: TGocciaValue;
    FMonotonicNanosecondsProvider: TGocciaValue;
    FTimeZoneIdentifierProvider: TGocciaValue;
    FCallingProvider: Boolean;
    function CallProvider(const AProvider: TGocciaValue;
      const AExportName: string): TGocciaValue;
  public
    constructor Create(const AEpochNanosecondsProvider,
      AMonotonicNanosecondsProvider,
      ATimeZoneIdentifierProvider: TGocciaValue);
    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  TGocciaJavaScriptHostRandom = class(TInterfacedObject, IGocciaHostRandom)
  private
    FProvider: TGocciaValue;
    FStreamId: QWord;
    FCallingProvider: Boolean;
    class function DeriveStreamId(const AParentStreamId,
      AChildStreamId: QWord): QWord; static;
  public
    constructor Create(const AProvider: TGocciaValue;
      const AStreamId: QWord);
    function NextDouble: Double;
    function Fork(const AStreamId: QWord): IGocciaHostRandom;
  end;

function BigIntegerFromQWord(const AValue: QWord): TBigInteger;
var
  HighBits, LowBits: Int64;
begin
  LowBits := Int64(AValue and $FFFFFFFF);
  HighBits := Int64(AValue shr 32);
  if HighBits = 0 then
    Result := TBigInteger.FromInt64(LowBits)
  else
    Result := TBigInteger.FromInt64(HighBits)
      .Multiply(TBigInteger.FromInt64($100000000))
      .Add(TBigInteger.FromInt64(LowBits));
end;

function RequireCallableExport(const AModule: TGocciaModule;
  const AExportName: string): TGocciaValue;
begin
  if (not AModule.TryGetExportValue(AExportName, Result)) or
     (not Assigned(Result)) or (not Result.IsCallable) then
    ThrowTypeError('Host environment module must export callable "' +
      AExportName + '"');
end;

function HostNanosecondsFromValue(const AValue: TGocciaValue;
  const AExportName: string): Int64;
var
  BigIntegerValue: TBigInteger;
  NumberValue: Double;
begin
  if AValue is TGocciaBigIntValue then
  begin
    BigIntegerValue := TGocciaBigIntValue(AValue).Value;
    if (BigIntegerValue.Compare(TBigInteger.FromInt64(Low(Int64))) < 0) or
       (BigIntegerValue.Compare(TBigInteger.FromInt64(High(Int64))) > 0) then
      ThrowRangeError('Host environment export "' + AExportName +
        '" must return an Int64-range BigInt');
    Exit(BigIntegerValue.ToInt64);
  end;

  if AValue is TGocciaNumberLiteralValue then
  begin
    NumberValue := TGocciaNumberLiteralValue(AValue).Value;
    if IsNan(NumberValue) or IsInfinite(NumberValue) or
       (Frac(NumberValue) <> 0) or (Abs(NumberValue) > MAX_SAFE_INTEGER_F) then
      ThrowRangeError('Host environment export "' + AExportName +
        '" must return a safe integer Number or Int64-range BigInt');
    Exit(Trunc(NumberValue));
  end;

  ThrowTypeError('Host environment export "' + AExportName +
    '" must return a Number or BigInt');
  Result := 0;
end;

{ TGocciaJavaScriptHostClock }

constructor TGocciaJavaScriptHostClock.Create(
  const AEpochNanosecondsProvider, AMonotonicNanosecondsProvider,
  ATimeZoneIdentifierProvider: TGocciaValue);
begin
  inherited Create;
  FEpochNanosecondsProvider := AEpochNanosecondsProvider;
  FMonotonicNanosecondsProvider := AMonotonicNanosecondsProvider;
  FTimeZoneIdentifierProvider := ATimeZoneIdentifierProvider;
end;

function TGocciaJavaScriptHostClock.CallProvider(
  const AProvider: TGocciaValue; const AExportName: string): TGocciaValue;
var
  Arguments: TGocciaArgumentsCollection;
begin
  if FCallingProvider then
    ThrowTypeError('Host environment export "' + AExportName +
      '" cannot call a host time source recursively');

  FCallingProvider := True;
  Arguments := TGocciaArgumentsCollection.Create;
  try
    Result := DispatchCall(AProvider, Arguments,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Arguments.Free;
    FCallingProvider := False;
  end;
end;

function TGocciaJavaScriptHostClock.EpochNanoseconds: Int64;
begin
  Result := HostNanosecondsFromValue(CallProvider(
    FEpochNanosecondsProvider, 'epochNanoseconds'), 'epochNanoseconds');
end;

function TGocciaJavaScriptHostClock.MonotonicNanoseconds: Int64;
begin
  Result := HostNanosecondsFromValue(CallProvider(
    FMonotonicNanosecondsProvider, 'monotonicNanoseconds'),
    'monotonicNanoseconds');
end;

function TGocciaJavaScriptHostClock.TimeZoneIdentifier: string;
var
  CanonicalTimeZone: string;
  Value: TGocciaValue;
begin
  Value := CallProvider(FTimeZoneIdentifierProvider, 'timeZoneIdentifier');
  if not (Value is TGocciaStringLiteralValue) then
    ThrowTypeError('Host environment export "timeZoneIdentifier" must ' +
      'return a string');

  Result := TGocciaStringLiteralValue(Value).Value;
  if TimeZoneIdentifiersEqual(Result, 'UTC') then
    Exit('UTC');
  if TryCanonicalizeTimeZoneIdentifierCase(Result, CanonicalTimeZone) and
     IsValidTimeZone(CanonicalTimeZone) then
    Exit(CanonicalTimeZone);
  if not IsValidTimeZone(Result) then
    ThrowRangeError('Host environment export "timeZoneIdentifier" returned ' +
      'an unsupported time zone: ' + Result);
end;

{ TGocciaJavaScriptHostRandom }

constructor TGocciaJavaScriptHostRandom.Create(const AProvider: TGocciaValue;
  const AStreamId: QWord);
begin
  inherited Create;
  FProvider := AProvider;
  FStreamId := AStreamId;
end;

{$PUSH}
{$OVERFLOWCHECKS OFF}
class function TGocciaJavaScriptHostRandom.DeriveStreamId(
  const AParentStreamId, AChildStreamId: QWord): QWord;
begin
  Result := (AParentStreamId xor STREAM_HASH_INCREMENT) + AChildStreamId;
  Result := (Result xor (Result shr 30)) * STREAM_HASH_MULTIPLIER;
  Result := Result xor (Result shr 27);
end;
{$POP}

function TGocciaJavaScriptHostRandom.NextDouble: Double;
var
  Arguments: TGocciaArgumentsCollection;
  Value: TGocciaValue;
begin
  if FCallingProvider then
    ThrowTypeError('Host environment export "random" cannot call ' +
      'Math.random recursively');

  FCallingProvider := True;
  Arguments := TGocciaArgumentsCollection.Create([
    TGocciaBigIntValue.Create(BigIntegerFromQWord(FStreamId))]);
  try
    Value := DispatchCall(FProvider, Arguments,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Arguments.Free;
    FCallingProvider := False;
  end;

  if not (Value is TGocciaNumberLiteralValue) then
    ThrowTypeError('Host environment export "random" must return a Number');
  Result := TGocciaNumberLiteralValue(Value).Value;
  if IsNan(Result) or IsInfinite(Result) or (Result < 0) or (Result >= 1) then
    ThrowRangeError('Host environment export "random" must return a finite ' +
      'Number greater than or equal to 0 and less than 1');
end;

function TGocciaJavaScriptHostRandom.Fork(
  const AStreamId: QWord): IGocciaHostRandom;
begin
  Result := TGocciaJavaScriptHostRandom.Create(FProvider,
    DeriveStreamId(FStreamId, AStreamId));
end;

procedure ConfigureHostEnvironmentFromModule(const AEngine: TGocciaEngine;
  const AModulePath: string);
var
  Clock: IGocciaHostClock;
  EpochNanosecondsProvider: TGocciaValue;
  Module: TGocciaModule;
  MonotonicNanosecondsProvider: TGocciaValue;
  RandomProvider: TGocciaValue;
  TimeZoneIdentifierProvider: TGocciaValue;
begin
  if not Assigned(AEngine) then
    raise EArgumentNilException.Create('AEngine');

  Module := AEngine.ModuleLoader.LoadModule(AModulePath, AEngine.SourcePath);
  EpochNanosecondsProvider := RequireCallableExport(Module,
    'epochNanoseconds');
  MonotonicNanosecondsProvider := RequireCallableExport(Module,
    'monotonicNanoseconds');
  TimeZoneIdentifierProvider := RequireCallableExport(Module,
    'timeZoneIdentifier');
  RandomProvider := RequireCallableExport(Module, 'random');

  Clock := TGocciaJavaScriptHostClock.Create(EpochNanosecondsProvider,
    MonotonicNanosecondsProvider, TimeZoneIdentifierProvider);
  AEngine.HostEnvironment.Configure(Clock,
    TGocciaJavaScriptHostRandom.Create(RandomProvider, 0));
end;

end.
