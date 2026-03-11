unit Goccia.Builtins.Performance;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Constants.PropertyNames,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaPerformance = class(TGocciaBuiltin)
  private
    FTimeOriginEpochNanoseconds: Int64;
    FTimeOriginMonotonicNanoseconds: Int64;

    function GetTimeOriginMilliseconds: Double; inline;
  protected
    function PerformanceNow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PerformanceTimeOriginGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PerformanceToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,

  TimingUtils,

  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

function TGocciaPerformance.GetTimeOriginMilliseconds: Double;
begin
  Result := FTimeOriginEpochNanoseconds / 1000000.0;
end;

constructor TGocciaPerformance.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  MonotonicBefore, MonotonicAfter: Int64;
begin
  inherited Create(AName, AScope, AThrowError);

  MonotonicBefore := GetNanoseconds;
  FTimeOriginEpochNanoseconds := GetEpochNanoseconds;
  MonotonicAfter := GetNanoseconds;
  FTimeOriginMonotonicNanoseconds := (MonotonicBefore + MonotonicAfter) div 2;

  FBuiltinObject.RegisterNativeMethod(
    TGocciaNativeFunctionValue.Create(PerformanceNow, PROP_NOW, 0));
  FBuiltinObject.RegisterNativeMethod(
    TGocciaNativeFunctionValue.Create(PerformanceToJSON, PROP_TO_JSON, 0));

  FBuiltinObject.DefineProperty(PROP_TIME_ORIGIN,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        PerformanceTimeOriginGetter, 'get timeOrigin', 0),
      nil, [pfConfigurable]));

  FBuiltinObject.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create('Performance'),
      [pfConfigurable]));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

// High Resolution Time Level 3 §7.1 Performance.now()
function TGocciaPerformance.PerformanceNow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ElapsedNanoseconds: Int64;
begin
  ElapsedNanoseconds := GetNanoseconds - FTimeOriginMonotonicNanoseconds;
  Result := TGocciaNumberLiteralValue.Create(Max(0.0, ElapsedNanoseconds / 1000000.0));
end;

// High Resolution Time Level 3 §7.2 Performance.timeOrigin
function TGocciaPerformance.PerformanceTimeOriginGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(GetTimeOriginMilliseconds);
end;

// High Resolution Time Level 3 §7.3 Performance.toJSON()
function TGocciaPerformance.PerformanceToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  JsonObject: TGocciaObjectValue;
begin
  JsonObject := TGocciaObjectValue.Create;
  JsonObject.AssignProperty(PROP_TIME_ORIGIN, TGocciaNumberLiteralValue.Create(GetTimeOriginMilliseconds));
  Result := JsonObject;
end;

end.
