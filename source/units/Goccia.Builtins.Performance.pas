unit Goccia.Builtins.Performance;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.SharedPrototype,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaPerformanceValue = class(TGocciaObjectValue)
  private
    FTimeOriginEpochNanoseconds: Int64;
    FTimeOriginMonotonicNanoseconds: Int64;

    function GetTimeOriginMilliseconds: Double; inline;
  public
    constructor Create;

    property TimeOriginMilliseconds: Double read GetTimeOriginMilliseconds;
  end;

  TGocciaPerformancePrototypeHost = class(TGocciaObjectValue)
  public
    function PerformanceConstructorCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PerformanceNow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PerformanceTimeOriginGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PerformanceToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaPerformance = class(TGocciaBuiltin)
  private
    class procedure InitializePrototype;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function CreateInterfaceObject: TGocciaNativeFunctionValue;
  end;

implementation

uses
  Math,

  TimingUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GPerformanceSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FPrototypeMethodHost: TGocciaPerformancePrototypeHost;

function GetPerformanceShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GPerformanceSharedSlot))
  else
    Result := nil;
end;

function RequirePerformanceThis(const AThisValue: TGocciaValue): TGocciaPerformanceValue;
begin
  if not (AThisValue is TGocciaPerformanceValue) then
    ThrowTypeError(SErrorPerformanceIncompatibleReceiver, SSuggestPerformanceThisType);
  Result := TGocciaPerformanceValue(AThisValue);
end;

{ TGocciaPerformanceValue }

function TGocciaPerformanceValue.GetTimeOriginMilliseconds: Double;
begin
  Result := FTimeOriginEpochNanoseconds / 1000000.0;
end;

// High Resolution Time Level 3 §7 The Performance interface
constructor TGocciaPerformanceValue.Create;
var
  MonotonicBefore, MonotonicAfter: Int64;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);

  MonotonicBefore := GetNanoseconds;
  FTimeOriginEpochNanoseconds := GetEpochNanoseconds;
  MonotonicAfter := GetNanoseconds;
  FTimeOriginMonotonicNanoseconds := (MonotonicBefore + MonotonicAfter) div 2;

  TGocciaPerformance.InitializePrototype;
  Shared := GetPerformanceShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

{ TGocciaPerformancePrototypeHost }

function TGocciaPerformancePrototypeHost.PerformanceConstructorCall(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(SErrorIllegalConstructor, SSuggestIllegalConstructor);
end;

// High Resolution Time Level 3 §7.1 Performance.now()
function TGocciaPerformancePrototypeHost.PerformanceNow(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Performance: TGocciaPerformanceValue;
  ElapsedNanoseconds: Int64;
begin
  Performance := RequirePerformanceThis(AThisValue);
  ElapsedNanoseconds := GetNanoseconds - Performance.FTimeOriginMonotonicNanoseconds;
  Result := TGocciaNumberLiteralValue.Create(Max(0.0, ElapsedNanoseconds / 1000000.0));
end;

// High Resolution Time Level 3 §7.2 Performance.timeOrigin
function TGocciaPerformancePrototypeHost.PerformanceTimeOriginGetter(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Performance: TGocciaPerformanceValue;
begin
  Performance := RequirePerformanceThis(AThisValue);
  Result := TGocciaNumberLiteralValue.Create(Performance.TimeOriginMilliseconds);
end;

// High Resolution Time Level 3 §7.3 Performance.toJSON()
function TGocciaPerformancePrototypeHost.PerformanceToJSON(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  JsonObject: TGocciaObjectValue;
  Performance: TGocciaPerformanceValue;
begin
  Performance := RequirePerformanceThis(AThisValue);
  JsonObject := TGocciaObjectValue.Create;
  JsonObject.AssignProperty(PROP_TIME_ORIGIN, TGocciaNumberLiteralValue.Create(Performance.TimeOriginMilliseconds));
  Result := JsonObject;
end;

{ TGocciaPerformance }

class procedure TGocciaPerformance.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetPerformanceShared) then
    Exit;

  FPrototypeMethodHost := TGocciaPerformancePrototypeHost.Create(nil);
  Shared := TGocciaSharedPrototype.Create(FPrototypeMethodHost);
  CurrentRealm.SetOwnedSlot(GPerformanceSharedSlot, Shared);
  Shared.Prototype.Prototype := TGocciaObjectValue.SharedObjectPrototype;

  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddDataProperty(
        PROP_NOW,
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          FPrototypeMethodHost.PerformanceNow,
          PROP_NOW,
          0),
        [pfEnumerable, pfConfigurable, pfWritable]);
      Members.AddDataProperty(
        PROP_TO_JSON,
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          FPrototypeMethodHost.PerformanceToJSON,
          PROP_TO_JSON,
          0),
        [pfEnumerable, pfConfigurable, pfWritable]);
      Members.AddAccessor(
        PROP_TIME_ORIGIN,
        FPrototypeMethodHost.PerformanceTimeOriginGetter,
        nil,
        [pfEnumerable, pfConfigurable],
        gmkPrototypeGetter);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Performance'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;

  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaPerformance.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  InitializePrototype;
  Shared := GetPerformanceShared;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

class function TGocciaPerformance.CreateInterfaceObject: TGocciaNativeFunctionValue;
begin
  InitializePrototype;
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FPrototypeMethodHost.PerformanceConstructorCall,
    CONSTRUCTOR_PERFORMANCE,
    0);
  ExposePrototype(Result);
end;

// High Resolution Time Level 3 §7 The Performance interface
constructor TGocciaPerformance.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  FName := AName;
  FScope := AScope;
  FThrowError := AThrowError;

  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;

  FBuiltinObject := TGocciaPerformanceValue.Create;
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

initialization
  GPerformanceSharedSlot := RegisterRealmOwnedSlot('Performance.shared');

end.
