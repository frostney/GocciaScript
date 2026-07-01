unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  HashMap,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TSymbolDescriptorMap = THashMap<TGocciaSymbolValue, TGocciaPropertyDescriptor>;

  TGocciaObjectValue = class(TGocciaValue)
  private
    procedure SetRegExpData(const AValue: TObject);
    function MaterializeOwnLazyProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
    procedure MaterializeAllLazyStringProperties;
    function StoreLazyPropertyDescriptor(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor;
      out ABlockedByExtensibility: Boolean): Boolean;
  protected
    FProperties: TGocciaPropertyMap;
    FSymbolDescriptors: TSymbolDescriptorMap;
    FSymbolInsertionOrder: TList<TGocciaSymbolValue>;
    FPrototype: TGocciaObjectValue;
    FFrozen: Boolean;
    FSealed: Boolean;
    FExtensible: Boolean;
    FHasErrorData: Boolean;
    FErrorStack: string;
    FHasRegExpData: Boolean;
    FRegExpData: TObject;
  public
    class procedure InitializeSharedPrototype;
    class function GetSharedObjectPrototype: TGocciaObjectValue; static;
    class function GetSharedObjectPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; static;
    class procedure SetSharedObjectPrototype(const AValue: TGocciaObjectValue); static;
    class property SharedObjectPrototype: TGocciaObjectValue read GetSharedObjectPrototype write SetSharedObjectPrototype;
    constructor Create(const APrototype: TGocciaObjectValue = nil;
      const APropertyCapacity: Integer = 0);
    destructor Destroy; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToStringTag: string; virtual;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;

    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); virtual;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; virtual;
    procedure CreateDataPropertyOrThrow(const AName: string;
      const AValue: TGocciaValue); overload;
    procedure CreateDataPropertyOrThrow(const ASymbol: TGocciaSymbolValue;
      const AValue: TGocciaValue); overload;
    procedure CreateDataPropertyOrThrow(const APropertyKey: TGocciaValue;
      const AValue: TGocciaValue); overload;
    function TryCreateDataProperty(const AName: string;
      const AValue: TGocciaValue): Boolean; overload;
    function TryCreateDataProperty(const ASymbol: TGocciaSymbolValue;
      const AValue: TGocciaValue): Boolean; overload;
    function TryCreateDataProperty(const APropertyKey: TGocciaValue;
      const AValue: TGocciaValue): Boolean; overload;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); virtual;
    function AssignPropertyWithReceiver(const AName: string; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean; virtual;

    procedure RegisterNativeMethod(const AMethod: TGocciaValue);
    procedure RegisterConstant(const AName: string; const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; virtual;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; virtual;
    function HasProperty(const AName: string): Boolean; virtual;
    function HasOwnProperty(const AName: string): Boolean; virtual;
    function DeleteProperty(const AName: string): Boolean; virtual;

    function GetEnumerablePropertyNames: TArray<string>; virtual;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; virtual;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; virtual;
    function GetAllPropertyNames: TArray<string>; virtual;
    function GetOwnPropertyNames: TArray<string>; virtual;
    function GetOwnPropertyKeys: TArray<string>; virtual;

    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor); virtual;
    function TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor): Boolean; virtual;
    procedure AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
    function AssignSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean; virtual;
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue; virtual;
    function GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue): TGocciaValue; virtual;
    function GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; virtual;
    function HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean; virtual;
    function DeleteSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
    function GetEnumerableSymbolProperties: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>; virtual;

    procedure Freeze; virtual;
    procedure Seal; virtual;
    procedure PreventExtensions; virtual;

    // ES2026 §7.3.16 TestIntegrityLevel(O, level)
    function TestIntegrityFrozen: Boolean; virtual;
    function TestIntegritySealed: Boolean; virtual;

    procedure MarkReferences; override;

    property Properties: TGocciaPropertyMap read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
    property Frozen: Boolean read FFrozen;
    property Sealed: Boolean read FSealed;
    property Extensible: Boolean read FExtensible;
    property HasErrorData: Boolean read FHasErrorData write FHasErrorData;
    property ErrorStack: string read FErrorStack write FErrorStack;
    property HasRegExpData: Boolean read FHasRegExpData write FHasRegExpData;
    property RegExpData: TObject read FRegExpData write SetRegExpData;
  published
    function ObjectPrototypeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeHasOwnProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeIsPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypePropertyIsEnumerable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;


implementation

uses
  Classes,
  SysUtils,

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ProxyValue,
  Goccia.Values.Shape,
  Goccia.Values.ToPrimitive;

// Object.prototype and its method host both live in per-realm slots, so the
// realm pins them on SetSlot and unpins them on Destroy.  The method definitions
// are rebuilt per realm because their method pointers embed the host instance;
// keeping the host realm-owned (rather than a per-thread pinned threadvar that
// outlives every realm) means it is reclaimed with its realm. See #892.
var
  GObjectPrototypeSlot: TGocciaRealmSlotId;
  GObjectMethodHostSlot: TGocciaRealmSlotId;

const
  MAX_PROTOTYPE_CHAIN_DEPTH = 256;
  BYTECODE_PRIVATE_INITIALIZED_PREFIX = '#initialized:';

function IsBytecodePrivateInitializerMarker(const AName: string): Boolean;
begin
  Result := Copy(AName, 1, Length(BYTECODE_PRIVATE_INITIALIZED_PREFIX)) =
    BYTECODE_PRIVATE_INITIALIZED_PREFIX;
end;

function IsHiddenRegExpDataProperty(const AObject: TGocciaObjectValue;
  const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := False;
  if (not AObject.HasRegExpData) or
     ((AName <> PROP_SOURCE) and (AName <> PROP_FLAGS)) then
    Exit;

  Descriptor := AObject.GetOwnPropertyDescriptor(AName);
  Result := (Descriptor is TGocciaPropertyDescriptorData) and
    (not Descriptor.Enumerable) and
    (not Descriptor.Configurable) and
    (not TGocciaPropertyDescriptorData(Descriptor).Writable);
end;

function TryParseArrayIndexKey(const AKey: string; out AIndex: UInt64): Boolean;
var
  Digit: UInt64;
  I: Integer;
begin
  AIndex := 0;
  Result := False;
  if AKey = '' then
    Exit;
  if (Length(AKey) > 1) and (AKey[1] = '0') then
    Exit;

  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    if AIndex > (High(UInt32) - Digit) div 10 then
      Exit;
    AIndex := AIndex * 10 + Digit;
  end;

  Result := AIndex < High(UInt32);
end;

function OrderOwnStringPropertyKeys(const AKeys: TArray<string>):
  TArray<string>;
var
  Count, I, J, K: Integer;
  NumericKeys: TArray<UInt64>;
  OtherKeys: TArray<string>;
  ParsedIndex, TempIndex: UInt64;
begin
  SetLength(NumericKeys, Length(AKeys));
  SetLength(OtherKeys, Length(AKeys));
  Count := 0;
  J := 0;

  for I := 0 to High(AKeys) do
  begin
    if TryParseArrayIndexKey(AKeys[I], ParsedIndex) then
    begin
      NumericKeys[Count] := ParsedIndex;
      Inc(Count);
    end
    else
    begin
      OtherKeys[J] := AKeys[I];
      Inc(J);
    end;
  end;

  for I := 1 to Count - 1 do
  begin
    TempIndex := NumericKeys[I];
    K := I - 1;
    while (K >= 0) and (NumericKeys[K] > TempIndex) do
    begin
      NumericKeys[K + 1] := NumericKeys[K];
      Dec(K);
    end;
    NumericKeys[K + 1] := TempIndex;
  end;

  SetLength(Result, Count + J);
  for I := 0 to Count - 1 do
    Result[I] := UIntToStr(NumericKeys[I]);
  for I := 0 to J - 1 do
    Result[Count + I] := OtherKeys[I];
end;

// Local ToObject variant for Object.prototype methods.  Keeping this here
// avoids a unit cycle with the shared ToObject unit.
function ObjectPrototypeToObject(const AValue: TGocciaValue): TGocciaObjectValue;
var
  Boxed: TGocciaObjectValue;
begin
  if (AValue is TGocciaUndefinedLiteralValue) or (AValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);

  if AValue is TGocciaObjectValue then
  begin
    Result := TGocciaObjectValue(AValue);
    Exit;
  end;

  Boxed := AValue.Box;
  if Assigned(Boxed) then
  begin
    Result := Boxed;
    Exit;
  end;

  ThrowTypeError(SErrorCannotConvertValueToObject, SSuggestObjectArgType);
  Result := nil;
end;

function ObjectPrototypeOf(const AObject: TGocciaObjectValue): TGocciaValue;
begin
  if AObject is TGocciaProxyValue then
    Exit(TGocciaProxyValue(AObject).GetPrototypeTrap);

  if Assigned(AObject.Prototype) then
    Result := AObject.Prototype
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function OwnPropertyKeyValues(const AObject: TGocciaObjectValue): TArray<TGocciaValue>;
var
  Count: Integer;
  I: Integer;
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
begin
  if AObject is TGocciaProxyValue then
    Exit(TGocciaProxyValue(AObject).GetOwnPropertyKeyValues);

  StringKeys := AObject.GetOwnPropertyKeys;
  SymbolKeys := AObject.GetOwnSymbols;
  SetLength(Result, Length(StringKeys) + Length(SymbolKeys));
  Count := 0;
  for I := 0 to High(StringKeys) do
  begin
    Result[Count] := TGocciaStringLiteralValue.Create(StringKeys[I]);
    Inc(Count);
  end;
  for I := 0 to High(SymbolKeys) do
  begin
    Result[Count] := SymbolKeys[I];
    Inc(Count);
  end;
end;

function OwnPropertyDescriptorForKey(
  const AObject: TGocciaObjectValue;
  const AKey: TGocciaValue): TGocciaPropertyDescriptor;
begin
  if AKey is TGocciaSymbolValue then
    Result := AObject.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey))
  else
    Result := AObject.GetOwnPropertyDescriptor(AKey.ToStringLiteral.Value);
end;

procedure DefineOwnPropertyOrThrowForKey(
  const AObject: TGocciaObjectValue;
  const AKey: TGocciaValue;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  try
    if AKey is TGocciaSymbolValue then
      AObject.DefineSymbolProperty(TGocciaSymbolValue(AKey), ADescriptor)
    else
      AObject.DefineProperty(AKey.ToStringLiteral.Value, ADescriptor);
  except
    ADescriptor.Free;
    raise;
  end;
end;

procedure MarkPropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor);
begin
  ADescriptor.MarkValues;
end;

{ TGocciaObjectValue }

procedure TGocciaObjectValue.SetRegExpData(const AValue: TObject);
begin
  if FRegExpData = AValue then
    Exit;
  FRegExpData.Free;
  FRegExpData := AValue;
end;

function TGocciaObjectValue.MaterializeOwnLazyProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
var
  LazyDescriptor: TGocciaLazyPropertyDescriptorData;
  MaterializedDescriptor: TGocciaPropertyDescriptorData;
  Value: TGocciaValue;
begin
  Result := ADescriptor;
  if not (ADescriptor is TGocciaLazyPropertyDescriptorData) then
    Exit;

  LazyDescriptor := TGocciaLazyPropertyDescriptorData(ADescriptor);
  Value := LazyDescriptor.Materialize;
  MaterializedDescriptor := TGocciaPropertyDescriptorData.Create(
    Value, LazyDescriptor.Flags);
  FProperties.Add(AName, MaterializedDescriptor);
  LazyDescriptor.Free;
  Result := MaterializedDescriptor;
end;

procedure TGocciaObjectValue.MaterializeAllLazyStringProperties;
var
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
  Keys: TArray<string>;
begin
  Keys := FProperties.Keys;
  for I := 0 to High(Keys) do
    if FProperties.TryGetValue(Keys[I], Descriptor) then
      MaterializeOwnLazyProperty(Keys[I], Descriptor);
end;

function TGocciaObjectValue.StoreLazyPropertyDescriptor(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor;
  out ABlockedByExtensibility: Boolean): Boolean;
var
  Current: TGocciaPropertyDescriptor;
begin
  Result := False;
  ABlockedByExtensibility := False;
  if not (ADescriptor is TGocciaLazyPropertyDescriptorData) then
    Exit;

  Current := nil;
  FProperties.TryGetValue(AName, Current);
  if not Assigned(Current) and not FExtensible then
  begin
    ABlockedByExtensibility := True;
    Exit;
  end;
  if Assigned(Current) and not Current.Configurable then
    Exit;

  if Assigned(Current) then
    Current.Free;
  FProperties.Add(AName, ADescriptor);
  Result := True;
end;

class function TGocciaObjectValue.GetSharedObjectPrototype: TGocciaObjectValue;
begin
  Result := GetSharedObjectPrototypeForRealm(CurrentRealm);
end;

class function TGocciaObjectValue.GetSharedObjectPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  if Assigned(ARealm) then
    Result := TGocciaObjectValue(ARealm.GetSlot(GObjectPrototypeSlot))
  else
    Result := nil;
end;

class procedure TGocciaObjectValue.SetSharedObjectPrototype(const AValue: TGocciaObjectValue);
begin
  if Assigned(CurrentRealm) then
    CurrentRealm.SetSlot(GObjectPrototypeSlot, AValue);
end;

constructor TGocciaObjectValue.Create(const APrototype: TGocciaObjectValue = nil;
  const APropertyCapacity: Integer = 0);
begin
  // Shaped map: layout tracking for the VM's shape-validated inline caches
  // rides on the property map itself, so every mutation path stays in sync.
  FProperties := TGocciaShapedPropertyMap.Create(APropertyCapacity);
  FSymbolDescriptors := TSymbolDescriptorMap.Create;
  FSymbolInsertionOrder := TList<TGocciaSymbolValue>.Create;
  FPrototype := APrototype;
  FFrozen := False;
  FSealed := False;
  FExtensible := True;
end;

// ES2026 §20.1.3.6 Object.prototype.toString()
function TGocciaObjectValue.ObjectPrototypeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Tag: string;
  SymbolTag: TGocciaValue;
  Obj: TGocciaObjectValue;

  function IsArrayObject(const AObject: TGocciaObjectValue): Boolean;
  var
    Proxy: TGocciaProxyValue;
  begin
    if AObject is TGocciaArrayValue then
      Exit(True);
    if AObject is TGocciaProxyValue then
    begin
      Proxy := TGocciaProxyValue(AObject);
      if Proxy.Revoked then
        ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
      if Proxy.Target is TGocciaObjectValue then
        Exit(IsArrayObject(TGocciaObjectValue(Proxy.Target)));
    end;
    Result := False;
  end;

  function LegacyBuiltinTagForObject(const AObject: TGocciaObjectValue): string;
  begin
    Result := AObject.ToStringTag;
  end;

begin
  if AThisValue is TGocciaUndefinedLiteralValue then
    Exit(TGocciaStringLiteralValue.Create('[object Undefined]'));
  if AThisValue is TGocciaNullLiteralValue then
    Exit(TGocciaStringLiteralValue.Create('[object Null]'));

  Obj := ObjectPrototypeToObject(AThisValue);
  if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    if IsArrayObject(Obj) then
      Tag := CONSTRUCTOR_ARRAY
    else if Obj.IsCallable then
      Tag := 'Function'
    else if Obj.HasErrorData then
      Tag := 'Error'
    else if Obj.HasRegExpData then
      Tag := 'RegExp'
    else if (Obj is TGocciaProxyValue) then
      Tag := CONSTRUCTOR_OBJECT
    else
      Tag := LegacyBuiltinTagForObject(Obj);

    SymbolTag := Obj.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue.WellKnownToStringTag, AThisValue);
    if Assigned(SymbolTag) and (SymbolTag is TGocciaStringLiteralValue) then
      Tag := TGocciaStringLiteralValue(SymbolTag).Value;
  finally
    if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;

  Result := TGocciaStringLiteralValue.Create('[object ' + Tag + ']');
end;

// ES2026 §20.1.3.2 Object.prototype.hasOwnProperty(V)
function TGocciaObjectValue.ObjectPrototypeHasOwnProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  KeyValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Own: Boolean;
  PropertyArg: TGocciaValue;
begin
  // Step 1: Let P be ? ToPropertyKey(V)
  if AArgs.Length = 0 then
    PropertyArg := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    PropertyArg := AArgs.GetElement(0);
  KeyValue := ToPropertyKey(PropertyArg);

  // Step 2: Let O be ? ToObject(this value)
  Obj := ObjectPrototypeToObject(AThisValue);
  if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 3: Return ? HasOwnProperty(O, P)
    if KeyValue is TGocciaSymbolValue then
      Own := Obj.HasSymbolProperty(TGocciaSymbolValue(KeyValue))
    else
      Own := Obj.HasOwnProperty(KeyValue.ToStringLiteral.Value);

    if Own then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  finally
    if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.3.3 Object.prototype.isPrototypeOf(V)
function TGocciaObjectValue.ObjectPrototypeIsPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  V: TGocciaObjectValue;
  Current: TGocciaValue;
begin
  // Step 1: If V is not an Object, return false
  if (AArgs.Length = 0) or not (AArgs.GetElement(0) is TGocciaObjectValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  // Step 2: Let O be ? ToObject(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);
  if not (AThisValue is TGocciaObjectValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  V := TGocciaObjectValue(AArgs.GetElement(0));
  // Step 3: Repeat — walk V's prototype chain
  Current := ObjectPrototypeOf(V);
  while Current is TGocciaObjectValue do
  begin
    // Step 3a: If SameValue(O, V.[[Prototype]]) is true, return true
    if Current = AThisValue then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Current := ObjectPrototypeOf(TGocciaObjectValue(Current));
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.3.4 Object.prototype.propertyIsEnumerable(V)
function TGocciaObjectValue.ObjectPrototypePropertyIsEnumerable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  KeyValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyArg: TGocciaValue;
begin
  // Step 1: Let P be ? ToPropertyKey(V)
  if AArgs.Length = 0 then
    PropertyArg := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    PropertyArg := AArgs.GetElement(0);
  KeyValue := ToPropertyKey(PropertyArg);

  // Step 2: Let O be ? ToObject(this value)
  Obj := ObjectPrototypeToObject(AThisValue);
  if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    if KeyValue is TGocciaSymbolValue then
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(KeyValue))
    else
      Descriptor := Obj.GetOwnPropertyDescriptor(KeyValue.ToStringLiteral.Value);

    // Step 4: If desc is undefined, return false
    if not Assigned(Descriptor) then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    // Step 5: Return desc.[[Enumerable]]
    if Descriptor.Enumerable then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  finally
    if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.3.5 Object.prototype.toLocaleString()
function TGocciaObjectValue.ObjectPrototypeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ToStringMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Step 1: Let O be the this value
  // Step 2: Return ? Invoke(O, "toString")
  Obj := ObjectPrototypeToObject(AThisValue);
  if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    ToStringMethod := Obj.GetPropertyWithContext(PROP_TO_STRING, AThisValue);
    if not Assigned(ToStringMethod) or not ToStringMethod.IsCallable then
      ThrowTypeError(Format(SErrorValueNotFunction, [PROP_TO_STRING]), SSuggestNotFunctionType);

    CallArgs := TGocciaArgumentsCollection.Create;
    try
      Result := DispatchCall(ToStringMethod, CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) and not (AThisValue is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.3.7 Object.prototype.valueOf()
function TGocciaObjectValue.ObjectPrototypeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return ? ToObject(this value)
  Result := ObjectPrototypeToObject(AThisValue);
end;

class procedure TGocciaObjectValue.InitializeSharedPrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
  MethodHost: TGocciaObjectValue;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  // No realm yet - very early bootstrap path.  Subsequent constructor runs
  // under an assigned realm will retry.
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedObjectPrototype) then Exit;

  MethodHost := TGocciaObjectValue.Create;
  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GObjectPrototypeSlot, SharedPrototype);
  // Keep the method host alive for the realm's lifetime in its own slot: the
  // realm pins it here and unpins it on Destroy, together with the prototype
  // whose method pointers embed it. #892
  CurrentRealm.SetSlot(GObjectMethodHostSlot, MethodHost);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(MethodHost.ObjectPrototypeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(MethodHost.ObjectPrototypeHasOwnProperty, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(MethodHost.ObjectPrototypeIsPrototypeOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(MethodHost.ObjectPrototypePropertyIsEnumerable, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(MethodHost.ObjectPrototypeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(MethodHost.ObjectPrototypeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  PrototypeMembers[0].ExposedName := PROP_TO_STRING;
  PrototypeMembers[1].ExposedName := 'hasOwnProperty';
  PrototypeMembers[2].ExposedName := PROP_IS_PROTOTYPE_OF;
  PrototypeMembers[3].ExposedName := PROP_PROPERTY_IS_ENUMERABLE;
  PrototypeMembers[4].ExposedName := PROP_TO_LOCALE_STRING;
  PrototypeMembers[5].ExposedName := PROP_VALUE_OF;
  RegisterMemberDefinitions(SharedPrototype, PrototypeMembers);
end;

destructor TGocciaObjectValue.Destroy;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymValues: array of TGocciaPropertyDescriptor;
  I: Integer;
begin
  for Pair in FProperties do
    Pair.Value.Free;
  FProperties.Free;

  SymValues := FSymbolDescriptors.Values;
  for I := 0 to Length(SymValues) - 1 do
    SymValues[I].Free;
  FSymbolDescriptors.Free;

  FSymbolInsertionOrder.Free;
  FRegExpData.Free;
  inherited;
end;

procedure TGocciaObjectValue.MarkReferences;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymPair: TSymbolDescriptorMap.TKeyValuePair;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FPrototype) then
    FPrototype.MarkReferences;

  for Pair in FProperties do
    Pair.Value.MarkValues;

  for SymPair in FSymbolDescriptors do
  begin
    SymPair.Key.MarkReferences;
    SymPair.Value.MarkValues;
  end;
end;

function TGocciaObjectValue.TypeName: string;
begin
  Result := 'object';
end;

function TGocciaObjectValue.TypeOf: string;
begin
  Result := 'object';
end;

function TGocciaObjectValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_OBJECT;
end;

// ES2026 §7.1.17 ToString. For an object: ToPrimitive(O, string) → ToString
// of the resulting primitive. May invoke user-defined toString()/valueOf()
// and may throw. Callers that cannot tolerate user re-entry must use
// SafeToString from Goccia.Values.ToPrimitive instead.
function TGocciaObjectValue.ToStringLiteral: TGocciaStringLiteralValue;
var
  Prim: TGocciaValue;
begin
  Prim := ToPrimitive(Self, tphString);
  if Prim is TGocciaSymbolValue then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
  // Prim is a primitive at this point — its ToStringLiteral cannot recurse
  // into user code.
  Result := Prim.ToStringLiteral;
end;

function TGocciaObjectValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §7.1.4 ToNumber. For an object: ToPrimitive(O, number) → ToNumber
// of the resulting primitive. May invoke user-defined valueOf()/toString()
// and may throw.
function TGocciaObjectValue.ToNumberLiteral: TGocciaNumberLiteralValue;
var
  Prim: TGocciaValue;
begin
  Prim := ToPrimitive(Self, tphNumber);
  if Prim is TGocciaSymbolValue then
    ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
  Result := Prim.ToNumberLiteral;
end;

// ES2026 §10.1.9 [[Set]](P, V, Receiver)
procedure TGocciaObjectValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Proto: TGocciaObjectValue;
  ChainDepth: Integer;
begin
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    Descriptor := MaterializeOwnLazyProperty(AName, Descriptor);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Setter) and Accessor.Setter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          TGocciaFunctionBase(Accessor.Setter).Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end;
      ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [AName, ToStringTag]), SSuggestPropertyHasOnlyGetter);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
        Exit;
      end;
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
    end;
  end;

  // ES2026 §10.1.9 step 2-3: Walk prototype chain for inherited accessor descriptors
  Proto := FPrototype;
  ChainDepth := 0;
  while Assigned(Proto) do
  begin
    Inc(ChainDepth);
    if ChainDepth > MAX_PROTOTYPE_CHAIN_DEPTH then
      ThrowTypeError(Format(SErrorProtoChainDepthExceeded, [AName]), SSuggestPrototypeChainTooDeep);
    Descriptor := Proto.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
        if Assigned(Accessor.Setter) and Accessor.Setter.IsCallable then
        begin
          Args := TGocciaArgumentsCollection.Create;
          try
            Args.Add(AValue);
            TGocciaFunctionBase(Accessor.Setter).Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end;
        ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [AName, ToStringTag]), SSuggestPropertyHasOnlyGetter);
      end
      else if Descriptor is TGocciaPropertyDescriptorData then
      begin
        if not TGocciaPropertyDescriptorData(Descriptor).Writable then
          ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
        Break;
      end;
    end;
    Proto := Proto.FPrototype;
  end;

  if not ACanCreate then
    ThrowTypeError(Format(SErrorCannotAssignNonExistent, [AName]), SSuggestCannotDeleteNonConfigurable);

  if not FExtensible then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]), SSuggestObjectNotExtensible);

  DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc)
function TGocciaObjectValue.AssignPropertyWithReceiver(const AName: string; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean;
var
  OwnDesc, ReceiverDesc: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  ReceiverObj: TGocciaObjectValue;
begin
  // Step 1: Let ownDesc be O.[[GetOwnProperty]](P)
  OwnDesc := GetOwnPropertyDescriptor(AName);

  if OwnDesc = nil then
  begin
    // Step 1a-b: Walk prototype chain
    if Assigned(FPrototype) then
      Exit(FPrototype.AssignPropertyWithReceiver(AName, AValue, AReceiver));
    // Step 1c: No prototype — ownDesc defaults to writable data descriptor,
    // fall through to step 2 which consults Receiver.[[GetOwnProperty]](P)
    if not (AReceiver is TGocciaObjectValue) then
      Exit(False);
    ReceiverObj := TGocciaObjectValue(AReceiver);
    ReceiverDesc := ReceiverObj.GetOwnPropertyDescriptor(AName);
    if Assigned(ReceiverDesc) then
    begin
      // Step 2d: Receiver already has own property P
      if (ReceiverDesc is TGocciaPropertyDescriptorAccessor) or
         (not ReceiverDesc.Writable) then
        Exit(False);
      // Step 2d.iii-iv: Receiver.[[DefineOwnProperty]](P, { [[Value]]: V })
      Exit(ReceiverObj.TryDefineProperty(AName,
        TGocciaPropertyDescriptorData.Create(AValue, ReceiverDesc.Flags)));
    end;
    // Step 2e: CreateDataProperty(Receiver, P, V)
    Exit(ReceiverObj.TryDefineProperty(AName,
      TGocciaPropertyDescriptorData.Create(AValue,
        [pfEnumerable, pfConfigurable, pfWritable])));
  end;

  if OwnDesc is TGocciaPropertyDescriptorData then
  begin
    // Step 2a: If ownDesc.[[Writable]] is false, return false
    if not OwnDesc.Writable then
      Exit(False);
    // Step 2b: If Receiver is not an Object, return false
    if not (AReceiver is TGocciaObjectValue) then
      Exit(False);
    ReceiverObj := TGocciaObjectValue(AReceiver);
    // Step 2c: Let existingDescriptor be Receiver.[[GetOwnProperty]](P)
    ReceiverDesc := ReceiverObj.GetOwnPropertyDescriptor(AName);
    if Assigned(ReceiverDesc) then
    begin
      // Step 2d.i: If IsAccessorDescriptor(existingDescriptor), return false
      if ReceiverDesc is TGocciaPropertyDescriptorAccessor then
        Exit(False);
      // Step 2d.ii: If existingDescriptor.[[Writable]] is false, return false
      if not ReceiverDesc.Writable then
        Exit(False);
      // Step 2d.iii-iv: Receiver.[[DefineOwnProperty]](P, { [[Value]]: V })
      Exit(ReceiverObj.TryDefineProperty(AName,
        TGocciaPropertyDescriptorData.Create(AValue, ReceiverDesc.Flags)));
    end
    else
    begin
      // Step 2e: CreateDataProperty(Receiver, P, V)
      Exit(ReceiverObj.TryDefineProperty(AName,
        TGocciaPropertyDescriptorData.Create(AValue,
          [pfEnumerable, pfConfigurable, pfWritable])));
    end;
  end;

  // Step 3-7: Accessor descriptor
  Accessor := TGocciaPropertyDescriptorAccessor(OwnDesc);
  // Step 4-5: If setter is undefined, return false
  if not Assigned(Accessor.Setter) or not Accessor.Setter.IsCallable then
    Exit(False);
  // Step 6: Call(setter, Receiver, « V »)
  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(AValue);
    TGocciaFunctionBase(Accessor.Setter).Call(Args, AReceiver);
  finally
    Args.Free;
  end;
  // Step 7: Return true
  Result := True;
end;

function SetFlag(const AFlags: TPropertyFlags; const AFlag: TPropertyFlag;
  const AEnabled: Boolean): TPropertyFlags;
begin
  Result := AFlags;
  if AEnabled then
    Include(Result, AFlag)
  else
    Exclude(Result, AFlag);
end;

function CompleteNewPropertyDescriptor(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
var
  Flags: TPropertyFlags;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
begin
  Flags := [];
  if ADescriptor.HasEnumerableField and ADescriptor.Enumerable then
    Include(Flags, pfEnumerable);
  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Include(Flags, pfConfigurable);

  if IsAccessorDescriptor(ADescriptor) then
  begin
    Getter := nil;
    Setter := nil;
    if ADescriptor.HasGet then
      Getter := TGocciaPropertyDescriptorAccessor(ADescriptor).Getter;
    if ADescriptor.HasSet then
      Setter := TGocciaPropertyDescriptorAccessor(ADescriptor).Setter;
    Exit(TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
  end;

  Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasValue then
    Value := TGocciaPropertyDescriptorData(ADescriptor).Value;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasWritableField and
     ADescriptor.Writable then
    Include(Flags, pfWritable);

  Result := TGocciaPropertyDescriptorData.Create(Value, Flags);
end;

function ApplyPropertyDescriptor(
  const ACurrent: TGocciaPropertyDescriptor;
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
var
  Flags: TPropertyFlags;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
begin
  Flags := ACurrent.Flags;
  if ADescriptor.HasEnumerableField then
    Flags := SetFlag(Flags, pfEnumerable, ADescriptor.Enumerable);
  if ADescriptor.HasConfigurableField then
    Flags := SetFlag(Flags, pfConfigurable, ADescriptor.Configurable);

  if IsAccessorDescriptor(ADescriptor) then
  begin
    Exclude(Flags, pfWritable);
    Getter := nil;
    Setter := nil;
    if IsAccessorDescriptor(ACurrent) then
    begin
      Getter := TGocciaPropertyDescriptorAccessor(ACurrent).Getter;
      Setter := TGocciaPropertyDescriptorAccessor(ACurrent).Setter;
    end;
    if ADescriptor.HasGet then
      Getter := TGocciaPropertyDescriptorAccessor(ADescriptor).Getter;
    if ADescriptor.HasSet then
      Setter := TGocciaPropertyDescriptorAccessor(ADescriptor).Setter;
    Exit(TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
  end;

  if IsDataDescriptor(ADescriptor) then
  begin
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exclude(Flags, pfWritable);
    if IsDataDescriptor(ACurrent) then
    begin
      Value := TGocciaPropertyDescriptorData(ACurrent).Value;
      Flags := SetFlag(Flags, pfWritable, ACurrent.Writable);
    end;
    if ADescriptor.HasValue then
      Value := TGocciaPropertyDescriptorData(ADescriptor).Value;
    if ADescriptor.HasWritableField then
      Flags := SetFlag(Flags, pfWritable, ADescriptor.Writable);
    Exit(TGocciaPropertyDescriptorData.Create(Value, Flags));
  end;

  if ACurrent is TGocciaPropertyDescriptorData then
    Result := TGocciaPropertyDescriptorData.Create(
      TGocciaPropertyDescriptorData(ACurrent).Value, Flags)
  else
    Result := TGocciaPropertyDescriptorAccessor.Create(
      TGocciaPropertyDescriptorAccessor(ACurrent).Getter,
      TGocciaPropertyDescriptorAccessor(ACurrent).Setter,
      Flags);
end;

// ES2026 §10.1.6.3 ValidateAndApplyPropertyDescriptor
function ValidateAndCreatePropertyDescriptor(
  const ACurrent: TGocciaPropertyDescriptor;
  const ANew: TGocciaPropertyDescriptor;
  const AExtensible: Boolean;
  out AApplied: TGocciaPropertyDescriptor
): Boolean;
var
  CurrentData: TGocciaPropertyDescriptorData;
  CurrentAccessor: TGocciaPropertyDescriptorAccessor;
  NewData: TGocciaPropertyDescriptorData;
  NewAccessor: TGocciaPropertyDescriptorAccessor;
begin
  AApplied := nil;

  if not Assigned(ACurrent) then
  begin
    if not AExtensible then
      Exit(False);
    AApplied := CompleteNewPropertyDescriptor(ANew);
    Exit(True);
  end;

  Result := True;
  if ANew.Fields = [] then
  begin
    AApplied := ClonePropertyDescriptor(ACurrent);
    Exit;
  end;

  if ACurrent.Configurable then
  begin
    AApplied := ApplyPropertyDescriptor(ACurrent, ANew);
    Exit;
  end;

  // §10.1.6.3 step 3a: Cannot make configurable
  if ANew.HasConfigurableField and ANew.Configurable then
    Exit(False);

  // 3b: Cannot change enumerable
  if ANew.HasEnumerableField and (ACurrent.Enumerable <> ANew.Enumerable) then
    Exit(False);

  if IsGenericDescriptor(ANew) then
  begin
    AApplied := ApplyPropertyDescriptor(ACurrent, ANew);
    Exit;
  end;

  // 3c: Cannot change descriptor kind (data ↔ accessor)
  if IsDataDescriptor(ACurrent) <> IsDataDescriptor(ANew) then
    Exit(False);

  // 3d: Non-configurable accessor — cannot change get or set
  if IsAccessorDescriptor(ACurrent) and IsAccessorDescriptor(ANew) then
  begin
    CurrentAccessor := TGocciaPropertyDescriptorAccessor(ACurrent);
    NewAccessor := TGocciaPropertyDescriptorAccessor(ANew);
    if ANew.HasGet and (CurrentAccessor.Getter <> NewAccessor.Getter) then
      Exit(False);
    if ANew.HasSet and (CurrentAccessor.Setter <> NewAccessor.Setter) then
      Exit(False);
  end;

  // 3e: Non-configurable data — validate writable/value changes
  if IsDataDescriptor(ACurrent) and IsDataDescriptor(ANew) then
  begin
    CurrentData := TGocciaPropertyDescriptorData(ACurrent);
    NewData := TGocciaPropertyDescriptorData(ANew);
    if not CurrentData.Writable then
    begin
      // 3e.i: Cannot make writable once non-writable
      if ANew.HasWritableField and NewData.Writable then
        Exit(False);
      // 3e.ii: Cannot change value
      if ANew.HasValue and not IsSameValue(CurrentData.Value, NewData.Value) then
        Exit(False);
    end;
  end;

  AApplied := ApplyPropertyDescriptor(ACurrent, ANew);
end;

// ES2026 §20.1.2.3.1 DefinePropertyOrThrow(O, P, desc)
// Does NOT take ownership of ADescriptor on failure — caller must free.
procedure TGocciaObjectValue.DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  Current: TGocciaPropertyDescriptor;
  Applied: TGocciaPropertyDescriptor;
  BlockedByExtensibility: Boolean;
begin
  if StoreLazyPropertyDescriptor(AName, ADescriptor,
     BlockedByExtensibility) then
    Exit;
  if BlockedByExtensibility then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]),
      SSuggestObjectNotExtensible);

  Current := nil;
  FProperties.TryGetValue(AName, Current);
  if Assigned(Current) then
    Current := MaterializeOwnLazyProperty(AName, Current);
  if not ValidateAndCreatePropertyDescriptor(Current, ADescriptor,
    FExtensible, Applied) then
  begin
    if not Assigned(Current) and not FExtensible then
      ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]), SSuggestObjectNotExtensible);
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]), SSuggestCannotDeleteNonConfigurable);
  end;

  if Assigned(Current) then
  begin
    Current.Free;
    FProperties.Add(AName, Applied);
  end
  else
    FProperties.Add(AName, Applied);

  ADescriptor.Free;
end;

// ES2026 §10.1.6.1 OrdinaryDefineOwnProperty(O, P, Desc)
// Takes ownership of ADescriptor — frees it on failure.
function TGocciaObjectValue.TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Current: TGocciaPropertyDescriptor;
  Applied: TGocciaPropertyDescriptor;
  BlockedByExtensibility: Boolean;
begin
  if StoreLazyPropertyDescriptor(AName, ADescriptor,
     BlockedByExtensibility) then
    Exit(True);
  if BlockedByExtensibility then
  begin
    ADescriptor.Free;
    Exit(False);
  end;

  Current := nil;
  FProperties.TryGetValue(AName, Current);
  if Assigned(Current) then
    Current := MaterializeOwnLazyProperty(AName, Current);
  if not ValidateAndCreatePropertyDescriptor(Current, ADescriptor,
    FExtensible, Applied) then
  begin
    ADescriptor.Free;
    Exit(False);
  end;

  if Assigned(Current) then
    Current.Free;
  FProperties.Add(AName, Applied);
  ADescriptor.Free;
  Result := True;
end;

// ES2026 §7.3.6 CreateDataPropertyOrThrow(O, P, V)
procedure TGocciaObjectValue.CreateDataPropertyOrThrow(const AName: string;
  const AValue: TGocciaValue);
begin
  DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

// ES2026 §7.3.6 CreateDataPropertyOrThrow(O, P, V) — symbol key variant
procedure TGocciaObjectValue.CreateDataPropertyOrThrow(
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
begin
  DefineSymbolProperty(ASymbol, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

// APropertyKey must already be the result of ToPropertyKey.
procedure TGocciaObjectValue.CreateDataPropertyOrThrow(
  const APropertyKey: TGocciaValue; const AValue: TGocciaValue);
begin
  if APropertyKey is TGocciaSymbolValue then
    CreateDataPropertyOrThrow(TGocciaSymbolValue(APropertyKey), AValue)
  else
    CreateDataPropertyOrThrow(APropertyKey.ToStringLiteral.Value, AValue);
end;

// ES2026 §7.3.5 CreateDataProperty(O, P, V)
function TGocciaObjectValue.TryCreateDataProperty(const AName: string;
  const AValue: TGocciaValue): Boolean;
begin
  Result := TryDefineProperty(AName, TGocciaPropertyDescriptorData.Create(
    AValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

// ES2026 §7.3.5 CreateDataProperty(O, P, V) — symbol key variant
function TGocciaObjectValue.TryCreateDataProperty(
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue): Boolean;
begin
  Result := TryDefineSymbolProperty(ASymbol,
    TGocciaPropertyDescriptorData.Create(AValue,
      [pfEnumerable, pfConfigurable, pfWritable]));
end;

// APropertyKey must already be the result of ToPropertyKey.
function TGocciaObjectValue.TryCreateDataProperty(
  const APropertyKey: TGocciaValue; const AValue: TGocciaValue): Boolean;
begin
  if APropertyKey is TGocciaSymbolValue then
    Result := TryCreateDataProperty(TGocciaSymbolValue(APropertyKey), AValue)
  else
    Result := TryCreateDataProperty(APropertyKey.ToStringLiteral.Value, AValue);
end;

function TGocciaObjectValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaObjectValue.GetOwnPropertyKeys: TArray<string>;
var
  Key: string;
  Keys: TArray<string>;
  Count: Integer;
begin
  MaterializeAllLazyStringProperties;
  SetLength(Keys, FProperties.Count);
  Count := 0;
  for Key in FProperties.Keys do
  begin
    if IsBytecodePrivateInitializerMarker(Key) or
       IsHiddenRegExpDataProperty(Self, Key) then
      Continue;
    Keys[Count] := Key;
    Inc(Count);
  end;
  SetLength(Keys, Count);
  Result := OrderOwnStringPropertyKeys(Keys);
end;



procedure TGocciaObjectValue.RegisterNativeMethod(const AMethod: TGocciaValue);
var
  OldDescriptor: TGocciaPropertyDescriptor;
begin
  if not (AMethod is TGocciaNativeFunctionValue) then
    raise Exception.Create('Method must be a native function');

  if FProperties.TryGetValue(TGocciaNativeFunctionValue(AMethod).Name, OldDescriptor) then
    OldDescriptor.Free;
  FProperties.Add(TGocciaNativeFunctionValue(AMethod).Name,
    TGocciaPropertyDescriptorData.Create(AMethod, [pfConfigurable, pfWritable]));
end;

procedure TGocciaObjectValue.RegisterConstant(const AName: string; const AValue: TGocciaValue);
var
  OldDescriptor: TGocciaPropertyDescriptor;
begin
  if FProperties.TryGetValue(AName, OldDescriptor) then
    OldDescriptor.Free;
  FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(AValue, []));
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

procedure TGocciaObjectValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  AssignProperty(AName, AValue);
end;

function TGocciaObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
begin
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    Descriptor := MaterializeOwnLazyProperty(AName, Descriptor);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Getter) and Accessor.Getter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.Create;
        try
          Result := TGocciaFunctionBase(Accessor.Getter).Call(Args, AThisContext);
        finally
          Args.Free;
        end;
        Exit;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end;
  end;

  if Assigned(FPrototype) then
  begin
    Descriptor := FPrototype.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
        if Assigned(Accessor.Getter) and Accessor.Getter.IsCallable then
        begin
          Args := TGocciaArgumentsCollection.Create;
          try
            Result := TGocciaFunctionBase(Accessor.Getter).Call(Args, AThisContext);
          finally
            Args.Free;
          end;
          Exit;
        end;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end
      else if Descriptor is TGocciaPropertyDescriptorData then
      begin
        Result := TGocciaPropertyDescriptorData(Descriptor).Value;
        Exit;
      end;
    end;
    Result := FPrototype.GetPropertyWithContext(AName, AThisContext);
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  if FProperties.TryGetValue(AName, Result) then
    Result := MaterializeOwnLazyProperty(AName, Result)
  else
    Result := nil;
end;

function TGocciaObjectValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasOwnProperty(AName);

  if not Result and Assigned(FPrototype) then
    Result := FPrototype.HasProperty(AName);
end;

function TGocciaObjectValue.HasOwnProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := FProperties.TryGetValue(AName, Descriptor);
  if Result then
    MaterializeOwnLazyProperty(AName, Descriptor);
end;

function TGocciaObjectValue.DeleteProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not FProperties.TryGetValue(AName, Descriptor) then
  begin
    Result := True;
    Exit;
  end;
  Descriptor := MaterializeOwnLazyProperty(AName, Descriptor);

  if not Descriptor.Configurable then
  begin
    Result := False;
    Exit;
  end;

  FProperties.Remove(AName);
  Descriptor.Free;
  Result := True;
end;

function TGocciaObjectValue.GetEnumerablePropertyNames: TArray<string>;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Names: TArray<string>;
  Count: Integer;
begin
  MaterializeAllLazyStringProperties;
  SetLength(Names, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable and
       not IsBytecodePrivateInitializerMarker(Pair.Key) then
    begin
      Names[Count] := Pair.Key;
      Inc(Count);
    end;

  SetLength(Names, Count);
  Result := OrderOwnStringPropertyKeys(Names);
end;

function TGocciaObjectValue.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  Descriptor: TGocciaPropertyDescriptor;
  Names: TArray<string>;
  Values: TArray<TGocciaValue>;
  I, Count: Integer;
begin
  Names := GetEnumerablePropertyNames;
  SetLength(Values, Length(Names));
  Count := 0;

  for I := 0 to High(Names) do
  begin
    Descriptor := GetOwnPropertyDescriptor(Names[I]);
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      Values[Count] := GetProperty(Names[I]);
      Inc(Count);
    end;
  end;

  SetLength(Values, Count);
  Result := Values;
end;

function TGocciaObjectValue.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  Descriptor: TGocciaPropertyDescriptor;
  Entries: TArray<TPair<string, TGocciaValue>>;
  Names: TArray<string>;
  I, Count: Integer;
  Entry: TPair<string, TGocciaValue>;
begin
  Names := GetEnumerablePropertyNames;
  SetLength(Entries, Length(Names));
  Count := 0;

  for I := 0 to High(Names) do
  begin
    Descriptor := GetOwnPropertyDescriptor(Names[I]);
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      Entry.Key := Names[I];
      Entry.Value := GetProperty(Names[I]);
      Entries[Count] := Entry;
      Inc(Count);
    end;
  end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetAllPropertyNames: TArray<string>;
var
  Key: string;
  Names: TArray<string>;
  Count: Integer;
begin
  MaterializeAllLazyStringProperties;
  SetLength(Names, FProperties.Count);
  Count := 0;
  for Key in FProperties.Keys do
  begin
    if IsBytecodePrivateInitializerMarker(Key) or
       IsHiddenRegExpDataProperty(Self, Key) then
      Continue;
    Names[Count] := Key;
    Inc(Count);
  end;
  SetLength(Names, Count);
  Result := OrderOwnStringPropertyKeys(Names);
end;

{ Symbol property methods }

// ES2026 §20.1.2.3.1 DefinePropertyOrThrow — symbol variant
// Does NOT take ownership of ADescriptor on failure — caller must free.
procedure TGocciaObjectValue.DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
var
  Current: TGocciaPropertyDescriptor;
  Applied: TGocciaPropertyDescriptor;
begin
  Current := nil;
  FSymbolDescriptors.TryGetValue(ASymbol, Current);
  if not ValidateAndCreatePropertyDescriptor(Current, ADescriptor,
    FExtensible, Applied) then
  begin
    if not Assigned(Current) and not FExtensible then
      ThrowTypeError(SErrorCannotAddSymbolNotExtensible, SSuggestObjectNotExtensible);
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [ASymbol.ToDisplayString.Value]), SSuggestCannotDeleteNonConfigurable);
  end;

  if Assigned(Current) then
    Current.Free
  else
    FSymbolInsertionOrder.Add(ASymbol);

  FSymbolDescriptors.AddOrSetValue(ASymbol, Applied);
  ADescriptor.Free;
end;

// ES2026 §10.1.6.1 OrdinaryDefineOwnProperty — symbol variant
// Takes ownership of ADescriptor — frees it on failure.
function TGocciaObjectValue.TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Current: TGocciaPropertyDescriptor;
  Applied: TGocciaPropertyDescriptor;
begin
  Current := nil;
  FSymbolDescriptors.TryGetValue(ASymbol, Current);
  if not ValidateAndCreatePropertyDescriptor(Current, ADescriptor,
    FExtensible, Applied) then
  begin
    ADescriptor.Free;
    Exit(False);
  end;

  if Assigned(Current) then
    Current.Free
  else
    FSymbolInsertionOrder.Add(ASymbol);
  FSymbolDescriptors.AddOrSetValue(ASymbol, Applied);
  ADescriptor.Free;
  Result := True;
end;

procedure TGocciaObjectValue.AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Current: TGocciaObjectValue;
begin
  if FSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Setter) then
      begin
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          if Accessor.Setter is TGocciaFunctionBase then
            TGocciaFunctionBase(Accessor.Setter).Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end;
      ThrowTypeError(SErrorSetSymbolOnlyGetter, SSuggestPropertyHasOnlyGetter);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        FSymbolDescriptors.AddOrSetValue(ASymbol, TGocciaPropertyDescriptorData.Create(AValue, Descriptor.Flags));
        Descriptor.Free;
        Exit;
      end;
      ThrowTypeError(SErrorReadOnlySymbolProperty, SSuggestCannotDeleteNonConfigurable);
    end;
  end;

  Current := FPrototype;
  while Assigned(Current) do
  begin
    if Current.FSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
    begin
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
        if Assigned(Accessor.Setter) then
        begin
          Args := TGocciaArgumentsCollection.Create;
          try
            Args.Add(AValue);
            if Accessor.Setter is TGocciaFunctionBase then
              TGocciaFunctionBase(Accessor.Setter).Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end;
        ThrowTypeError(SErrorSetSymbolOnlyGetter, SSuggestPropertyHasOnlyGetter);
      end
      else if Descriptor is TGocciaPropertyDescriptorData then
      begin
        if not TGocciaPropertyDescriptorData(Descriptor).Writable then
          ThrowTypeError(SErrorReadOnlySymbolProperty, SSuggestCannotDeleteNonConfigurable);
        Break;
      end;
    end;
    Current := Current.FPrototype;
  end;

  if not FExtensible then
    ThrowTypeError(SErrorCannotAddSymbolNotExtensible, SSuggestObjectNotExtensible);

  DefineSymbolProperty(ASymbol, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc) — symbol variant
function TGocciaObjectValue.AssignSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean;
var
  OwnDesc, ReceiverDesc: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  ReceiverObj: TGocciaObjectValue;
begin
  // Step 1: Let ownDesc be O.[[GetOwnProperty]](P)
  OwnDesc := GetOwnSymbolPropertyDescriptor(ASymbol);

  if OwnDesc = nil then
  begin
    // Step 1a-b: Walk prototype chain
    if Assigned(FPrototype) then
      Exit(FPrototype.AssignSymbolPropertyWithReceiver(ASymbol, AValue, AReceiver));
    // Step 1c: No prototype — ownDesc defaults to writable data descriptor,
    // fall through to step 2 which consults Receiver.[[GetOwnProperty]](P)
    if not (AReceiver is TGocciaObjectValue) then
      Exit(False);
    ReceiverObj := TGocciaObjectValue(AReceiver);
    ReceiverDesc := ReceiverObj.GetOwnSymbolPropertyDescriptor(ASymbol);
    if Assigned(ReceiverDesc) then
    begin
      // Step 2d: Receiver already has own property P
      if (ReceiverDesc is TGocciaPropertyDescriptorAccessor) or
         (not ReceiverDesc.Writable) then
        Exit(False);
      // Step 2d.iii-iv: Receiver.[[DefineOwnProperty]](P, { [[Value]]: V })
      Exit(ReceiverObj.TryDefineSymbolProperty(ASymbol,
        TGocciaPropertyDescriptorData.Create(AValue, ReceiverDesc.Flags)));
    end;
    // Step 2e: CreateDataProperty(Receiver, P, V)
    Exit(ReceiverObj.TryDefineSymbolProperty(ASymbol,
      TGocciaPropertyDescriptorData.Create(AValue,
        [pfEnumerable, pfConfigurable, pfWritable])));
  end;

  if OwnDesc is TGocciaPropertyDescriptorData then
  begin
    // Step 2a: If ownDesc.[[Writable]] is false, return false
    if not OwnDesc.Writable then
      Exit(False);
    // Step 2b: If Receiver is not an Object, return false
    if not (AReceiver is TGocciaObjectValue) then
      Exit(False);
    ReceiverObj := TGocciaObjectValue(AReceiver);
    // Step 2c: Let existingDescriptor be Receiver.[[GetOwnProperty]](P)
    ReceiverDesc := ReceiverObj.GetOwnSymbolPropertyDescriptor(ASymbol);
    if Assigned(ReceiverDesc) then
    begin
      // Step 2d.i: If IsAccessorDescriptor(existingDescriptor), return false
      if ReceiverDesc is TGocciaPropertyDescriptorAccessor then
        Exit(False);
      // Step 2d.ii: If existingDescriptor.[[Writable]] is false, return false
      if not ReceiverDesc.Writable then
        Exit(False);
      // Step 2d.iii-iv: Receiver.[[DefineOwnProperty]](P, { [[Value]]: V })
      Exit(ReceiverObj.TryDefineSymbolProperty(ASymbol,
        TGocciaPropertyDescriptorData.Create(AValue, ReceiverDesc.Flags)));
    end
    else
    begin
      // Step 2e: CreateDataProperty(Receiver, P, V)
      Exit(ReceiverObj.TryDefineSymbolProperty(ASymbol,
        TGocciaPropertyDescriptorData.Create(AValue,
          [pfEnumerable, pfConfigurable, pfWritable])));
    end;
  end;

  // Step 3-7: Accessor descriptor
  Accessor := TGocciaPropertyDescriptorAccessor(OwnDesc);
  // Step 4-5: If setter is undefined, return false
  if not Assigned(Accessor.Setter) or not Accessor.Setter.IsCallable then
    Exit(False);
  // Step 6: Call(setter, Receiver, « V »)
  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(AValue);
    TGocciaFunctionBase(Accessor.Setter).Call(Args, AReceiver);
  finally
    Args.Free;
  end;
  // Step 7: Return true
  Result := True;
end;

function TGocciaObjectValue.GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
begin
  Result := GetSymbolPropertyWithReceiver(ASymbol, Self);
end;

function TGocciaObjectValue.GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
begin
  if FSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Getter) then
      begin
        Args := TGocciaArgumentsCollection.Create;
        try
          if Accessor.Getter is TGocciaFunctionBase then
            Result := TGocciaFunctionBase(Accessor.Getter).Call(Args, AReceiver)
          else
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        finally
          Args.Free;
        end;
        Exit;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  // Check prototype chain
  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetSymbolPropertyWithReceiver(ASymbol, AReceiver);
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectValue.GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  if not FSymbolDescriptors.TryGetValue(ASymbol, Result) then
    Result := nil;
end;

function TGocciaObjectValue.HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := FSymbolDescriptors.ContainsKey(ASymbol);
end;

// ES2026 §10.1.10 [[Delete]](P)
function TGocciaObjectValue.DeleteSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  if not FSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
  begin
    Result := True;
    Exit;
  end;

  if not Descriptor.Configurable then
  begin
    Result := False;
    Exit;
  end;

  FSymbolDescriptors.Remove(ASymbol);
  for I := FSymbolInsertionOrder.Count - 1 downto 0 do
    if FSymbolInsertionOrder[I] = ASymbol then
    begin
      FSymbolInsertionOrder.Delete(I);
      Break;
    end;
  Descriptor.Free;
  Result := True;
end;

function TGocciaObjectValue.GetEnumerableSymbolProperties: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
var
  Entries: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
  Count, I: Integer;
  Symbol: TGocciaSymbolValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  SetLength(Entries, FSymbolInsertionOrder.Count);
  Count := 0;

  for I := 0 to FSymbolInsertionOrder.Count - 1 do
  begin
    Symbol := FSymbolInsertionOrder[I];
    if FSymbolDescriptors.TryGetValue(Symbol, Descriptor) then
    begin
      if Descriptor.Enumerable then
      begin
        if Descriptor is TGocciaPropertyDescriptorData then
        begin
          Entries[Count] := TPair<TGocciaSymbolValue, TGocciaValue>.Create(Symbol, TGocciaPropertyDescriptorData(Descriptor).Value);
          Inc(Count);
        end;
      end;
    end;
  end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetOwnSymbols: TArray<TGocciaSymbolValue>;
var
  Symbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  SetLength(Symbols, FSymbolInsertionOrder.Count);
  for I := 0 to FSymbolInsertionOrder.Count - 1 do
    Symbols[I] := FSymbolInsertionOrder[I];
  Result := Symbols;
end;

procedure TGocciaObjectValue.Freeze;
var
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
  I: Integer;
  Keys: TArray<TGocciaValue>;
begin
  PreventExtensions;
  Keys := OwnPropertyKeyValues(Self);
  for I := 0 to High(Keys) do
  begin
    Descriptor := OwnPropertyDescriptorForKey(Self, Keys[I]);
    if not Assigned(Descriptor) then
      Continue;
    if IsAccessorDescriptor(Descriptor) then
      NewDescriptor := TGocciaPropertyDescriptor.Create([],
        [pdfConfigurable])
    else if IsDataDescriptor(Descriptor) then
      NewDescriptor := TGocciaPropertyDescriptorData.CreatePartial(
        TGocciaPropertyDescriptorData(Descriptor).Value, [],
        [pdfValue, pdfConfigurable, pdfWritable])
    else
      NewDescriptor := TGocciaPropertyDescriptor.Create([],
        [pdfConfigurable]);
    DefineOwnPropertyOrThrowForKey(Self, Keys[I], NewDescriptor);
  end;
  FFrozen := True;
  FSealed := True;
  FExtensible := False;
end;

procedure TGocciaObjectValue.Seal;
var
  I: Integer;
  Keys: TArray<TGocciaValue>;
begin
  PreventExtensions;
  Keys := OwnPropertyKeyValues(Self);
  for I := 0 to High(Keys) do
    DefineOwnPropertyOrThrowForKey(Self, Keys[I],
      TGocciaPropertyDescriptor.Create([], [pdfConfigurable]));
  FSealed := True;
  FExtensible := False;
end;

procedure TGocciaObjectValue.PreventExtensions;
begin
  FExtensible := False;
end;

// ES2026 §7.3.16 TestIntegrityLevel(O, frozen)
function TGocciaObjectValue.TestIntegrityFrozen: Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  ExtensibleCheck: Boolean;
  I: Integer;
  Keys: TArray<TGocciaValue>;
begin
  if Self is TGocciaProxyValue then
    ExtensibleCheck := TGocciaProxyValue(Self).IsExtensibleTrap
  else
    ExtensibleCheck := FExtensible;

  // Step 3: If extensible is true, return false
  if ExtensibleCheck then
    Exit(False);

  // Step 5: For each element key of keys
  Keys := OwnPropertyKeyValues(Self);
  for I := 0 to High(Keys) do
  begin
    Descriptor := OwnPropertyDescriptorForKey(Self, Keys[I]);
    if not Assigned(Descriptor) then
      Continue;
    if Descriptor.Configurable then
      Exit(False);
    if IsDataDescriptor(Descriptor) and Descriptor.Writable then
      Exit(False);
  end;

  // Step 6: Return true
  Result := True;
end;

// ES2026 §7.3.16 TestIntegrityLevel(O, sealed)
function TGocciaObjectValue.TestIntegritySealed: Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  ExtensibleCheck: Boolean;
  I: Integer;
  Keys: TArray<TGocciaValue>;
begin
  if Self is TGocciaProxyValue then
    ExtensibleCheck := TGocciaProxyValue(Self).IsExtensibleTrap
  else
    ExtensibleCheck := FExtensible;

  // Step 3: If extensible is true, return false
  if ExtensibleCheck then
    Exit(False);

  // Step 5: For each element key of keys
  Keys := OwnPropertyKeyValues(Self);
  for I := 0 to High(Keys) do
  begin
    Descriptor := OwnPropertyDescriptorForKey(Self, Keys[I]);
    if Assigned(Descriptor) and Descriptor.Configurable then
      Exit(False);
  end;

  // Step 6: Return true
  Result := True;
end;

initialization
  GObjectPrototypeSlot := RegisterRealmSlot('Object.prototype');
  GObjectMethodHostSlot := RegisterRealmSlot('Object.prototype.methodHost');

end.
