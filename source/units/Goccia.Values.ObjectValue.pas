unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  HashMap,

  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.ObjectModel.Types,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TSymbolDescriptorMap = THashMap<TGocciaSymbolValue, TGocciaPropertyDescriptor>;

  TGocciaObjectValue = class(TGocciaValue)
  private
  protected
    FProperties: TGocciaPropertyMap;
    FSymbolDescriptors: TSymbolDescriptorMap;
    FSymbolInsertionOrder: TList<TGocciaSymbolValue>;
    FPrototype: TGocciaObjectValue;
    FFrozen: Boolean;
    FSealed: Boolean;
    FExtensible: Boolean;
    FHasErrorData: Boolean;
  public
    class procedure InitializeSharedPrototype;
    class function GetSharedObjectPrototype: TGocciaObjectValue; static;
    class procedure SetSharedObjectPrototype(const AValue: TGocciaObjectValue); static;
    class property SharedObjectPrototype: TGocciaObjectValue read GetSharedObjectPrototype write SetSharedObjectPrototype;
    constructor Create(const APrototype: TGocciaObjectValue = nil;
      const APropertyCapacity: Integer = 0);
    destructor Destroy; override;
    function ToDebugString: string;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToStringTag: string; virtual;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function CanContainEscapedReferences: Boolean; override;

    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); virtual;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; virtual;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); virtual;
    function AssignPropertyWithReceiver(const AName: string; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean; virtual;

    procedure RegisterNativeMethod(const AMethod: TGocciaValue);
    procedure RegisterConstant(const AName: string; const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; virtual;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; virtual;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean; virtual;
    function DeleteProperty(const AName: string): Boolean; virtual;

    function GetEnumerablePropertyNames: TArray<string>; virtual;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; virtual;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; virtual;
    function GetAllPropertyNames: TArray<string>; virtual;
    function GetOwnPropertyNames: TArray<string>; virtual;
    function GetOwnPropertyKeys: TArray<string>; virtual;

    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
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
    function MarkEscapedReferencesIn(const AVisited: TGCObjectSet): Boolean; override;

    property Properties: TGocciaPropertyMap read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
    property Frozen: Boolean read FFrozen;
    property Sealed: Boolean read FSealed;
    property Extensible: Boolean read FExtensible;
    property HasErrorData: Boolean read FHasErrorData write FHasErrorData;
  published
    function ObjectPrototypeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeIsPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypePropertyIsEnumerable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPrototypeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;


implementation

uses
  SysUtils,

  StringBuffer,

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction;

// Object.prototype lives in a per-realm slot.  See Goccia.Realm and the
// matching pattern in Goccia.Values.ArrayValue for the rationale; the method
// host and member definitions stay in process-wide threadvar caches because
// they are immutable across realms.
var
  GObjectPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

const
  MAX_PROTOTYPE_CHAIN_DEPTH = 256;

procedure MarkPropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor);
begin
  ADescriptor.MarkValues;
end;

{ TGocciaObjectValue }

class function TGocciaObjectValue.GetSharedObjectPrototype: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GObjectPrototypeSlot))
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
  FProperties := TGocciaPropertyMap.Create(APropertyCapacity);
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
begin
  if AThisValue is TGocciaUndefinedLiteralValue then
    Exit(TGocciaStringLiteralValue.Create('[object Undefined]'));
  if AThisValue is TGocciaNullLiteralValue then
    Exit(TGocciaStringLiteralValue.Create('[object Null]'));

  if AThisValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AThisValue);
    SymbolTag := Obj.GetSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag);
    if Assigned(SymbolTag) and (SymbolTag is TGocciaStringLiteralValue) then
      Tag := TGocciaStringLiteralValue(SymbolTag).Value
    else if AThisValue.IsCallable then
      Tag := 'Function'
    else
      Tag := Obj.ToStringTag;
  end
  else if AThisValue is TGocciaBooleanLiteralValue then
    Tag := CONSTRUCTOR_BOOLEAN
  else if AThisValue is TGocciaNumberLiteralValue then
    Tag := CONSTRUCTOR_NUMBER
  else if AThisValue is TGocciaStringLiteralValue then
    Tag := CONSTRUCTOR_STRING
  else if AThisValue is TGocciaSymbolValue then
    Tag := CONSTRUCTOR_SYMBOL
  else
    Tag := CONSTRUCTOR_OBJECT;

  Result := TGocciaStringLiteralValue.Create('[object ' + Tag + ']');
end;

// ES2026 §20.1.3.3 Object.prototype.isPrototypeOf(V)
function TGocciaObjectValue.ObjectPrototypeIsPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  V, Current: TGocciaObjectValue;
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
  Current := V.FPrototype;
  while Assigned(Current) do
  begin
    // Step 3a: If SameValue(O, V.[[Prototype]]) is true, return true
    if Current = AThisValue then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Current := Current.FPrototype;
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.3.4 Object.prototype.propertyIsEnumerable(V)
function TGocciaObjectValue.ObjectPrototypePropertyIsEnumerable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Key: string;
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyArg: TGocciaValue;
begin
  // Step 2: Let O be ? ToObject(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);

  if not (AThisValue is TGocciaObjectValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  Obj := TGocciaObjectValue(AThisValue);

  // Step 1: Let P be ? ToPropertyKey(V)
  if AArgs.Length = 0 then
    PropertyArg := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    PropertyArg := AArgs.GetElement(0);

  if PropertyArg is TGocciaSymbolValue then
  begin
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(PropertyArg));
    if not Assigned(Descriptor) then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    if Descriptor.Enumerable then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
  begin
    Key := PropertyArg.ToStringLiteral.Value;
    // Step 3: Let desc be ? O.[[GetOwnProperty]](P)
    Descriptor := Obj.GetOwnPropertyDescriptor(Key);
    // Step 4: If desc is undefined, return false
    if not Assigned(Descriptor) then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    // Step 5: Return desc.[[Enumerable]]
    if Descriptor.Enumerable then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

// ES2026 §20.1.3.5 Object.prototype.toLocaleString()
function TGocciaObjectValue.ObjectPrototypeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ToStringMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Step 1: Let O be the this value
  // Step 2: Return ? Invoke(O, "toString")
  if AThisValue is TGocciaObjectValue then
  begin
    ToStringMethod := TGocciaObjectValue(AThisValue).GetProperty(PROP_TO_STRING);
    if Assigned(ToStringMethod) and ToStringMethod.IsCallable then
    begin
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        if ToStringMethod is TGocciaFunctionBase then
          Result := TGocciaFunctionBase(ToStringMethod).Call(CallArgs, AThisValue)
        else
          Result := AThisValue.ToStringLiteral;
      finally
        CallArgs.Free;
      end;
    end
    else
      Result := AThisValue.ToStringLiteral;
  end
  else
    Result := AThisValue.ToStringLiteral;
end;

// ES2026 §20.1.3.7 Object.prototype.valueOf()
function TGocciaObjectValue.ObjectPrototypeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return ? ToObject(this value)
  Result := AThisValue;
end;

class procedure TGocciaObjectValue.InitializeSharedPrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  // No realm yet - very early bootstrap path.  Subsequent constructor runs
  // under an assigned realm will retry.
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedObjectPrototype) then Exit;

  if not Assigned(FPrototypeMethodHost) then
    FPrototypeMethodHost := TGocciaObjectValue.Create;
  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GObjectPrototypeSlot, SharedPrototype);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(FPrototypeMethodHost.ObjectPrototypeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(FPrototypeMethodHost.ObjectPrototypeIsPrototypeOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(FPrototypeMethodHost.ObjectPrototypePropertyIsEnumerable, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(FPrototypeMethodHost.ObjectPrototypeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(FPrototypeMethodHost.ObjectPrototypeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    FPrototypeMembers[0].ExposedName := PROP_TO_STRING;
    FPrototypeMembers[1].ExposedName := PROP_IS_PROTOTYPE_OF;
    FPrototypeMembers[2].ExposedName := PROP_PROPERTY_IS_ENUMERABLE;
    FPrototypeMembers[3].ExposedName := PROP_TO_LOCALE_STRING;
    FPrototypeMembers[4].ExposedName := PROP_VALUE_OF;
  end;
  RegisterMemberDefinitions(SharedPrototype, FPrototypeMembers);

  // SharedPrototype is pinned via the realm slot; the method host is a
  // process-wide singleton (immutable across realms) that we pin directly.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
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
  inherited;
end;

procedure TGocciaObjectValue.MarkReferences;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymPair: TSymbolDescriptorMap.TKeyValuePair;
  GC: TGarbageCollector;
begin
  if GCMarked then Exit;
  inherited;

  GC := TGarbageCollector.Instance;
  if Assigned(FPrototype) and
     ((not Assigned(GC)) or GC.ContainsObject(FPrototype)) then
    FPrototype.MarkReferences;

  if Assigned(FProperties) then
    for Pair in FProperties do
      if Assigned(Pair.Value) then
        Pair.Value.MarkValues;

  if Assigned(FSymbolDescriptors) then
    for SymPair in FSymbolDescriptors do
    begin
      if Assigned(SymPair.Key) then
        SymPair.Key.MarkReferences;
      if Assigned(SymPair.Value) then
        SymPair.Value.MarkValues;
    end;
end;

function TGocciaObjectValue.MarkEscapedReferencesIn(
  const AVisited: TGCObjectSet): Boolean;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymPair: TSymbolDescriptorMap.TKeyValuePair;
begin
  Result := inherited MarkEscapedReferencesIn(AVisited);
  if not Result then
    Exit;

  if Assigned(FProperties) then
    for Pair in FProperties do
      if Assigned(Pair.Value) then
        Pair.Value.MarkEscapedReferences(AVisited);

  if Assigned(FSymbolDescriptors) then
    for SymPair in FSymbolDescriptors do
    begin
      if Assigned(SymPair.Key) then
        SymPair.Key.MarkEscapedReferencesIn(AVisited);
      if Assigned(SymPair.Value) then
        SymPair.Value.MarkEscapedReferences(AVisited);
    end;

  if Assigned(FPrototype) then
    FPrototype.MarkEscapedReferencesIn(AVisited);
end;

function TGocciaObjectValue.ToDebugString: string;
var
  SB: TStringBuffer;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  First: Boolean;
  Value: TGocciaValue;
begin
  SB := TStringBuffer.Create;
  SB.AppendChar('{');
  First := True;

  for Pair in FProperties do
  begin
    if not First then
      SB.Append(', ');

    if Pair.Value is TGocciaPropertyDescriptorData then
      Value := TGocciaPropertyDescriptorData(Pair.Value).Value
    else
      Value := nil;

    if Assigned(Value) then
    begin
      SB.Append(Pair.Key);
      SB.Append(': ');
      if Value is TGocciaObjectValue then
        SB.Append(TGocciaObjectValue(Value).ToDebugString)
      else if Value is TGocciaSymbolValue then
        SB.Append(TGocciaSymbolValue(Value).ToDisplayString.Value)
      else
        SB.Append(Value.ToStringLiteral.Value);
    end
    else
    begin
      SB.Append(Pair.Key);
      SB.Append(': [accessor]');
    end;

    First := False;
  end;

  if Assigned(FPrototype) then
  begin
    if not First then
      SB.Append(', ');
    SB.Append('[[Prototype]]: ');
    SB.Append(FPrototype.ToDebugString);
  end;

  SB.AppendChar('}');
  Result := SB.ToString;
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

function TGocciaObjectValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(Format('[%s %s]', [TypeName, ToStringTag]));
end;

function TGocciaObjectValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaObjectValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaObjectValue.CanContainEscapedReferences: Boolean;
begin
  Result := True;
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
  if FFrozen then
    ThrowTypeError(Format(SErrorReadOnlyPropertyFrozen, [AName]), SSuggestCannotDeleteNonConfigurable);

  if FProperties.TryGetValue(AName, Descriptor) then
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
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        if Assigned(AValue) and AValue.CanContainEscapedReferences then
          AValue.MarkEscapedReferences;
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
    if Proto.FProperties.TryGetValue(AName, Descriptor) then
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

// ES2026 §10.1.6.3 ValidateAndApplyPropertyDescriptor
// Returns True if the new descriptor is compatible with current.
// Does NOT free or apply anything — pure validation.
function ValidatePropertyDescriptor(
  const ACurrent: TGocciaPropertyDescriptor;
  const ANew: TGocciaPropertyDescriptor
): Boolean;
var
  CurrentData: TGocciaPropertyDescriptorData;
  CurrentAccessor: TGocciaPropertyDescriptorAccessor;
  NewData: TGocciaPropertyDescriptorData;
  NewAccessor: TGocciaPropertyDescriptorAccessor;
begin
  Result := True;
  if ACurrent.Configurable then
    Exit;

  // §10.1.6.3 step 3a: Cannot make configurable
  if ANew.Configurable then
    Exit(False);

  // 3b: Cannot change enumerable
  if ACurrent.Enumerable <> ANew.Enumerable then
    Exit(False);

  // 3c: Cannot change descriptor kind (data ↔ accessor)
  if (ACurrent is TGocciaPropertyDescriptorData) <> (ANew is TGocciaPropertyDescriptorData) then
    Exit(False);

  // 3d: Non-configurable accessor — cannot change get or set
  if (ACurrent is TGocciaPropertyDescriptorAccessor) and
     (ANew is TGocciaPropertyDescriptorAccessor) then
  begin
    CurrentAccessor := TGocciaPropertyDescriptorAccessor(ACurrent);
    NewAccessor := TGocciaPropertyDescriptorAccessor(ANew);
    if (CurrentAccessor.Getter <> NewAccessor.Getter) or
       (CurrentAccessor.Setter <> NewAccessor.Setter) then
      Exit(False);
  end;

  // 3e: Non-configurable data — validate writable/value changes
  if (ACurrent is TGocciaPropertyDescriptorData) and
     (ANew is TGocciaPropertyDescriptorData) then
  begin
    CurrentData := TGocciaPropertyDescriptorData(ACurrent);
    NewData := TGocciaPropertyDescriptorData(ANew);
    if not CurrentData.Writable then
    begin
      // 3e.i: Cannot make writable once non-writable
      if NewData.Writable then
        Exit(False);
      // 3e.ii: Cannot change value
      if not IsSameValue(CurrentData.Value, NewData.Value) then
        Exit(False);
    end;
  end;
end;

procedure MarkDescriptorEscapedReferences(
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Visited: TGCObjectSet;
begin
  Visited := TGCObjectSet.Create;
  try
    ADescriptor.MarkEscapedReferences(Visited);
  finally
    Visited.Free;
  end;
end;

// ES2026 §20.1.2.3.1 DefinePropertyOrThrow(O, P, desc)
// Does NOT take ownership of ADescriptor on failure — caller must free.
procedure TGocciaObjectValue.DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  Current: TGocciaPropertyDescriptor;
begin
  if FProperties.TryGetValue(AName, Current) then
  begin
    if not ValidatePropertyDescriptor(Current, ADescriptor) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]), SSuggestCannotDeleteNonConfigurable);
    MarkDescriptorEscapedReferences(ADescriptor);
    Current.Free;
  end
  else if not FExtensible then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]), SSuggestObjectNotExtensible)
  else
    MarkDescriptorEscapedReferences(ADescriptor);

  FProperties.Add(AName, ADescriptor);
end;

// ES2026 §10.1.6.1 OrdinaryDefineOwnProperty(O, P, Desc)
// Takes ownership of ADescriptor — frees it on failure.
function TGocciaObjectValue.TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Current: TGocciaPropertyDescriptor;
begin
  if not FProperties.TryGetValue(AName, Current) then
  begin
    if not FExtensible then
    begin
      ADescriptor.Free;
      Exit(False);
    end;
    MarkDescriptorEscapedReferences(ADescriptor);
    FProperties.Add(AName, ADescriptor);
    Exit(True);
  end;

  if not ValidatePropertyDescriptor(Current, ADescriptor) then
  begin
    ADescriptor.Free;
    Exit(False);
  end;

  MarkDescriptorEscapedReferences(ADescriptor);
  Current.Free;
  FProperties.Add(AName, ADescriptor);
  Result := True;
end;

function TGocciaObjectValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaObjectValue.GetOwnPropertyKeys: TArray<string>;
begin
  Result := FProperties.Keys;
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
  if not FProperties.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaObjectValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasOwnProperty(AName);

  if not Result and Assigned(FPrototype) then
    Result := FPrototype.HasProperty(AName);
end;

function TGocciaObjectValue.HasOwnProperty(const AName: string): Boolean;
begin
  Result := FProperties.ContainsKey(AName);
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
  SetLength(Names, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable then
    begin
      Names[Count] := Pair.Key;
      Inc(Count);
    end;

  SetLength(Names, Count);
  Result := Names;
end;

function TGocciaObjectValue.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Values: TArray<TGocciaValue>;
  Count: Integer;
begin
  SetLength(Values, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Values[Count] := TGocciaPropertyDescriptorData(Pair.Value).Value;
      Inc(Count);
    end;

  SetLength(Values, Count);
  Result := Values;
end;

function TGocciaObjectValue.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Entries: TArray<TPair<string, TGocciaValue>>;
  Count: Integer;
  Entry: TPair<string, TGocciaValue>;
begin
  SetLength(Entries, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Entry.Key := Pair.Key;
      Entry.Value := TGocciaPropertyDescriptorData(Pair.Value).Value;
      Entries[Count] := Entry;
      Inc(Count);
    end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetAllPropertyNames: TArray<string>;
begin
  Result := FProperties.Keys;
end;

{ Symbol property methods }

// ES2026 §20.1.2.3.1 DefinePropertyOrThrow — symbol variant
// Does NOT take ownership of ADescriptor on failure — caller must free.
procedure TGocciaObjectValue.DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
var
  Current: TGocciaPropertyDescriptor;
begin
  if FSymbolDescriptors.TryGetValue(ASymbol, Current) then
  begin
    if not ValidatePropertyDescriptor(Current, ADescriptor) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [ASymbol.ToDisplayString.Value]), SSuggestCannotDeleteNonConfigurable);
    MarkDescriptorEscapedReferences(ADescriptor);
    Current.Free;
  end
  else if not FExtensible then
    ThrowTypeError(SErrorCannotAddSymbolNotExtensible, SSuggestObjectNotExtensible)
  else
  begin
    MarkDescriptorEscapedReferences(ADescriptor);
    FSymbolInsertionOrder.Add(ASymbol);
  end;

  FSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
end;

// ES2026 §10.1.6.1 OrdinaryDefineOwnProperty — symbol variant
// Takes ownership of ADescriptor — frees it on failure.
function TGocciaObjectValue.TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Current: TGocciaPropertyDescriptor;
begin
  if not FSymbolDescriptors.TryGetValue(ASymbol, Current) then
  begin
    if not FExtensible then
    begin
      ADescriptor.Free;
      Exit(False);
    end;
    MarkDescriptorEscapedReferences(ADescriptor);
    FSymbolInsertionOrder.Add(ASymbol);
    FSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
    Exit(True);
  end;

  if not ValidatePropertyDescriptor(Current, ADescriptor) then
  begin
    ADescriptor.Free;
    Exit(False);
  end;

  MarkDescriptorEscapedReferences(ADescriptor);
  Current.Free;
  FSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
  Result := True;
end;

procedure TGocciaObjectValue.AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Current: TGocciaObjectValue;
begin
  if FFrozen then
    ThrowTypeError(SErrorCannotAssignFrozenObject, SSuggestCannotDeleteNonConfigurable);

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
        if Assigned(AValue) and AValue.CanContainEscapedReferences then
          AValue.MarkEscapedReferences;
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
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
begin
  for Pair in FProperties do
  begin
    Descriptor := Pair.Value;
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if Descriptor.Enumerable then
        NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, [pfEnumerable])
      else
        NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, []);
      FProperties.Add(Pair.Key, NewDescriptor);
      Descriptor.Free;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if Descriptor.Enumerable then
        NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
          TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
          TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
          [pfEnumerable])
      else
        NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
          TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
          TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
          []);
      FProperties.Add(Pair.Key, NewDescriptor);
      Descriptor.Free;
    end;
  end;
  FFrozen := True;
  FSealed := True;
  FExtensible := False;
end;

procedure TGocciaObjectValue.Seal;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
  Flags: TPropertyFlags;
begin
  for Pair in FProperties do
  begin
    Descriptor := Pair.Value;
    Flags := [];
    if Descriptor.Enumerable then
      Include(Flags, pfEnumerable);
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if Descriptor.Writable then
        Include(Flags, pfWritable);
      NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, Flags);
      FProperties.Add(Pair.Key, NewDescriptor);
      Descriptor.Free;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
        TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Flags);
      FProperties.Add(Pair.Key, NewDescriptor);
      Descriptor.Free;
    end;
  end;
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
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymPair: TSymbolDescriptorMap.TKeyValuePair;
begin
  // Step 3: If extensible is true, return false
  if FExtensible then
    Exit(False);

  // Step 5: For each element key of keys
  for Pair in FProperties do
  begin
    if Pair.Value.Configurable then
      Exit(False);
    if (Pair.Value is TGocciaPropertyDescriptorData) and Pair.Value.Writable then
      Exit(False);
  end;

  for SymPair in FSymbolDescriptors do
  begin
    if SymPair.Value.Configurable then
      Exit(False);
    if (SymPair.Value is TGocciaPropertyDescriptorData) and SymPair.Value.Writable then
      Exit(False);
  end;

  // Step 6: Return true
  Result := True;
end;

// ES2026 §7.3.16 TestIntegrityLevel(O, sealed)
function TGocciaObjectValue.TestIntegritySealed: Boolean;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymPair: TSymbolDescriptorMap.TKeyValuePair;
begin
  // Step 3: If extensible is true, return false
  if FExtensible then
    Exit(False);

  // Step 5: For each element key of keys
  for Pair in FProperties do
  begin
    if Pair.Value.Configurable then
      Exit(False);
  end;

  for SymPair in FSymbolDescriptors do
  begin
    if SymPair.Value.Configurable then
      Exit(False);
  end;

  // Step 6: Return true
  Result := True;
end;

initialization
  GObjectPrototypeSlot := RegisterRealmSlot('Object.prototype');

end.
