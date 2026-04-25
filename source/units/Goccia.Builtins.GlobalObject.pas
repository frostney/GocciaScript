unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  private
  protected
    // Native methods
  published
    function ObjectIs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyNames(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptors(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperties(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertySymbols(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectFreeze(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectIsFrozen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectFromEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectSeal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectIsSealed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectIsExtensible(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectSetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Generics.Collections,

  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

// ES2026 §6.2.6.4 FromPropertyDescriptor(Desc)
function FromPropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  if ADescriptor.Enumerable then
    Result.AssignProperty(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.TrueValue)
  else
    Result.AssignProperty(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.FalseValue);
  if ADescriptor.Configurable then
    Result.AssignProperty(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.TrueValue)
  else
    Result.AssignProperty(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.FalseValue);

  if ADescriptor is TGocciaPropertyDescriptorData then
  begin
    Result.AssignProperty(PROP_VALUE, TGocciaPropertyDescriptorData(ADescriptor).Value);
    if ADescriptor.Writable then
      Result.AssignProperty(PROP_WRITABLE, TGocciaBooleanLiteralValue.TrueValue)
    else
      Result.AssignProperty(PROP_WRITABLE, TGocciaBooleanLiteralValue.FalseValue);
  end
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
      Result.AssignProperty(PROP_GET, TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
    else
      Result.AssignProperty(PROP_GET, TGocciaUndefinedLiteralValue.UndefinedValue);

    if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
      Result.AssignProperty(PROP_SET, TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
    else
      Result.AssignProperty(PROP_SET, TGocciaUndefinedLiteralValue.UndefinedValue);
  end;
end;

constructor TGocciaGlobalObject.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(ObjectIs, 2, gmkStaticMethod);
    Members.AddMethod(ObjectKeys, 1, gmkStaticMethod);
    Members.AddMethod(ObjectValues, 1, gmkStaticMethod);
    Members.AddMethod(ObjectEntries, 1, gmkStaticMethod);
    Members.AddMethod(ObjectAssign, 2, gmkStaticMethod);
    Members.AddMethod(ObjectCreate, 2, gmkStaticMethod);
    Members.AddMethod(ObjectHasOwn, 2, gmkStaticMethod);
    Members.AddMethod(ObjectGetOwnPropertyNames, 1, gmkStaticMethod);
    Members.AddMethod(ObjectGetOwnPropertyDescriptor, 2, gmkStaticMethod);
    Members.AddMethod(ObjectGetOwnPropertyDescriptors, 1, gmkStaticMethod);
    Members.AddMethod(ObjectDefineProperty, 3, gmkStaticMethod);
    Members.AddMethod(ObjectDefineProperties, 2, gmkStaticMethod);
    Members.AddMethod(ObjectGetOwnPropertySymbols, 1, gmkStaticMethod);
    Members.AddMethod(ObjectFreeze, 1, gmkStaticMethod);
    Members.AddMethod(ObjectIsFrozen, 1, gmkStaticMethod);
    Members.AddMethod(ObjectGetPrototypeOf, 1, gmkStaticMethod);
    Members.AddMethod(ObjectFromEntries, 1, gmkStaticMethod);
    Members.AddMethod(ObjectSeal, 1, gmkStaticMethod);
    Members.AddMethod(ObjectIsSealed, 1, gmkStaticMethod);
    Members.AddMethod(ObjectPreventExtensions, 1, gmkStaticMethod);
    Members.AddMethod(ObjectIsExtensible, 1, gmkStaticMethod);
    Members.AddMethod(ObjectSetPrototypeOf, 2, gmkStaticMethod);
    Members.AddMethod(ObjectGroupBy, 2, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// ES2026 §20.1.2.10 Object.is(value1, value2)
function TGocciaGlobalObject.ObjectIs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.is', ThrowError);

  Left := AArgs.GetElement(0);
  Right := AArgs.GetElement(1);

  // Step 1: Return SameValue(value1, value2)
  if IsSameValue(Left, Right) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;


// ES2026 §20.1.2.17 Object.keys(O)
function TGocciaGlobalObject.ObjectKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.keys', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    Keys := TGocciaArrayValue.Create;

    // Step 2: Let nameList be ? EnumerableOwnProperties(obj, key)
    Names := Obj.GetEnumerablePropertyNames;
    // Step 3: Return CreateArrayFromList(nameList)
    for I := 0 to High(Names) do
      Keys.Elements.Add(TGocciaStringLiteralValue.Create(Names[I]));

    Result := Keys;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.22 Object.values(O)
function TGocciaGlobalObject.ObjectValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  PropertyValues: TArray<TGocciaValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.values', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    Values := TGocciaArrayValue.Create;

    // Step 2: Let nameList be ? EnumerableOwnProperties(obj, value)
    PropertyValues := Obj.GetEnumerablePropertyValues;
    // Step 3: Return CreateArrayFromList(nameList)
    for I := 0 to High(PropertyValues) do
      Values.Elements.Add(PropertyValues[I]);

    Result := Values;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.5 Object.entries(O)
function TGocciaGlobalObject.ObjectEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.entries', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    Entries := TGocciaArrayValue.Create;

    // Step 2: Let nameList be ? EnumerableOwnProperties(obj, key+value)
    PropertyEntries := Obj.GetEnumerablePropertyEntries;
    // Step 3: Return CreateArrayFromList(nameList) — each entry is a [key, value] pair
    for I := 0 to High(PropertyEntries) do
    begin
      Entry := TGocciaArrayValue.Create;
      Entry.Elements.Add(TGocciaStringLiteralValue.Create(PropertyEntries[I].Key));
      Entry.Elements.Add(PropertyEntries[I].Value);
      Entries.Elements.Add(Entry);
    end;

    Result := Entries;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.1 Object.assign(target, ...sources)
function TGocciaGlobalObject.ObjectAssign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I, J: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.assign', ThrowError);

  // Step 1: Let to be ? ToObject(target)
  InitialObj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(InitialObj);
  try
    // Step 2: For each element nextSource of sources
    for I := 1 to AArgs.Length - 1 do
    begin
      if (AArgs.GetElement(I) is TGocciaObjectValue) then
      begin
        Source := TGocciaObjectValue(AArgs.GetElement(I));

        // Step 3: Let keys be ? EnumerableOwnProperties(nextSource, key+value)
        PropertyEntries := Source.GetEnumerablePropertyEntries;
        // Step 4: For each element key, Get value and Set on target
        for J := 0 to High(PropertyEntries) do
          InitialObj.AssignProperty(PropertyEntries[J].Key, PropertyEntries[J].Value);
      end;
    end;

    // Step 5: Return to
    Result := InitialObj;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(InitialObj);
  end;
end;

// ES2026 §20.1.2.2 Object.create(O [, Properties])
function TGocciaGlobalObject.ObjectCreate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewObj: TGocciaObjectValue;
  ProtoArg, PropsArg: TGocciaValue;
  DefineArgs: TGocciaArgumentsCollection;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.create', ThrowError);

  ProtoArg := AArgs.GetElement(0);

  // Step 1: If Type(O) is neither Object nor Null, throw a TypeError exception
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorObjectCreateCalledOnNonObject, SSuggestObjectArgType);

  // Step 2: Let obj be OrdinaryObjectCreate(O)
  if ProtoArg is TGocciaObjectValue then
    NewObj := TGocciaObjectValue.Create(TGocciaObjectValue(ProtoArg))
  else
    NewObj := TGocciaObjectValue.Create(nil);

  // Step 3: If Properties is not undefined, perform ObjectDefineProperties(obj, Properties)
  if AArgs.Length >= 2 then
  begin
    PropsArg := AArgs.GetElement(1);
    if not (PropsArg is TGocciaUndefinedLiteralValue) then
    begin
      DefineArgs := TGocciaArgumentsCollection.CreateWithCapacity(2);
      try
        DefineArgs.Add(NewObj);
        DefineArgs.Add(PropsArg);
        ObjectDefineProperties(DefineArgs, AThisValue);
      finally
        DefineArgs.Free;
      end;
    end;
  end;

  // Step 4: Return obj
  Result := NewObj;
end;

// ES2026 §20.1.2.9 Object.hasOwn(O, P)
function TGocciaGlobalObject.ObjectHasOwn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  ClassObj: TGocciaClassValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.hasOwn', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  if AArgs.GetElement(0) is TGocciaClassValue then
  begin
    ClassObj := TGocciaClassValue(AArgs.GetElement(0));
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Return ? HasOwnProperty(obj, key)
    if AArgs.GetElement(1) is TGocciaSymbolValue then
    begin
      if not (ClassObj.GetSymbolProperty(TGocciaSymbolValue(AArgs.GetElement(1))) is TGocciaUndefinedLiteralValue) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;
      if not (ClassObj.GetProperty(PropertyName) is TGocciaUndefinedLiteralValue) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
    Exit;
  end;

  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Return ? HasOwnProperty(obj, key)
    if AArgs.GetElement(1) is TGocciaSymbolValue then
    begin
      if Obj.HasSymbolProperty(TGocciaSymbolValue(AArgs.GetElement(1))) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;
      if Obj.HasOwnProperty(PropertyName) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.8 Object.getOwnPropertyNames(O)
function TGocciaGlobalObject.ObjectGetOwnPropertyNames(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Names: TGocciaArrayValue;
  PropertyNames: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.getOwnPropertyNames', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    Names := TGocciaArrayValue.Create;

    // Step 1: Return GetOwnPropertyKeys(O, string)
    PropertyNames := Obj.GetAllPropertyNames;
    for I := 0 to High(PropertyNames) do
      Names.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));

    Result := Names;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.6 Object.getOwnPropertyDescriptor(O, P)
function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.getOwnPropertyDescriptor', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Let desc be ? O.[[GetOwnProperty]](key)
    if AArgs.GetElement(1) is TGocciaSymbolValue then
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AArgs.GetElement(1)))
    else
    begin
      PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;
      Descriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
    end;
    // Step 4: Return FromPropertyDescriptor(desc)
    if Descriptor = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      Result := FromPropertyDescriptor(Descriptor);
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.7 Object.getOwnPropertyDescriptors(O)
function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptors(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj, Descriptors: TGocciaObjectValue;
  PropertyNames: TArray<string>;
  OwnSymbols: TArray<TGocciaSymbolValue>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertyDescriptors', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let ownKeys be ? obj.[[OwnPropertyKeys]]()
    // Step 3: Let descriptors be OrdinaryObjectCreate(%Object.prototype%)
    Descriptors := TGocciaObjectValue.Create;

    // Step 4: For each element key of ownKeys (string keys)
    PropertyNames := Obj.GetAllPropertyNames;
    for I := 0 to High(PropertyNames) do
    begin
      // Step 4a: Let desc be ? obj.[[GetOwnProperty]](key)
      Descriptor := Obj.GetOwnPropertyDescriptor(PropertyNames[I]);
      // Step 4b-c: Let descriptor be FromPropertyDescriptor(desc) and add to result
      if Descriptor <> nil then
        Descriptors.AssignProperty(PropertyNames[I], FromPropertyDescriptor(Descriptor));
    end;

    // Step 4 continued: For each element key of ownKeys (symbol keys)
    OwnSymbols := Obj.GetOwnSymbols;
    for I := 0 to High(OwnSymbols) do
    begin
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(OwnSymbols[I]);
      if Descriptor <> nil then
        Descriptors.AssignSymbolProperty(OwnSymbols[I], FromPropertyDescriptor(Descriptor));
    end;

    // Step 5: Return descriptors
    Result := Descriptors;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.3 Object.defineProperty(O, P, Attributes)
function TGocciaGlobalObject.ObjectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  DescriptorObject: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  IsSymbolKey: Boolean;
  SymbolKey: TGocciaSymbolValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 3, 'Object.defineProperty', ThrowError);

  // Step 1: If Type(O) is not Object, throw a TypeError exception
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertyCalledOnNonObject, SSuggestObjectArgType);

  if not (AArgs.GetElement(2) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertyDescriptorMustBeObject, SSuggestObjectArgType);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  DescriptorObject := TGocciaObjectValue(AArgs.GetElement(2));

  // Step 2: Let key be ? ToPropertyKey(P)
  IsSymbolKey := AArgs.GetElement(1) is TGocciaSymbolValue;
  SymbolKey := nil;
  PropertyName := '';
  ExistingDescriptor := nil;

  if IsSymbolKey then
  begin
    SymbolKey := TGocciaSymbolValue(AArgs.GetElement(1));
    ExistingDescriptor := Obj.GetOwnSymbolPropertyDescriptor(SymbolKey);
  end
  else
  begin
    PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;
    ExistingDescriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  end;

  // Step 3: Let desc be ? ToPropertyDescriptor(Attributes)
  Descriptor := ToPropertyDescriptor(DescriptorObject, ExistingDescriptor);

  // Step 4: Perform ? DefinePropertyOrThrow(O, key, desc)
  // DefineProperty/DefineSymbolProperty takes ownership on success;
  // free Descriptor and re-raise if the call throws.
  try
    if IsSymbolKey then
      Obj.DefineSymbolProperty(SymbolKey, Descriptor)
    else
      Obj.DefineProperty(PropertyName, Descriptor);
  except
    Descriptor.Free;
    raise;
  end;

  // Step 5: Return O
  Result := Obj;
end;

// ES2026 §20.1.2.2 Object.defineProperties(O, Properties)
function TGocciaGlobalObject.ObjectDefineProperties(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertiesDescriptor: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
  SymbolDesc: TGocciaPropertyDescriptor;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.defineProperties', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertiesCalledOnNonObject, SSuggestObjectArgType);

  // Step 1: Let props be ? ToObject(Properties)
  if not (AArgs.GetElement(1) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertiesMustBeObject, SSuggestObjectArgType);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  PropertiesDescriptor := TGocciaObjectValue(AArgs.GetElement(1));
  // Step 2: Let keys be ? props.[[OwnPropertyKeys]]()
  PropertyEntries := PropertiesDescriptor.GetEnumerablePropertyEntries;

  // Step 3: For each string key, ToPropertyDescriptor and DefinePropertyOrThrow
  for I := 0 to High(PropertyEntries) do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Obj);
      CallArgs.Add(TGocciaStringLiteralValue.Create(PropertyEntries[I].Key));
      CallArgs.Add(PropertyEntries[I].Value);
      ObjectDefineProperty(CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
  end;

  // Step 3 (cont): Also process symbol-keyed descriptors per §20.1.2.3
  // Use Get(props, key) to obtain the descriptor value — handles both data
  // properties and accessor properties (invoking the getter if present).
  SymbolKeys := PropertiesDescriptor.GetOwnSymbols;
  for I := 0 to High(SymbolKeys) do
  begin
    SymbolDesc := PropertiesDescriptor.GetOwnSymbolPropertyDescriptor(SymbolKeys[I]);
    if Assigned(SymbolDesc) and SymbolDesc.Enumerable then
    begin
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        CallArgs.Add(Obj);
        CallArgs.Add(SymbolKeys[I]);
        // Get(props, key): invoke getter for accessors, read value for data
        CallArgs.Add(PropertiesDescriptor.GetSymbolProperty(SymbolKeys[I]));
        ObjectDefineProperty(CallArgs, AThisValue);
      finally
        CallArgs.Free;
      end;
    end;
  end;

  Result := Obj;
end;

// ES2026 §20.1.2.9 Object.getOwnPropertySymbols(O)
function TGocciaGlobalObject.ObjectGetOwnPropertySymbols(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  OwnSymbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertySymbols', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    Arr := TGocciaArrayValue.Create;

    // Step 1: Return GetOwnPropertyKeys(O, symbol)
    OwnSymbols := Obj.GetOwnSymbols;
    for I := 0 to High(OwnSymbols) do
      Arr.Elements.Add(OwnSymbols[I]);

    Result := Arr;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.6 Object.freeze(O)
function TGocciaGlobalObject.ObjectFreeze(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.freeze', ThrowError);

  // Step 1: If Type(O) is not Object, return O
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  // Step 2: Let status be ? SetIntegrityLevel(O, frozen)
  TGocciaObjectValue(AArgs.GetElement(0)).Freeze;
  // Step 3: Return O
  Result := AArgs.GetElement(0);
end;

// ES2026 §20.1.2.13 Object.isFrozen(O)
function TGocciaGlobalObject.ObjectIsFrozen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isFrozen', ThrowError);

  // Step 1: If Type(O) is not Object, return true
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // Step 2: Return ? TestIntegrityLevel(O, frozen)
  if TGocciaObjectValue(AArgs.GetElement(0)).Frozen then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.2.12 Object.getPrototypeOf(O)
function TGocciaGlobalObject.ObjectGetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Arg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.getPrototypeOf', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  // Symbol primitives: ToObject(Symbol) -> Symbol wrapper whose prototype is %Symbol.prototype%.
  // Without a wrapper class we short-circuit to %Symbol.prototype% directly.
  Arg := AArgs.GetElement(0);
  if Arg is TGocciaSymbolValue then
  begin
    if Assigned(TGocciaSymbolValue.SharedPrototype) then
      Result := TGocciaSymbolValue.SharedPrototype
    else
      Result := TGocciaNullLiteralValue.NullValue;
    Exit;
  end;

  Obj := ToObject(Arg);
  if Assigned(TGarbageCollector.Instance) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Proxy intercept: delegate to getPrototypeOf trap
    if Obj is TGocciaProxyValue then
    begin
      Result := TGocciaProxyValue(Obj).GetPrototypeTrap;
      Exit;
    end;

    // Step 2: Return ? obj.[[GetPrototypeOf]]()
    if Assigned(Obj.Prototype) then
      Result := Obj.Prototype
    else
      Result := TGocciaNullLiteralValue.NullValue;
  finally
    if Assigned(TGarbageCollector.Instance) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.7 Object.fromEntries(iterable)
function TGocciaGlobalObject.ObjectFromEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  I: Integer;
  Key: string;
  Value: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.fromEntries', ThrowError);

  // Step 1: Perform ? RequireObjectCoercible(iterable)
  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowTypeError(SErrorObjectFromEntriesRequiresIterable, SSuggestNotIterable);

  Entries := TGocciaArrayValue(AArgs.GetElement(0));
  // Step 2: Let obj be OrdinaryObjectCreate(%Object.prototype%)
  Obj := TGocciaObjectValue.Create;

  // Step 3: For each entry of iterable
  for I := 0 to Entries.Elements.Count - 1 do
  begin
    if not (Entries.Elements[I] is TGocciaArrayValue) then
      ThrowTypeError(SErrorObjectFromEntriesRequiresIterable, SSuggestNotIterable);

    Entry := TGocciaArrayValue(Entries.Elements[I]);
    if Entry.Elements.Count < 2 then
      ThrowTypeError(SErrorObjectFromEntriesRequiresPairs, SSuggestNotIterable);

    // Step 3a: Let key be entry[0], let value be entry[1]
    Key := Entry.Elements[0].ToStringLiteral.Value;
    Value := Entry.Elements[1];
    // Step 3b: Perform ! CreateDataPropertyOrThrow(obj, key, value)
    Obj.AssignProperty(Key, Value);
  end;

  // Step 4: Return obj
  Result := Obj;
end;

// ES2026 §20.1.2.20 Object.seal(O)
function TGocciaGlobalObject.ObjectSeal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.seal', ThrowError);

  // Step 1: If Type(O) is not Object, return O
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  // Step 2: Let status be ? SetIntegrityLevel(O, sealed)
  TGocciaObjectValue(AArgs.GetElement(0)).Seal;
  // Step 3: Return O
  Result := AArgs.GetElement(0);
end;

// ES2026 §20.1.2.15 Object.isSealed(O)
function TGocciaGlobalObject.ObjectIsSealed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isSealed', ThrowError);

  // Step 1: If Type(O) is not Object, return true
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // Step 2: Return ? TestIntegrityLevel(O, sealed)
  if TGocciaObjectValue(AArgs.GetElement(0)).Sealed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.2.18 Object.preventExtensions(O)
function TGocciaGlobalObject.ObjectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.preventExtensions', ThrowError);

  // Step 1: If Type(O) is not Object, return O
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  // Step 2: Let status be ? O.[[PreventExtensions]]()
  TGocciaObjectValue(AArgs.GetElement(0)).PreventExtensions;
  // Step 3: Return O
  Result := AArgs.GetElement(0);
end;

// ES2026 §20.1.2.14 Object.isExtensible(O)
function TGocciaGlobalObject.ObjectIsExtensible(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isExtensible', ThrowError);

  // Step 1: If Type(O) is not Object, return false
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Proxy intercept: delegate to isExtensible trap
  if AArgs.GetElement(0) is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AArgs.GetElement(0)).IsExtensibleTrap then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Step 2: Return ? IsExtensible(O)
  if TGocciaObjectValue(AArgs.GetElement(0)).Extensible then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.2.21 Object.setPrototypeOf(O, proto)
function TGocciaGlobalObject.ObjectSetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ProtoArg: TGocciaValue;
  Target: TGocciaObjectValue;
  Walker: TGocciaObjectValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.setPrototypeOf', ThrowError);

  // Step 1: Perform ? RequireObjectCoercible(O) — throws for undefined/null
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(0) is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);

  // Step 2: If Type(proto) is neither Object nor Null, throw a TypeError exception
  ProtoArg := AArgs.GetElement(1);
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorObjectPrototypeMustBeObjectOrNull, SSuggestObjectArgType);

  // Step 3: If Type(O) is not Object, return O (primitives are immutable)
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  Target := TGocciaObjectValue(AArgs.GetElement(0));

  // Proxy intercept: delegate to setPrototypeOf trap
  if Target is TGocciaProxyValue then
  begin
    if not TGocciaProxyValue(Target).SetPrototypeTrap(ProtoArg) then
      ThrowTypeError(SErrorSetPrototypeOfTrapReturnedFalse, SSuggestObjectArgType);
    Result := Target;
    Exit;
  end;

  // ES2026 §10.1.2 OrdinarySetPrototypeOf step 2: SameValue short-circuit
  if (ProtoArg is TGocciaNullLiteralValue) and (not Assigned(Target.Prototype)) then
  begin
    Result := Target;
    Exit;
  end;
  if (ProtoArg is TGocciaObjectValue) and
     (Target.Prototype = TGocciaObjectValue(ProtoArg)) then
  begin
    Result := Target;
    Exit;
  end;

  // Step 3: If Type(O) is not Object, return O (handled above via throw)
  if not Target.Extensible then
    ThrowTypeError(SErrorSetPrototypeOfNonExtensible, SSuggestObjectNotExtensible);

  // ES2026 §10.1.2 OrdinarySetPrototypeOf step 8: Cycle detection.  Walk the
  // proposed prototype's chain — if we reach Target, the new chain would be
  // cyclic.  Object.setPrototypeOf throws in this case (Reflect.setPrototypeOf
  // returns false; see Goccia.Builtins.GlobalReflect.ReflectSetPrototypeOf).
  // Step 8.b.ii: if the next link doesn't use the ordinary [[GetPrototypeOf]]
  // (e.g. it's a Proxy with a getPrototypeOf trap), stop the walk and let the
  // assignment proceed — we cannot statically prove a cycle through an exotic.
  if ProtoArg is TGocciaObjectValue then
  begin
    Walker := TGocciaObjectValue(ProtoArg);
    while Assigned(Walker) do
    begin
      if Walker = Target then
        ThrowTypeError(SErrorSetPrototypeOfCyclic, SSuggestObjectArgType);
      if Walker is TGocciaProxyValue then
        Break;
      Walker := Walker.Prototype;
    end;
  end;

  // Step 4: Let status be ? O.[[SetPrototypeOf]](proto)
  if ProtoArg is TGocciaNullLiteralValue then
    Target.Prototype := nil
  else
    Target.Prototype := TGocciaObjectValue(ProtoArg);

  // Step 5: Return O
  Result := Target;
end;

// ES2026 §20.1.2.8 Object.groupBy(items, callbackfn)
function TGocciaGlobalObject.ObjectGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Items: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultObj: TGocciaObjectValue;
  GroupKey: string;
  GroupArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  KeyValue: TGocciaValue;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.groupBy', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowTypeError(SErrorObjectGroupByRequiresIterable, SSuggestNotIterable);
  if not AArgs.GetElement(1).IsCallable then
    ThrowTypeError(SErrorObjectGroupByRequiresCallback, SSuggestCallbackRequired);

  // Step 1: Let groups be ? GroupBy(items, callbackfn, property)
  Items := TGocciaArrayValue(AArgs.GetElement(0));
  Callback := AArgs.GetElement(1);
  // Step 2: Let obj be OrdinaryObjectCreate(null)
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.Prototype := nil;

  // Step 3: For each Record { [[Key]], [[Elements]] } g of groups
  I := 0;
  while I < Items.Elements.Count do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Items.Elements[I]);
      CallArgs.Add(TGocciaNumberLiteralValue.Create(I));

      KeyValue := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;

    GroupKey := KeyValue.ToStringLiteral.Value;

    if ResultObj.HasOwnProperty(GroupKey) then
      GroupArray := TGocciaArrayValue(ResultObj.GetProperty(GroupKey))
    else
    begin
      // Step 3a: Let elements be CreateArrayFromList(g.[[Elements]])
      GroupArray := TGocciaArrayValue.Create;
      // Step 3b: Perform ! CreateDataPropertyOrThrow(obj, g.[[Key]], elements)
      ResultObj.AssignProperty(GroupKey, GroupArray);
    end;

    GroupArray.Elements.Add(Items.Elements[I]);
    Inc(I);
  end;

  // Step 4: Return obj
  Result := ResultObj;
end;

end.
