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
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Validator,
  Goccia.Arithmetic,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ProxyValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive,
  Goccia.Values.TypedArrayValue;

type
  TPendingDefineProperty = record
    Name: string;
    Symbol: TGocciaSymbolValue;
    IsSymbol: Boolean;
    Descriptor: TGocciaPropertyDescriptor;
  end;

  TPendingDefinePropertyArray = array of TPendingDefineProperty;

// AStagingRoots roots what the appended descriptor points at for as long as
// the batch is staged. §20.1.2.3.1 collects every descriptor before it defines
// any of them, and each collection step reads through the properties object —
// an accessor or a Proxy trap, so a guest-code safe point. A staged
// TGocciaPropertyDescriptor is a plain class the collector does not trace, and
// this array is a native local it cannot see either, so without this push the
// values captured for the first key are reachable from nowhere while the
// second key's getter runs. A shared frame, not a temp root per descriptor:
// the same function can legitimately appear as several keys' getter, and a
// temp-root set would collapse those into one entry.
procedure AppendPendingDefineProperty(
  var APendingProperties: TPendingDefinePropertyArray;
  const AName: string; const ASymbol: TGocciaSymbolValue;
  const AIsSymbol: Boolean; const ADescriptor: TGocciaPropertyDescriptor;
  var AStagingRoots: TGocciaActiveRootFrame);
var
  Index: Integer;
begin
  Index := Length(APendingProperties);
  SetLength(APendingProperties, Index + 1);
  APendingProperties[Index].Name := AName;
  APendingProperties[Index].Symbol := ASymbol;
  APendingProperties[Index].IsSymbol := AIsSymbol;
  APendingProperties[Index].Descriptor := ADescriptor;
  AStagingRoots.Add(ASymbol);
  if Assigned(ADescriptor) then
    ADescriptor.PushRoots(AStagingRoots);
end;

procedure ReleasePendingDefineProperties(
  var APendingProperties: TPendingDefinePropertyArray);
var
  I: Integer;
begin
  for I := 0 to High(APendingProperties) do
  begin
    if Assigned(APendingProperties[I].Descriptor) then
    begin
      APendingProperties[I].Descriptor.Free;
      APendingProperties[I].Descriptor := nil;
    end;
  end;
  SetLength(APendingProperties, 0);
end;

// ES2026 §6.2.6.4 FromPropertyDescriptor(Desc)
function FromPropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if ADescriptor is TGocciaPropertyDescriptorData then
  begin
    if ADescriptor.HasValue then
      Result.CreateDataPropertyOrThrow(PROP_VALUE,
        TGocciaPropertyDescriptorData(ADescriptor).Value);
    if ADescriptor.HasWritableField then
    begin
      if ADescriptor.Writable then
        Result.CreateDataPropertyOrThrow(PROP_WRITABLE, TGocciaBooleanLiteralValue.TrueValue)
      else
        Result.CreateDataPropertyOrThrow(PROP_WRITABLE, TGocciaBooleanLiteralValue.FalseValue);
    end;
  end
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    if ADescriptor.HasGet then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
        Result.CreateDataPropertyOrThrow(PROP_GET,
          TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
      else
        Result.CreateDataPropertyOrThrow(PROP_GET,
          TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    if ADescriptor.HasSet then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
        Result.CreateDataPropertyOrThrow(PROP_SET,
          TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
      else
        Result.CreateDataPropertyOrThrow(PROP_SET,
          TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  end;

  if ADescriptor.HasEnumerableField then
  begin
    if ADescriptor.Enumerable then
      Result.CreateDataPropertyOrThrow(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.TrueValue)
    else
      Result.CreateDataPropertyOrThrow(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.FalseValue);
  end;
  if ADescriptor.HasConfigurableField then
  begin
    if ADescriptor.Configurable then
      Result.CreateDataPropertyOrThrow(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.TrueValue)
    else
      Result.CreateDataPropertyOrThrow(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.FalseValue);
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
    RegisterMemberDefinitions(FBuiltinObject, Members.ToDefinitions);
  finally
    Members.Free;
  end;
  FBuiltinObject.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(1),
      [pfConfigurable]));
end;

// ES2026 §20.1.2.10 Object.is(value1, value2)
function TGocciaGlobalObject.ObjectIs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Left := AArgs.GetElement(0)
  else
    Left := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length > 1 then
    Right := AArgs.GetElement(1)
  else
    Right := TGocciaUndefinedLiteralValue.UndefinedValue;

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
  KeysRoot: TGocciaTempRoot;
  Names: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.keys', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  InitializeTempRoot(KeysRoot);
  try
    Keys := TGocciaArrayValue.Create;
    { The key strings below are charged against the memory ceiling, so each
      TGocciaStringLiteralValue.Create is a GC safe point.  Without a root the
      half-built result array is unreachable and gets swept mid-loop, leaving
      Keys dangling. }
    AddTempRootIfNeeded(KeysRoot, Keys);

    // Step 2: Let nameList be ? EnumerableOwnProperties(obj, key).
    if Obj is TGocciaStringObjectValue then
      Names := TGocciaStringObjectValue(Obj).GetAllPropertyNames
    else
      Names := Obj.GetOwnPropertyKeys;

    for I := 0 to High(Names) do
    begin
      Descriptor := Obj.GetOwnPropertyDescriptor(Names[I]);
      if Assigned(Descriptor) and Descriptor.Enumerable then
        Keys.Elements.Add(TGocciaStringLiteralValue.Create(Names[I]));
    end;

    Result := Keys;
  finally
    RemoveTempRootIfNeeded(KeysRoot);
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.22 Object.values(O)
function TGocciaGlobalObject.ObjectValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  ValuesRoot: TGocciaTempRoot;
  PropertyNames: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.values', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  InitializeTempRoot(ValuesRoot);
  try
    Values := TGocciaArrayValue.Create;
    { GetProperty below can run an accessor, and accessors allocate, so the
      half-built result array must stay reachable across the loop. }
    AddTempRootIfNeeded(ValuesRoot, Values);

    // Step 2: Let valueList be ? EnumerableOwnProperties(obj, value).
    if Obj is TGocciaStringObjectValue then
      PropertyNames := TGocciaStringObjectValue(Obj).GetAllPropertyNames
    else
      PropertyNames := Obj.GetOwnPropertyKeys;

    for I := 0 to High(PropertyNames) do
    begin
      Descriptor := Obj.GetOwnPropertyDescriptor(PropertyNames[I]);
      if Assigned(Descriptor) and Descriptor.Enumerable then
        Values.Elements.Add(Obj.GetProperty(PropertyNames[I]));
    end;

    Result := Values;
  finally
    RemoveTempRootIfNeeded(ValuesRoot);
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.5 Object.entries(O)
function TGocciaGlobalObject.ObjectEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  EntriesRoot: TGocciaTempRoot;
  Entry: TGocciaArrayValue;
  EntryRoot: TGocciaTempRoot;
  PropertyNames: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.entries', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  InitializeTempRoot(EntriesRoot);
  InitializeTempRoot(EntryRoot);
  try
    Entries := TGocciaArrayValue.Create;
    { Both the key string and the accessor call below are GC safe points, so
      the outer array and the in-flight pair need roots.  A pair is only
      unrooted once Entries owns it. }
    AddTempRootIfNeeded(EntriesRoot, Entries);

    // Step 2: Let entryList be ? EnumerableOwnProperties(obj, key+value).
    if Obj is TGocciaStringObjectValue then
      PropertyNames := TGocciaStringObjectValue(Obj).GetAllPropertyNames
    else
      PropertyNames := Obj.GetOwnPropertyKeys;

    for I := 0 to High(PropertyNames) do
    begin
      Descriptor := Obj.GetOwnPropertyDescriptor(PropertyNames[I]);
      if not (Assigned(Descriptor) and Descriptor.Enumerable) then
        Continue;

      Entry := TGocciaArrayValue.Create;
      AddTempRootIfNeeded(EntryRoot, Entry);
      Entry.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));
      Entry.Elements.Add(Obj.GetProperty(PropertyNames[I]));
      Entries.Elements.Add(Entry);
    end;

    Result := Entries;
  finally
    RemoveTempRootIfNeeded(EntryRoot);
    RemoveTempRootIfNeeded(EntriesRoot);
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.1 Object.assign(target, ...sources)
function TGocciaGlobalObject.ObjectAssign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  PropertyNames: TArray<string>;
  PropertyKeyValues: TArray<TGocciaValue>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
  Descriptor: TGocciaPropertyDescriptor;
  PropValue: TGocciaValue;
  SourceArg: TGocciaValue;
  GC: TGarbageCollector;
  I, J: Integer;

  function IsReadonlyStringExoticKey(const ATarget: TGocciaObjectValue;
    const AName: string): Boolean;
  var
    Index: Integer;
    StringValue: string;
  begin
    Result := False;
    if not (ATarget is TGocciaStringObjectValue) then
      Exit;

    if AName = PROP_LENGTH then
      Exit(True);

    if TryStrToInt(AName, Index) and (AName = IntToStr(Index)) then
    begin
      StringValue := TGocciaStringObjectValue(ATarget).Primitive.ToStringLiteral.Value;
      Result := (Index >= 0) and (Index < UTF16CodeUnitLength(StringValue));
    end;
  end;

  procedure AssignStringKey(const AName: string);
  begin
    Descriptor := Source.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      PropValue := Source.GetProperty(AName);
      if IsReadonlyStringExoticKey(InitialObj, AName) then
        ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]),
          SSuggestReadOnlyProperty);
      InitialObj.SetProperty(AName, PropValue);
    end;
  end;

  procedure AssignSymbolKey(const ASymbol: TGocciaSymbolValue);
  begin
    Descriptor := Source.GetOwnSymbolPropertyDescriptor(ASymbol);
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      PropValue := Source.GetSymbolProperty(ASymbol);
      InitialObj.AssignSymbolProperty(ASymbol, PropValue);
    end;
  end;
begin
  GC := TGarbageCollector.Instance;

  // Step 1: Let to be ? ToObject(target).
  if AArgs.Length > 0 then
    InitialObj := ToObject(AArgs.GetElement(0))
  else
    InitialObj := ToObject(TGocciaUndefinedLiteralValue.UndefinedValue);
  if Assigned(GC) and ((AArgs.Length = 0) or
     not (AArgs.GetElement(0) is TGocciaObjectValue)) then
    GC.AddTempRoot(InitialObj);
  try
    // Step 2: If only one argument was passed, return to.
    if AArgs.Length <= 1 then
      Exit(InitialObj);

    // Step 3: For each element nextSource of sources.
    for I := 1 to AArgs.Length - 1 do
    begin
      SourceArg := AArgs.GetElement(I);
      if (SourceArg is TGocciaUndefinedLiteralValue) or
         (SourceArg is TGocciaNullLiteralValue) then
        Continue;

      Source := ToObject(SourceArg);
      if Assigned(GC) and not (SourceArg is TGocciaObjectValue) then
        GC.AddTempRoot(Source);
      try
        // ES2026 §20.1.2.1 step 3.a.ii: [[OwnPropertyKeys]].
        if Source is TGocciaProxyValue then
        begin
          PropertyKeyValues := TGocciaProxyValue(Source).GetOwnPropertyKeyValues;
          for J := 0 to High(PropertyKeyValues) do
          begin
            if PropertyKeyValues[J] is TGocciaSymbolValue then
              AssignSymbolKey(TGocciaSymbolValue(PropertyKeyValues[J]))
            else
              AssignStringKey(PropertyKeyValues[J].ToStringLiteral.Value);
          end;
        end
        else
        begin
          if Source is TGocciaStringObjectValue then
            PropertyNames := TGocciaStringObjectValue(Source).GetAllPropertyNames
          else
            PropertyNames := Source.GetOwnPropertyKeys;
          for J := 0 to High(PropertyNames) do
            AssignStringKey(PropertyNames[J]);

          SymbolKeys := Source.GetOwnSymbols;
          for J := 0 to High(SymbolKeys) do
            AssignSymbolKey(SymbolKeys[J]);
        end;
      finally
        if Assigned(GC) and not (SourceArg is TGocciaObjectValue) then
          GC.RemoveTempRoot(Source);
      end;
    end;

    // Step 4: Return to.
    Result := InitialObj;
  finally
    if Assigned(GC) and ((AArgs.Length = 0) or
       not (AArgs.GetElement(0) is TGocciaObjectValue)) then
      GC.RemoveTempRoot(InitialObj);
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
  ClassObj: TGocciaClassValue;
  PropertyName: string;
  KeyValue: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Object.hasOwn', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  if AArgs.GetElement(0) is TGocciaClassValue then
  begin
    ClassObj := TGocciaClassValue(AArgs.GetElement(0));
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Return ? HasOwnProperty(obj, key)
    KeyValue := ToPropertyKey(AArgs.GetElement(1));
    if KeyValue is TGocciaSymbolValue then
    begin
      if Assigned(ClassObj.GetOwnSymbolPropertyDescriptor(
        TGocciaSymbolValue(KeyValue))) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      if ClassObj.HasOwnProperty(TGocciaStringLiteralValue(KeyValue).Value) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
    Exit;
  end;

  if AArgs.GetElement(0) is TGocciaSymbolValue then
  begin
    ToPropertyKey(AArgs.GetElement(1));
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Return ? HasOwnProperty(obj, key)
    KeyValue := ToPropertyKey(AArgs.GetElement(1));
    if KeyValue is TGocciaSymbolValue then
    begin
      if Obj.HasSymbolProperty(TGocciaSymbolValue(KeyValue)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      if Obj.HasOwnProperty(TGocciaStringLiteralValue(KeyValue).Value) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  finally
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.8 Object.getOwnPropertyNames(O)
function TGocciaGlobalObject.ObjectGetOwnPropertyNames(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Names: TGocciaArrayValue;
  NamesRoot: TGocciaTempRoot;
  PropertyNames: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertyNames', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  InitializeTempRoot(NamesRoot);
  try
    Names := TGocciaArrayValue.Create;
    { Each name string is charged against the memory ceiling and is therefore
      a GC safe point; keep the half-built result reachable. }
    AddTempRootIfNeeded(NamesRoot, Names);

    // Step 1: Return GetOwnPropertyKeys(O, string)
    PropertyNames := Obj.GetAllPropertyNames;
    for I := 0 to High(PropertyNames) do
      Names.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));

    Result := Names;
  finally
    RemoveTempRootIfNeeded(NamesRoot);
    if (TGarbageCollector.Instance <> nil) and
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
  KeyValue: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertyDescriptor', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let key be ? ToPropertyKey(P)
    // Step 3: Let desc be ? O.[[GetOwnProperty]](key)
    KeyValue := ToPropertyKey(AArgs.GetElement(1));
    if KeyValue is TGocciaSymbolValue then
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(KeyValue))
    else
      Descriptor := Obj.GetOwnPropertyDescriptor(TGocciaStringLiteralValue(KeyValue).Value);
    // Step 4: Return FromPropertyDescriptor(desc)
    if Descriptor = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      Result := FromPropertyDescriptor(Descriptor);
  finally
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.7 Object.getOwnPropertyDescriptors(O)
function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptors(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj, Descriptors: TGocciaObjectValue;
  PropertyNames: TArray<string>;
  PropertyKeys: TArray<TGocciaValue>;
  KeyValue: TGocciaValue;
  OwnSymbols: TArray<TGocciaSymbolValue>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertyDescriptors', ThrowError);

  // Step 1: Let obj be ? ToObject(O)
  Obj := ToObject(AArgs.GetElement(0));
  if (TGarbageCollector.Instance <> nil) and
     not (AArgs.GetElement(0) is TGocciaObjectValue) then
    TGarbageCollector.Instance.AddTempRoot(Obj);
  try
    // Step 2: Let ownKeys be ? obj.[[OwnPropertyKeys]]()
    // Step 3: Let descriptors be OrdinaryObjectCreate(%Object.prototype%)
    Descriptors := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);

    if Obj is TGocciaProxyValue then
    begin
      PropertyKeys := TGocciaProxyValue(Obj).GetOwnPropertyKeyValues;
      for KeyValue in PropertyKeys do
      begin
        if KeyValue is TGocciaSymbolValue then
          Descriptor := Obj.GetOwnSymbolPropertyDescriptor(
            TGocciaSymbolValue(KeyValue))
        else
          Descriptor := Obj.GetOwnPropertyDescriptor(
            KeyValue.ToStringLiteral.Value);
        if Descriptor <> nil then
          Descriptors.CreateDataPropertyOrThrow(KeyValue,
            FromPropertyDescriptor(Descriptor));
      end;
      Exit(Descriptors);
    end;

    // Step 4: For each element key of ownKeys (string keys)
    PropertyNames := Obj.GetAllPropertyNames;
    for I := 0 to High(PropertyNames) do
    begin
      // Step 4a: Let desc be ? obj.[[GetOwnProperty]](key)
      Descriptor := Obj.GetOwnPropertyDescriptor(PropertyNames[I]);
      // Step 4b-c: Let descriptor be FromPropertyDescriptor(desc) and add to result
      if Descriptor <> nil then
        Descriptors.CreateDataPropertyOrThrow(PropertyNames[I],
          FromPropertyDescriptor(Descriptor));
    end;

    // Step 4 continued: For each element key of ownKeys (symbol keys)
    OwnSymbols := Obj.GetOwnSymbols;
    for I := 0 to High(OwnSymbols) do
    begin
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(OwnSymbols[I]);
      if Descriptor <> nil then
        Descriptors.CreateDataPropertyOrThrow(OwnSymbols[I],
          FromPropertyDescriptor(Descriptor));
    end;

    // Step 5: Return descriptors
    Result := Descriptors;
  finally
    if (TGarbageCollector.Instance <> nil) and
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
  KeyValue: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Object.defineProperty', ThrowError);

  // Step 1: If Type(O) is not Object, throw a TypeError exception
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertyCalledOnNonObject, SSuggestObjectArgType);

  if not (AArgs.GetElement(2) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertyDescriptorMustBeObject, SSuggestObjectArgType);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  DescriptorObject := TGocciaObjectValue(AArgs.GetElement(2));

  // Step 2: Let key be ? ToPropertyKey(P)
  KeyValue := ToPropertyKey(AArgs.GetElement(1));
  IsSymbolKey := KeyValue is TGocciaSymbolValue;
  SymbolKey := nil;
  PropertyName := '';
  ExistingDescriptor := nil;

  if IsSymbolKey then
  begin
    SymbolKey := TGocciaSymbolValue(KeyValue);
    if not (Obj is TGocciaProxyValue) then
      ExistingDescriptor := Obj.GetOwnSymbolPropertyDescriptor(SymbolKey);
  end
  else
  begin
    PropertyName := TGocciaStringLiteralValue(KeyValue).Value;
    if not (Obj is TGocciaProxyValue) then
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
  PropertiesObject: TGocciaObjectValue;
  PropertyNames: TArray<string>;
  PropertyKeyValues: TArray<TGocciaValue>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
  PendingProperties: TPendingDefinePropertyArray;
  PropertyDescriptor: TGocciaPropertyDescriptor;
  DescriptorValue: TGocciaValue;
  KeyValue: TGocciaValue;
  GC: TGarbageCollector;
  I: Integer;
  StagingRoots: TGocciaActiveRootFrame;

  // PropertyDescriptor is read for its Enumerable flag and then dropped, and
  // it is deliberately NOT freed. Whether GetOwnPropertyDescriptor's result is
  // caller-owned cannot be decided here: an ordinary object returns the
  // descriptor its property map owns, and freeing that corrupts the map; a
  // proxy returns a freshly completed one when it has a
  // getOwnPropertyDescriptor trap but forwards to the target — map-owned
  // again — when it does not; a String object synthesises for index and
  // `length` keys and returns a map-owned descriptor for every other key. So
  // the receiver's type does not answer the question, and probing for the trap
  // would re-run guest code the spec only permits once per key.
  //
  // The consequence is a small leak per synthesising key, shared with every
  // other GetOwnPropertyDescriptor caller in the engine. Closing it needs an
  // ownership signal on the accessor itself, not a rule at this call site.
  procedure CaptureStringDescriptor(const AName: string);
  begin
    PropertyDescriptor := PropertiesObject.GetOwnPropertyDescriptor(AName);
    if Assigned(PropertyDescriptor) and PropertyDescriptor.Enumerable then
    begin
      DescriptorValue := PropertiesObject.GetProperty(AName);
      AppendPendingDefineProperty(PendingProperties, AName, nil, False,
        ToPropertyDescriptor(DescriptorValue, nil), StagingRoots);
    end;
  end;

  procedure CaptureSymbolDescriptor(const ASymbol: TGocciaSymbolValue);
  begin
    PropertyDescriptor := PropertiesObject.GetOwnSymbolPropertyDescriptor(ASymbol);
    if Assigned(PropertyDescriptor) and PropertyDescriptor.Enumerable then
    begin
      DescriptorValue := PropertiesObject.GetSymbolProperty(ASymbol);
      AppendPendingDefineProperty(PendingProperties, '', ASymbol, True,
        ToPropertyDescriptor(DescriptorValue, nil), StagingRoots);
    end;
  end;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Object.defineProperties', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowTypeError(SErrorObjectDefinePropertiesCalledOnNonObject, SSuggestObjectArgType);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  // ES2026 §20.1.2.3.1 step 1: Let props be ? ToObject(Properties).
  PropertiesObject := ToObject(AArgs.GetElement(1));
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and not (AArgs.GetElement(1) is TGocciaObjectValue) then
    GC.AddTempRoot(PropertiesObject);
  { The staged batch and the proxy key list are both native locals holding
    values nothing else refers to while the capture loop runs guest code; see
    AppendPendingDefineProperty. The frame spans the apply loop as well, so a
    descriptor still staged when an earlier DefineProperty runs a Proxy
    defineProperty trap stays rooted too. }
  StagingRoots.Initialize;
  try
    try
      // ES2026 §20.1.2.3.1 steps 2-4: collect descriptors before
      // defining any target property.
      if PropertiesObject is TGocciaProxyValue then
      begin
        PropertyKeyValues := TGocciaProxyValue(PropertiesObject).GetOwnPropertyKeyValues;
        for I := 0 to High(PropertyKeyValues) do
          StagingRoots.Add(PropertyKeyValues[I]);
        for KeyValue in PropertyKeyValues do
        begin
          if KeyValue is TGocciaSymbolValue then
            CaptureSymbolDescriptor(TGocciaSymbolValue(KeyValue))
          else
            CaptureStringDescriptor(KeyValue.ToStringLiteral.Value);
        end;
      end
      else
      begin
        if PropertiesObject is TGocciaStringObjectValue then
          PropertyNames := TGocciaStringObjectValue(PropertiesObject).GetAllPropertyNames
        else
          PropertyNames := PropertiesObject.GetOwnPropertyKeys;
        for I := 0 to High(PropertyNames) do
          CaptureStringDescriptor(PropertyNames[I]);

        SymbolKeys := PropertiesObject.GetOwnSymbols;
        for I := 0 to High(SymbolKeys) do
          StagingRoots.Add(SymbolKeys[I]);
        for I := 0 to High(SymbolKeys) do
          CaptureSymbolDescriptor(SymbolKeys[I]);
      end;

      // ES2026 §20.1.2.3.1 step 5: apply the collected descriptors.
      for I := 0 to High(PendingProperties) do
      begin
        try
          if PendingProperties[I].IsSymbol then
            Obj.DefineSymbolProperty(PendingProperties[I].Symbol,
              PendingProperties[I].Descriptor)
          else
            Obj.DefineProperty(PendingProperties[I].Name,
              PendingProperties[I].Descriptor);
          PendingProperties[I].Descriptor := nil;
        except
          if Assigned(PendingProperties[I].Descriptor) then
          begin
            PendingProperties[I].Descriptor.Free;
            PendingProperties[I].Descriptor := nil;
          end;
          raise;
        end;
      end;
    except
      ReleasePendingDefineProperties(PendingProperties);
      raise;
    end;
  finally
    ReleasePendingDefineProperties(PendingProperties);
    StagingRoots.Clear;
    if Assigned(GC) and not (AArgs.GetElement(1) is TGocciaObjectValue) then
      GC.RemoveTempRoot(PropertiesObject);
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
  if (TGarbageCollector.Instance <> nil) and
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
    if (TGarbageCollector.Instance <> nil) and
       not (AArgs.GetElement(0) is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.6 Object.freeze(O)
function TGocciaGlobalObject.ObjectFreeze(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaValue;
  TypedArray: TGocciaTypedArrayValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.freeze', ThrowError);

  Obj := AArgs.GetElement(0);

  // Step 1: If Type(O) is not Object, return O
  if not (Obj is TGocciaObjectValue) then
  begin
    Result := Obj;
    Exit;
  end;

  if Obj is TGocciaTypedArrayValue then
  begin
    TypedArray := TGocciaTypedArrayValue(Obj);
    if (TypedArray.BufferValue is TGocciaArrayBufferValue) and
       (TGocciaArrayBufferValue(TypedArray.BufferValue).MaxByteLength >= 0) then
      ThrowTypeError(
        'Cannot freeze a typed array backed by a resizable ArrayBuffer',
        'copy it to a fixed-length ArrayBuffer before freezing');
  end;

  // Step 2: Let status be ? SetIntegrityLevel(O, frozen)
  TGocciaObjectValue(Obj).Freeze;
  // Step 3: Return O
  Result := Obj;
end;

// ES2026 §20.1.2.13 Object.isFrozen(O)
function TGocciaGlobalObject.ObjectIsFrozen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.isFrozen', ThrowError);

  // Step 1: If Type(O) is not Object, return true
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // Step 2: Return ? TestIntegrityLevel(O, frozen)
  if TGocciaObjectValue(AArgs.GetElement(0)).TestIntegrityFrozen then
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
  if AArgs.Length > 0 then
    Arg := AArgs.GetElement(0)
  else
    Arg := TGocciaUndefinedLiteralValue.UndefinedValue;
  Obj := ToObject(Arg);
  if (TGarbageCollector.Instance <> nil) and
     not (Arg is TGocciaObjectValue) then
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
    if (TGarbageCollector.Instance <> nil) and
       not (Arg is TGocciaObjectValue) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

// ES2026 §20.1.2.7 Object.fromEntries(iterable)
function TGocciaGlobalObject.ObjectFromEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entry: TGocciaValue;
  EntryObject: TGocciaObjectValue;
  Iterable: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  Done: Boolean;
  Key: TGocciaValue;
  PropertyKey: TGocciaValue;
  Value: TGocciaValue;
  IteratorRoot, ResultRoot: TGocciaTempRoot;
  EntryRoot, KeyRoot, ValueRoot: TGocciaTempRoot;
begin
  if AArgs.Length > 0 then
    Iterable := AArgs.GetElement(0)
  else
    Iterable := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 1: Perform ? RequireObjectCoercible(iterable).
  RequireObjectCoercible(Iterable);

  Iterator := GetIteratorFromValue(Iterable);
  if not Assigned(Iterator) then
    ThrowTypeError(SErrorObjectFromEntriesRequiresIterable, SSuggestNotIterable);

  InitializeTempRoot(IteratorRoot);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(EntryRoot);
  InitializeTempRoot(KeyRoot);
  InitializeTempRoot(ValueRoot);
  AddTempRootIfNeeded(IteratorRoot, Iterator);
  try
    // Step 2: Let obj be OrdinaryObjectCreate(%Object.prototype%).
    Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    AddTempRootIfNeeded(ResultRoot, Obj);

    try
      while True do
      begin
        Entry := Iterator.DirectNext(Done);
        if Done then
          Exit(Obj);

        AddTempRootIfNeeded(EntryRoot, Entry);

        try
          if not (Entry is TGocciaObjectValue) then
            ThrowTypeError(SErrorObjectFromEntriesRequiresPairs, SSuggestNotIterable);
          EntryObject := TGocciaObjectValue(Entry);

          { All three reads below can run user code — two accessors and then a
            @@toPrimitive or toString — and each is a GC safe point. The entry
            pair is a value the iterator just produced, so the key read first is
            reachable from nowhere else while the value accessor runs, and the
            value is likewise exposed while ToPropertyKey coerces the key. }
          // ES2026 §24.1.1.2 steps 2.d-f: Get(entry, "0") and Get(entry, "1").
          Key := EntryObject.GetProperty('0');
          if not Assigned(Key) then
            Key := TGocciaUndefinedLiteralValue.UndefinedValue;
          AddTempRootIfNeeded(KeyRoot, Key);
          Value := EntryObject.GetProperty('1');
          if not Assigned(Value) then
            Value := TGocciaUndefinedLiteralValue.UndefinedValue;
          AddTempRootIfNeeded(ValueRoot, Value);

          // ES2026 §20.1.2.7 steps 4.a-b: ToPropertyKey and CreateDataProperty.
          PropertyKey := ToPropertyKey(Key);
          Obj.CreateDataPropertyOrThrow(PropertyKey, Value);
        except
          CloseIteratorPreservingError(Iterator);
          raise;
        end;
      end;
    finally
      RemoveTempRootIfNeeded(ValueRoot);
      RemoveTempRootIfNeeded(KeyRoot);
      RemoveTempRootIfNeeded(EntryRoot);
      RemoveTempRootIfNeeded(ResultRoot);
    end;
  finally
    RemoveTempRootIfNeeded(IteratorRoot);
  end;
end;

// ES2026 §20.1.2.20 Object.seal(O)
function TGocciaGlobalObject.ObjectSeal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.seal', ThrowError);

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
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.isSealed', ThrowError);

  // Step 1: If Type(O) is not Object, return true
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // Step 2: Return ? TestIntegrityLevel(O, sealed)
  if TGocciaObjectValue(AArgs.GetElement(0)).TestIntegritySealed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §20.1.2.18 Object.preventExtensions(O)
function TGocciaGlobalObject.ObjectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
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
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.isExtensible', ThrowError);

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
  TargetArg: TGocciaValue;
  ProtoArg: TGocciaValue;
  Target: TGocciaObjectValue;
  Walker: TGocciaObjectValue;
begin
  if AArgs.Length > 0 then
    TargetArg := AArgs.GetElement(0)
  else
    TargetArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length > 1 then
    ProtoArg := AArgs.GetElement(1)
  else
    ProtoArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 1: Perform ? RequireObjectCoercible(O) — throws for undefined/null
  if (TargetArg is TGocciaUndefinedLiteralValue) or
     (TargetArg is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);

  // Step 2: If Type(proto) is neither Object nor Null, throw a TypeError exception
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorObjectPrototypeMustBeObjectOrNull, SSuggestObjectArgType);

  // Step 3: If Type(O) is not Object, return O (primitives are immutable)
  if not (TargetArg is TGocciaObjectValue) then
  begin
    Result := TargetArg;
    Exit;
  end;

  Target := TGocciaObjectValue(TargetArg);

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

  // ES2026 §10.4.7: Object.prototype is an immutable-prototype exotic
  // object.  Its [[SetPrototypeOf]] returns false for every non-current
  // prototype value; Object.setPrototypeOf converts that false into TypeError.
  if Target = TGocciaObjectValue.SharedObjectPrototype then
    ThrowTypeError(SErrorSetPrototypeOfNonExtensible, SSuggestObjectNotExtensible);

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
  Item: TGocciaValue;
  Items: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  ResultObj: TGocciaObjectValue;
  GroupKey: string;
  PropertyKey: TGocciaValue;
  SymbolKey: TGocciaSymbolValue;
  GroupArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  KeyValue: TGocciaValue;
  Done: Boolean;
  I: Integer;
  ItemsRoot, IteratorRoot, CallbackRoot, ResultRoot: TGocciaTempRoot;
  ItemRoot, KeyValueRoot: TGocciaTempRoot;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Object.groupBy', ThrowError);

  Items := AArgs.GetElement(0);
  if not AArgs.GetElement(1).IsCallable then
    ThrowTypeError(SErrorObjectGroupByRequiresCallback, SSuggestCallbackRequired);
  Iterator := GetIteratorFromValue(Items);
  if not Assigned(Iterator) then
    ThrowTypeError(SErrorObjectGroupByRequiresIterable, SSuggestNotIterable);

  Callback := AArgs.GetElement(1);
  // Step 2: Let obj be OrdinaryObjectCreate(null)
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.Prototype := nil;
  InitializeTempRoot(ItemsRoot);
  InitializeTempRoot(IteratorRoot);
  InitializeTempRoot(CallbackRoot);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(ItemRoot);
  InitializeTempRoot(KeyValueRoot);
  AddTempRootIfNeeded(ItemsRoot, Items);
  AddTempRootIfNeeded(IteratorRoot, Iterator);
  AddTempRootIfNeeded(CallbackRoot, Callback);
  AddTempRootIfNeeded(ResultRoot, ResultObj);
  try

    // Step 3: For each Record { [[Key]], [[Elements]] } g of groups
    I := 0;
    while True do
    begin
      Item := Iterator.DirectNext(Done);
      if Done then
        Break;

      { The item the iterator just produced, and the key the callback returned
        for it, are reachable only from these locals once CallArgs is gone —
        and ToPropertyKey below runs the key's @@toPrimitive or toString, which
        is a GC safe point. The item is only stored into its group after that. }
      AddTempRootIfNeeded(ItemRoot, Item);

      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          CallArgs.Add(Item);
          CallArgs.Add(TGocciaNumberLiteralValue.Create(I));

          KeyValue := InvokeCallable(Callback, CallArgs,
            TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          CallArgs.Free;
        end;

        AddTempRootIfNeeded(KeyValueRoot, KeyValue);
        PropertyKey := ToPropertyKey(KeyValue);

        if PropertyKey is TGocciaSymbolValue then
        begin
          SymbolKey := TGocciaSymbolValue(PropertyKey);
          if ResultObj.HasSymbolProperty(SymbolKey) then
            GroupArray := TGocciaArrayValue(ResultObj.GetSymbolProperty(SymbolKey))
          else
          begin
            // Step 3a: Let elements be CreateArrayFromList(g.[[Elements]])
            GroupArray := TGocciaArrayValue.Create;
            // Step 3b: Perform ! CreateDataPropertyOrThrow(obj, g.[[Key]], elements)
            ResultObj.CreateDataPropertyOrThrow(SymbolKey, GroupArray);
          end;
        end
        else
        begin
          GroupKey := TGocciaStringLiteralValue(PropertyKey).Value;
          if ResultObj.HasOwnProperty(GroupKey) then
            GroupArray := TGocciaArrayValue(ResultObj.GetProperty(GroupKey))
          else
          begin
            // Step 3a: Let elements be CreateArrayFromList(g.[[Elements]])
            GroupArray := TGocciaArrayValue.Create;
            // Step 3b: Perform ! CreateDataPropertyOrThrow(obj, g.[[Key]], elements)
            ResultObj.CreateDataPropertyOrThrow(GroupKey, GroupArray);
          end;
        end;

        GroupArray.Elements.Add(Item);
        Inc(I);
      except
        CloseIteratorPreservingError(Iterator);
        raise;
      end;
    end;

    // Step 4: Return obj
    Result := ResultObj;
  finally
    RemoveTempRootIfNeeded(KeyValueRoot);
    RemoveTempRootIfNeeded(ItemRoot);
    RemoveTempRootIfNeeded(ResultRoot);
    RemoveTempRootIfNeeded(CallbackRoot);
    RemoveTempRootIfNeeded(IteratorRoot);
    RemoveTempRootIfNeeded(ItemsRoot);
  end;
end;

end.
