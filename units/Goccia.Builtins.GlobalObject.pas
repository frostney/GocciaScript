unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  protected
    // Native methods
    function ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperty(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperties(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses
  Goccia.Values.ArrayValue, Goccia.Values.StringValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue, Goccia.Values.ObjectPropertyDescriptor;

constructor TGocciaGlobalObject.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  // Global Object methods: writable, non-enumerable, configurable
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectKeys, 'keys', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectValues, 'values', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectEntries, 'entries', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectAssign, 'assign', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectCreate, 'create', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectHasOwn, 'hasOwn', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGetOwnPropertyDescriptor, 'getOwnPropertyDescriptor', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectDefineProperty, 'defineProperty', 3));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectDefineProperties, 'defineProperties', 2));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaGlobalObject.ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
begin
  if Args.Count <> 1 then
    ThrowError('Object.keys expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.keys called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Keys := TGocciaArrayValue.Create;

  Names := Obj.GetEnumerablePropertyNames;
  for I := 0 to High(Names) do
    Keys.Elements.Add(TGocciaStringValue.Create(Names[I]));

  Result := Keys;
end;

function TGocciaGlobalObject.ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  PropertyValues: TArray<TGocciaValue>;
  I: Integer;
begin
  if Args.Count <> 1 then
    ThrowError('Object.values expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.values called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Values := TGocciaArrayValue.Create;

  PropertyValues := Obj.GetEnumerablePropertyValues;
  for I := 0 to High(PropertyValues) do
    Values.Elements.Add(PropertyValues[I]);

  Result := Values;
end;

function TGocciaGlobalObject.ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I: Integer;
begin
  if Args.Count <> 1 then
    ThrowError('Object.entries expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.entries called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Entries := TGocciaArrayValue.Create;

  PropertyEntries := Obj.GetEnumerablePropertyEntries;
  for I := 0 to High(PropertyEntries) do
  begin
    Entry := TGocciaArrayValue.Create;
    Entry.Elements.Add(TGocciaStringValue.Create(PropertyEntries[I].Key));
    Entry.Elements.Add(PropertyEntries[I].Value);
    Entries.Elements.Add(Entry);
  end;

  Result := Entries;
end;

function TGocciaGlobalObject.ObjectAssign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I, J: Integer;
begin
  if Args.Count < 2 then
    ThrowError('Object.assign expects at least 2 arguments', 0, 0);

  // TODO: Should check for the first object or filter out non-objects
  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.assign called on non-object', 0, 0);

  InitialObj := TGocciaObjectValue(Args[0]);

  for I := 1 to Args.Count - 1 do
  begin
    if (Args[I] is TGocciaObjectValue) then
    begin
      Source := TGocciaObjectValue(Args[I]);

      // Use enumerable property entries to safely copy properties
      PropertyEntries := Source.GetEnumerablePropertyEntries;
      for J := 0 to High(PropertyEntries) do
        InitialObj.AssignProperty(PropertyEntries[J].Key, PropertyEntries[J].Value);
    end;
  end;

  Result := InitialObj;
end;

function TGocciaGlobalObject.ObjectCreate(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NewObj: TGocciaObjectValue;
  ProtoArg: TGocciaValue;
begin
  if Args.Count < 1 then
    ThrowError('Object.create expects at least 1 argument', 0, 0);

  ProtoArg := Args[0];

  // Validate the prototype argument - must be an object or null
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullValue) then
    ThrowError('Object.create called on non-object', 0, 0);

  // Create a new object
  NewObj := TGocciaObjectValue.Create;

  // Set the prototype based on the argument
  if ProtoArg is TGocciaObjectValue then
    NewObj.Prototype := TGocciaObjectValue(ProtoArg)
  else if ProtoArg is TGocciaNullValue then
    NewObj.Prototype := nil; // null prototype

  Result := NewObj;
end;

function TGocciaGlobalObject.ObjectHasOwn(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
begin
  if Args.Count <> 2 then
    ThrowError('Object.hasOwn expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.hasOwn called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyName := Args[1].ToString;

  Result := TGocciaBooleanValue.Create(Obj.HasOwnProperty(PropertyName));
end;

function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj, DescriptorObj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyName: string;
begin
  if Args.Count <> 2 then
    ThrowError('Object.getOwnPropertyDescriptor expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyDescriptor called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyName := Args[1].ToString;

  Descriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  if Descriptor = nil then
  begin
    Result := TGocciaUndefinedValue.Create;
  end else begin
    DescriptorObj := TGocciaObjectValue.Create;
    DescriptorObj.AssignProperty('enumarable', TGocciaBooleanValue.Create(Descriptor.Enumerable));
    DescriptorObj.AssignProperty('configurable', TGocciaBooleanValue.Create(Descriptor.Configurable));
    DescriptorObj.AssignProperty('writable', TGocciaBooleanValue.Create(Descriptor.Writable));
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      DescriptorObj.AssignProperty('value', TGocciaPropertyDescriptorData(Descriptor).Value);
    end;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      DescriptorObj.AssignProperty('get', TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      DescriptorObj.AssignProperty('set', TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
    end;

    Result := DescriptorObj;
  end;
end;

function TGocciaGlobalObject.ObjectDefineProperty(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  DescriptorObject: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Enumerable: Boolean;
  Configurable: Boolean;
  Writable: Boolean;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
  PropertyFlags: TPropertyFlags;
begin
  if Args.Count <> 3 then
    ThrowError('Object.defineProperty expects exactly 3 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.defineProperty called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyName := Args[1].ToString;

  DescriptorObject := TGocciaObjectValue(Args[2]);
  Enumerable := DescriptorObject.GetProperty('enumarable').ToBoolean;
  Configurable := DescriptorObject.GetProperty('configurable').ToBoolean;
  Writable := DescriptorObject.GetProperty('writable').ToBoolean;
  if DescriptorObject.HasProperty('value') then
    Value := DescriptorObject.GetProperty('value');
  if DescriptorObject.HasProperty('get') then
    Getter := DescriptorObject.GetProperty('get');
  if DescriptorObject.HasProperty('set') then
    Setter := DescriptorObject.GetProperty('set');

  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  if DescriptorObject.HasProperty('value') then
  begin
    Descriptor := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags);
  end else
  begin
    Descriptor := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
  end;

  Obj.DefineProperty(PropertyName, Descriptor);
  Result := Obj;
end;

function TGocciaGlobalObject.ObjectDefineProperties(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  if Args.Count <> 2 then
    ThrowError('Object.defineProperties expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.defineProperties called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyEntries := Obj.GetEnumerablePropertyEntries;

  for I := 0 to High(PropertyEntries) do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create;
    CallArgs.Add(TGocciaStringValue.Create(PropertyEntries[I].Key));
    CallArgs.Add(PropertyEntries[I].Value);
    ObjectDefineProperty(CallArgs, Obj);
  end;

  Result := Obj;
end;

end.
