unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  protected
    // Native methods
    function ObjectIs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyNames(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperty(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperties(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses
  Goccia.Values.ArrayValue, Goccia.Values.StringValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue, Goccia.Values.ObjectPropertyDescriptor, Goccia.Evaluator.Comparison, Goccia.Values.FunctionValue;

constructor TGocciaGlobalObject.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  // Global Object methods: writable, non-enumerable, configurable
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectIs, 'is', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectKeys, 'keys', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectValues, 'values', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectEntries, 'entries', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectAssign, 'assign', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectCreate, 'create', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectHasOwn, 'hasOwn', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGetOwnPropertyNames, 'getOwnPropertyNames', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGetOwnPropertyDescriptor, 'getOwnPropertyDescriptor', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectDefineProperty, 'defineProperty', 3));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectDefineProperties, 'defineProperties', 2));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaGlobalObject.ObjectIs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  if Args.Count <> 2 then
    ThrowError('Object.is expects exactly 2 arguments', 0, 0);

  Left := Args[0];
  Right := Args[1];

  Result := TGocciaBooleanValue.Create(IsSameValue(Left, Right));
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
    Keys.Elements.Add(TGocciaStringLiteral.Create(Names[I]));

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
    Entry.Elements.Add(TGocciaStringLiteral.Create(PropertyEntries[I].Key));
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
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteral) then
    ThrowError('Object.create called on non-object', 0, 0);

  // Create a new object with the specified prototype
  if ProtoArg is TGocciaObjectValue then
    Result := TGocciaObjectValue.Create(TGocciaObjectValue(ProtoArg))
  else if ProtoArg is TGocciaNullLiteral then
    Result := TGocciaObjectValue.Create(nil)
  else
    ThrowError('Object.create called on non-object', 0, 0);
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

function TGocciaGlobalObject.ObjectGetOwnPropertyNames(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Names: TGocciaArrayValue;
  PropertyNames: TArray<string>;
  I: Integer;
begin
  if Args.Count <> 1 then
    ThrowError('Object.getOwnPropertyNames expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyNames called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Names := TGocciaArrayValue.Create;

  // Get all property names (both enumerable and non-enumerable)
  PropertyNames := Obj.GetAllPropertyNames;
  for I := 0 to High(PropertyNames) do
    Names.Elements.Add(TGocciaStringLiteral.Create(PropertyNames[I]));

  Result := Names;
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
    Result := TGocciaUndefinedLiteral.Create;
  end else begin
    DescriptorObj := TGocciaObjectValue.Create;
    DescriptorObj.AssignProperty('enumerable', TGocciaBooleanValue.Create(Descriptor.Enumerable));
    DescriptorObj.AssignProperty('configurable', TGocciaBooleanValue.Create(Descriptor.Configurable));

    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      // Data descriptor: has value and writable properties
      DescriptorObj.AssignProperty('value', TGocciaPropertyDescriptorData(Descriptor).Value);
      DescriptorObj.AssignProperty('writable', TGocciaBooleanValue.Create(Descriptor.Writable));
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      // Accessor descriptor: has get and set properties
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
        DescriptorObj.AssignProperty('get', TGocciaPropertyDescriptorAccessor(Descriptor).Getter)
      else
        DescriptorObj.AssignProperty('get', TGocciaUndefinedLiteral.Create);

      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
        DescriptorObj.AssignProperty('set', TGocciaPropertyDescriptorAccessor(Descriptor).Setter)
      else
        DescriptorObj.AssignProperty('set', TGocciaUndefinedLiteral.Create);
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

  if not (Args[2] is TGocciaObjectValue) then
    ThrowError('Object.defineProperty: descriptor must be an object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyName := Args[1].ToString;
  DescriptorObject := TGocciaObjectValue(Args[2]);

  // Initialize all variables
  Enumerable := False;
  Configurable := False;
  Writable := False;
  Value := nil;
  Getter := nil;
  Setter := nil;

  // Get descriptor properties (defaults to false for missing properties)
  if DescriptorObject.HasProperty('enumerable') then
    Enumerable := DescriptorObject.GetProperty('enumerable').ToBoolean;
  if DescriptorObject.HasProperty('configurable') then
    Configurable := DescriptorObject.GetProperty('configurable').ToBoolean;
  if DescriptorObject.HasProperty('writable') then
    Writable := DescriptorObject.GetProperty('writable').ToBoolean;
  if DescriptorObject.HasProperty('value') then
    Value := DescriptorObject.GetProperty('value');
  if DescriptorObject.HasProperty('get') then
  begin
    Getter := DescriptorObject.GetProperty('get');
    // Validate getter: must be a function or undefined
    if not (Getter is TGocciaUndefinedLiteral) and not (Getter is TGocciaFunctionValue) and not (Getter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: getter must be a function or undefined', 0, 0);
    // Treat undefined as nil
    if Getter is TGocciaUndefinedLiteral then
      Getter := nil;
  end;
  if DescriptorObject.HasProperty('set') then
  begin
    Setter := DescriptorObject.GetProperty('set');
    // Validate setter: must be a function or undefined
    if not (Setter is TGocciaUndefinedLiteral) and not (Setter is TGocciaFunctionValue) and not (Setter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: setter must be a function or undefined', 0, 0);
    // Treat undefined as nil
    if Setter is TGocciaUndefinedLiteral then
      Setter := nil;
  end;

  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  // Create appropriate descriptor type
  if DescriptorObject.HasProperty('value') then
  begin
    // Data descriptor
    Descriptor := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags);
  end else
  begin
    // Accessor descriptor
    Descriptor := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
  end;

  Obj.DefineProperty(PropertyName, Descriptor);
  Result := Obj;
end;

function TGocciaGlobalObject.ObjectDefineProperties(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertiesDescriptor: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  if Args.Count <> 2 then
    ThrowError('Object.defineProperties expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.defineProperties called on non-object', 0, 0);

  if not (Args[1] is TGocciaObjectValue) then
    ThrowError('Object.defineProperties: properties must be an object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertiesDescriptor := TGocciaObjectValue(Args[1]);
  PropertyEntries := PropertiesDescriptor.GetEnumerablePropertyEntries;

  for I := 0 to High(PropertyEntries) do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      CallArgs.Add(Obj);
      CallArgs.Add(TGocciaStringLiteral.Create(PropertyEntries[I].Key));
      CallArgs.Add(PropertyEntries[I].Value);
      ObjectDefineProperty(CallArgs, ThisValue);
    finally
      CallArgs.Free;
    end;
  end;

  Result := Obj;
end;

end.
