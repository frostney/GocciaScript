unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Scope, Goccia.Values.Primitives, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.NativeFunction, 
  Goccia.Values.ObjectValue, Goccia.Arguments.Collection, Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  protected
    // Native methods
    function ObjectIs(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectKeys(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyNames(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectDefineProperties(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.ArrayValue, Goccia.Values.ObjectPropertyDescriptor, Goccia.Evaluator.Comparison, Goccia.Values.FunctionValue, Goccia.Values.ClassValue, Goccia.Values.ClassHelper, Goccia.Arguments.Validator;

constructor TGocciaGlobalObject.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
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

function TGocciaGlobalObject.ObjectIs(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'Object.is', ThrowError);

  Left := Args.GetElement(0);
  Right := Args.GetElement(1);

  if IsSameValue(Left, Right) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;


function TGocciaGlobalObject.ObjectKeys(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Object.keys', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.keys called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  Keys := TGocciaArrayValue.Create;

  Names := Obj.GetEnumerablePropertyNames;
  for I := 0 to High(Names) do
    Keys.Elements.Add(TGocciaStringLiteralValue.Create(Names[I]));

  Result := Keys;
end;

function TGocciaGlobalObject.ObjectValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  PropertyValues: TArray<TGocciaValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Object.values', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.values called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  Values := TGocciaArrayValue.Create;

  PropertyValues := Obj.GetEnumerablePropertyValues;
  for I := 0 to High(PropertyValues) do
    Values.Elements.Add(PropertyValues[I]);

  Result := Values;
end;

function TGocciaGlobalObject.ObjectEntries(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Object.entries', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.entries called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  Entries := TGocciaArrayValue.Create;

  PropertyEntries := Obj.GetEnumerablePropertyEntries;
  for I := 0 to High(PropertyEntries) do
  begin
    Entry := TGocciaArrayValue.Create;
    Entry.Elements.Add(TGocciaStringLiteralValue.Create(PropertyEntries[I].Key));
    Entry.Elements.Add(PropertyEntries[I].Value);
    Entries.Elements.Add(Entry);
  end;

  Result := Entries;
end;

function TGocciaGlobalObject.ObjectAssign(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I, J: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(Args, 2, 'Object.assign', ThrowError);

  // TODO: Should check for the first object or filter out non-objects
  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.assign called on non-object', 0, 0);

  InitialObj := TGocciaObjectValue(Args.GetElement(0));

  for I := 1 to Args.Length - 1 do
  begin
    if (Args.GetElement(I) is TGocciaObjectValue) then
    begin
      Source := TGocciaObjectValue(Args.GetElement(I));

      // Use enumerable property entries to safely copy properties
      PropertyEntries := Source.GetEnumerablePropertyEntries;
      for J := 0 to High(PropertyEntries) do
        InitialObj.AssignProperty(PropertyEntries[J].Key, PropertyEntries[J].Value);
    end;
  end;

  Result := InitialObj;
end;

function TGocciaGlobalObject.ObjectCreate(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NewObj: TGocciaObjectValue;
  ProtoArg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(Args, 1, 'Object.create', ThrowError);

  ProtoArg := Args.GetElement(0);

  // Validate the prototype argument - must be an object or null
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowError('Object.create called on non-object', 0, 0);

  // Create a new object with the specified prototype
  if ProtoArg is TGocciaObjectValue then
    Result := TGocciaObjectValue.Create(TGocciaObjectValue(ProtoArg))
  else if ProtoArg is TGocciaNullLiteralValue then
    Result := TGocciaObjectValue.Create(nil)
  else
    ThrowError('Object.create called on non-object', 0, 0);
end;

function TGocciaGlobalObject.ObjectHasOwn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  ClassObj: TGocciaClassValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'Object.hasOwn', ThrowError);

  // Handle class values (TGocciaClassValue does not extend TGocciaObjectValue)
  if Args.GetElement(0) is TGocciaClassValue then
  begin
    ClassObj := TGocciaClassValue(Args.GetElement(0));
    PropertyName := Args.GetElement(1).ToStringLiteral.Value;
    // Check static properties on the class (private fields are never own properties)
    Result := TGocciaBooleanLiteralValue.Create(
      not (ClassObj.GetProperty(PropertyName) is TGocciaUndefinedLiteralValue));
    Exit;
  end;

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.hasOwn called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  PropertyName := Args.GetElement(1).ToStringLiteral.Value;

  Result := TGocciaBooleanLiteralValue.Create(Obj.HasOwnProperty(PropertyName));
end;

function TGocciaGlobalObject.ObjectGetOwnPropertyNames(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Names: TGocciaArrayValue;
  PropertyNames: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Object.getOwnPropertyNames', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyNames called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  Names := TGocciaArrayValue.Create;

  // Get all property names (both enumerable and non-enumerable)
  PropertyNames := Obj.GetAllPropertyNames;
  for I := 0 to High(PropertyNames) do
    Names.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));

  Result := Names;
end;

function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj, DescriptorObj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'Object.getOwnPropertyDescriptor', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyDescriptor called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  PropertyName := Args.GetElement(1).ToStringLiteral.Value;

  Descriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  if Descriptor = nil then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end else begin
    DescriptorObj := TGocciaObjectValue.Create;
    DescriptorObj.AssignProperty('enumerable', TGocciaBooleanLiteralValue.Create(Descriptor.Enumerable));
    DescriptorObj.AssignProperty('configurable', TGocciaBooleanLiteralValue.Create(Descriptor.Configurable));

    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      // Data descriptor: has value and writable properties
      DescriptorObj.AssignProperty('value', TGocciaPropertyDescriptorData(Descriptor).Value);
      DescriptorObj.AssignProperty('writable', TGocciaBooleanLiteralValue.Create(Descriptor.Writable));
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      // Accessor descriptor: has get and set properties
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
        DescriptorObj.AssignProperty('get', TGocciaPropertyDescriptorAccessor(Descriptor).Getter)
      else
        DescriptorObj.AssignProperty('get', TGocciaUndefinedLiteralValue.UndefinedValue);

      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
        DescriptorObj.AssignProperty('set', TGocciaPropertyDescriptorAccessor(Descriptor).Setter)
      else
        DescriptorObj.AssignProperty('set', TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    Result := DescriptorObj;
  end;
end;

function TGocciaGlobalObject.ObjectDefineProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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
  TGocciaArgumentValidator.RequireExactly(Args, 3, 'Object.defineProperty', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.defineProperty called on non-object', 0, 0);

  if not (Args.GetElement(2) is TGocciaObjectValue) then
    ThrowError('Object.defineProperty: descriptor must be an object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  PropertyName := Args.GetElement(1).ToStringLiteral.Value;
  DescriptorObject := TGocciaObjectValue(Args.GetElement(2));

  // Initialize all variables
  Enumerable := False;
  Configurable := False;
  Writable := False;
  Value := nil;
  Getter := nil;
  Setter := nil;

  // Get descriptor properties (defaults to false for missing properties)
  if DescriptorObject.HasProperty('enumerable') then
    Enumerable := DescriptorObject.GetProperty('enumerable').ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty('configurable') then
    Configurable := DescriptorObject.GetProperty('configurable').ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty('writable') then
    Writable := DescriptorObject.GetProperty('writable').ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty('value') then
    Value := DescriptorObject.GetProperty('value');
  if DescriptorObject.HasProperty('get') then
  begin
    Getter := DescriptorObject.GetProperty('get');
    // Validate getter: must be a function or undefined
    if not (Getter is TGocciaUndefinedLiteralValue) and not (Getter is TGocciaFunctionValue) and not (Getter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: getter must be a function or undefined', 0, 0);
    // Treat undefined as nil
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;
  if DescriptorObject.HasProperty('set') then
  begin
    Setter := DescriptorObject.GetProperty('set');
    // Validate setter: must be a function or undefined
    if not (Setter is TGocciaUndefinedLiteralValue) and not (Setter is TGocciaFunctionValue) and not (Setter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: setter must be a function or undefined', 0, 0);
    // Treat undefined as nil
    if Setter is TGocciaUndefinedLiteralValue then
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

function TGocciaGlobalObject.ObjectDefineProperties(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertiesDescriptor: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'Object.defineProperties', ThrowError);

  if not (Args.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.defineProperties called on non-object', 0, 0);

  if not (Args.GetElement(1) is TGocciaObjectValue) then
    ThrowError('Object.defineProperties: properties must be an object', 0, 0);

  Obj := TGocciaObjectValue(Args.GetElement(0));
  PropertiesDescriptor := TGocciaObjectValue(Args.GetElement(1));
  PropertyEntries := PropertiesDescriptor.GetEnumerablePropertyEntries;

  for I := 0 to High(PropertyEntries) do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Obj);
      CallArgs.Add(TGocciaStringLiteralValue.Create(PropertyEntries[I].Key));
      CallArgs.Add(PropertyEntries[I].Value);
      ObjectDefineProperty(CallArgs, ThisValue);
    finally
      CallArgs.Free;
    end;
  end;

  Result := Obj;
end;

end.
