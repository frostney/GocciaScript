unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  protected
    // Native methods
    function ObjectIs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyNames(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ObjectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
  Goccia.Evaluator.Comparison,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

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
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGetOwnPropertySymbols, 'getOwnPropertySymbols', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectFreeze, 'freeze', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectIsFrozen, 'isFrozen', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGetPrototypeOf, 'getPrototypeOf', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectFromEntries, 'fromEntries', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectSeal, 'seal', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectIsSealed, 'isSealed', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectPreventExtensions, 'preventExtensions', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectIsExtensible, 'isExtensible', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectSetPrototypeOf, 'setPrototypeOf', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ObjectGroupBy, 'groupBy', 2));
end;

function TGocciaGlobalObject.ObjectIs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.is', ThrowError);

  Left := AArgs.GetElement(0);
  Right := AArgs.GetElement(1);

  if IsSameValue(Left, Right) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;


function TGocciaGlobalObject.ObjectKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.keys', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.keys called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  Keys := TGocciaArrayValue.Create;

  Names := Obj.GetEnumerablePropertyNames;
  for I := 0 to High(Names) do
    Keys.Elements.Add(TGocciaStringLiteralValue.Create(Names[I]));

  Result := Keys;
end;

function TGocciaGlobalObject.ObjectValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  PropertyValues: TArray<TGocciaValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.values', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.values called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  Values := TGocciaArrayValue.Create;

  PropertyValues := Obj.GetEnumerablePropertyValues;
  for I := 0 to High(PropertyValues) do
    Values.Elements.Add(PropertyValues[I]);

  Result := Values;
end;

function TGocciaGlobalObject.ObjectEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.entries', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.entries called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
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

function TGocciaGlobalObject.ObjectAssign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  I, J: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.assign', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.assign called on non-object', 0, 0);

  InitialObj := TGocciaObjectValue(AArgs.GetElement(0));

  for I := 1 to AArgs.Length - 1 do
  begin
    if (AArgs.GetElement(I) is TGocciaObjectValue) then
    begin
      Source := TGocciaObjectValue(AArgs.GetElement(I));

      // Use enumerable property entries to safely copy properties
      PropertyEntries := Source.GetEnumerablePropertyEntries;
      for J := 0 to High(PropertyEntries) do
        InitialObj.AssignProperty(PropertyEntries[J].Key, PropertyEntries[J].Value);
    end;
  end;

  Result := InitialObj;
end;

function TGocciaGlobalObject.ObjectCreate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewObj: TGocciaObjectValue;
  ProtoArg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.create', ThrowError);

  ProtoArg := AArgs.GetElement(0);

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

function TGocciaGlobalObject.ObjectHasOwn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  ClassObj: TGocciaClassValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.hasOwn', ThrowError);

  // Handle class values (TGocciaClassValue does not extend TGocciaObjectValue)
  if AArgs.GetElement(0) is TGocciaClassValue then
  begin
    ClassObj := TGocciaClassValue(AArgs.GetElement(0));
    PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;
    // Check static properties on the class (private fields are never own properties)
    if not (ClassObj.GetProperty(PropertyName) is TGocciaUndefinedLiteralValue) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.hasOwn called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;

  if Obj.HasOwnProperty(PropertyName) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalObject.ObjectGetOwnPropertyNames(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Names: TGocciaArrayValue;
  PropertyNames: TArray<string>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.getOwnPropertyNames', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyNames called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  Names := TGocciaArrayValue.Create;

  // Get all property names (both enumerable and non-enumerable)
  PropertyNames := Obj.GetAllPropertyNames;
  for I := 0 to High(PropertyNames) do
    Names.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));

  Result := Names;
end;

function TGocciaGlobalObject.ObjectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj, DescriptorObj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.getOwnPropertyDescriptor', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.getOwnPropertyDescriptor called on non-object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  PropertyName := AArgs.GetElement(1).ToStringLiteral.Value;

  Descriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  if Descriptor = nil then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end else begin
    DescriptorObj := TGocciaObjectValue.Create;
    if Descriptor.Enumerable then
      DescriptorObj.AssignProperty('enumerable', TGocciaBooleanLiteralValue.TrueValue)
    else
      DescriptorObj.AssignProperty('enumerable', TGocciaBooleanLiteralValue.FalseValue);
    if Descriptor.Configurable then
      DescriptorObj.AssignProperty('configurable', TGocciaBooleanLiteralValue.TrueValue)
    else
      DescriptorObj.AssignProperty('configurable', TGocciaBooleanLiteralValue.FalseValue);

    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      // Data descriptor: has value and writable properties
      DescriptorObj.AssignProperty('value', TGocciaPropertyDescriptorData(Descriptor).Value);
      if Descriptor.Writable then
        DescriptorObj.AssignProperty('writable', TGocciaBooleanLiteralValue.TrueValue)
      else
        DescriptorObj.AssignProperty('writable', TGocciaBooleanLiteralValue.FalseValue);
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

function TGocciaGlobalObject.ObjectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
  DescriptorObject: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  Enumerable: Boolean;
  Configurable: Boolean;
  Writable: Boolean;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
  PropertyFlags: TPropertyFlags;
  IsSymbolKey: Boolean;
  SymbolKey: TGocciaSymbolValue;
  HasValue, HasGet, HasSet: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 3, 'Object.defineProperty', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.defineProperty called on non-object', 0, 0);

  if not (AArgs.GetElement(2) is TGocciaObjectValue) then
    ThrowError('Object.defineProperty: descriptor must be an object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  DescriptorObject := TGocciaObjectValue(AArgs.GetElement(2));

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

  // Initialize defaults: false for new properties, existing values for updates
  Enumerable := False;
  Configurable := False;
  Writable := False;
  Value := nil;
  Getter := nil;
  Setter := nil;

  if Assigned(ExistingDescriptor) then
  begin
    Enumerable := ExistingDescriptor.Enumerable;
    Configurable := ExistingDescriptor.Configurable;
    if ExistingDescriptor is TGocciaPropertyDescriptorData then
    begin
      Writable := ExistingDescriptor.Writable;
      Value := TGocciaPropertyDescriptorData(ExistingDescriptor).Value;
    end
    else if ExistingDescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Getter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
      Setter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
    end;
  end;

  HasValue := DescriptorObject.HasProperty('value');
  HasGet := DescriptorObject.HasProperty('get');
  HasSet := DescriptorObject.HasProperty('set');

  if DescriptorObject.HasProperty('enumerable') then
    Enumerable := DescriptorObject.GetProperty('enumerable').ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty('configurable') then
    Configurable := DescriptorObject.GetProperty('configurable').ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty('writable') then
    Writable := DescriptorObject.GetProperty('writable').ToBooleanLiteral.Value;
  if HasValue then
    Value := DescriptorObject.GetProperty('value');
  if HasGet then
  begin
    Getter := DescriptorObject.GetProperty('get');
    if not (Getter is TGocciaUndefinedLiteralValue) and not (Getter is TGocciaFunctionValue) and not (Getter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: getter must be a function or undefined', 0, 0);
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;
  if HasSet then
  begin
    Setter := DescriptorObject.GetProperty('set');
    if not (Setter is TGocciaUndefinedLiteralValue) and not (Setter is TGocciaFunctionValue) and not (Setter is TGocciaNativeFunctionValue) then
      ThrowError('TypeError: Object.defineProperty: setter must be a function or undefined', 0, 0);
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

  // Determine descriptor type: if the new descriptor specifies value/writable,
  // or if the existing descriptor is a data descriptor and no get/set is specified
  if HasValue or DescriptorObject.HasProperty('writable') or
     (Assigned(ExistingDescriptor) and (ExistingDescriptor is TGocciaPropertyDescriptorData) and not HasGet and not HasSet) or
     (not Assigned(ExistingDescriptor) and not HasGet and not HasSet) then
  begin
    Descriptor := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags);
  end
  else
  begin
    Descriptor := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
  end;

  if IsSymbolKey then
    Obj.DefineSymbolProperty(SymbolKey, Descriptor)
  else
    Obj.DefineProperty(PropertyName, Descriptor);

  Result := Obj;
end;

function TGocciaGlobalObject.ObjectDefineProperties(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertiesDescriptor: TGocciaObjectValue;
  PropertyEntries: TArray<TPair<string, TGocciaValue>>;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.defineProperties', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.defineProperties called on non-object', 0, 0);

  if not (AArgs.GetElement(1) is TGocciaObjectValue) then
    ThrowError('Object.defineProperties: properties must be an object', 0, 0);

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  PropertiesDescriptor := TGocciaObjectValue(AArgs.GetElement(1));
  PropertyEntries := PropertiesDescriptor.GetEnumerablePropertyEntries;

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

  Result := Obj;
end;

function TGocciaGlobalObject.ObjectGetOwnPropertySymbols(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  OwnSymbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Object.getOwnPropertySymbols', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaArrayValue.Create;
    Exit;
  end;

  Obj := TGocciaObjectValue(AArgs.GetElement(0));
  Arr := TGocciaArrayValue.Create;

  // Get all own symbol properties (both enumerable and non-enumerable)
  OwnSymbols := Obj.GetOwnSymbols;
  for I := 0 to High(OwnSymbols) do
    Arr.Elements.Add(OwnSymbols[I]);

  Result := Arr;
end;

function TGocciaGlobalObject.ObjectFreeze(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.freeze', ThrowError);

  // Non-objects are returned as-is (ECMAScript spec)
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  TGocciaObjectValue(AArgs.GetElement(0)).Freeze;
  Result := AArgs.GetElement(0);
end;

function TGocciaGlobalObject.ObjectIsFrozen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isFrozen', ThrowError);

  // Non-objects are always frozen (ECMAScript spec)
  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  if TGocciaObjectValue(AArgs.GetElement(0)).IsFrozen then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalObject.ObjectGetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.getPrototypeOf', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.getPrototypeOf called on non-object', 0, 0);

  if Assigned(TGocciaObjectValue(AArgs.GetElement(0)).Prototype) then
    Result := TGocciaObjectValue(AArgs.GetElement(0)).Prototype
  else
    Result := TGocciaNullLiteralValue.Create;
end;

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

  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowError('Object.fromEntries requires an iterable of key-value pairs', 0, 0);

  Entries := TGocciaArrayValue(AArgs.GetElement(0));
  Obj := TGocciaObjectValue.Create;

  for I := 0 to Entries.Elements.Count - 1 do
  begin
    if not (Entries.Elements[I] is TGocciaArrayValue) then
      ThrowError('Object.fromEntries requires an iterable of key-value pairs', 0, 0);

    Entry := TGocciaArrayValue(Entries.Elements[I]);
    if Entry.Elements.Count < 2 then
      ThrowError('Object.fromEntries requires each entry to have at least 2 elements', 0, 0);

    Key := Entry.Elements[0].ToStringLiteral.Value;
    Value := Entry.Elements[1];
    Obj.AssignProperty(Key, Value);
  end;

  Result := Obj;
end;

function TGocciaGlobalObject.ObjectSeal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.seal', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  TGocciaObjectValue(AArgs.GetElement(0)).Seal;
  Result := AArgs.GetElement(0);
end;

function TGocciaGlobalObject.ObjectIsSealed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isSealed', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  if TGocciaObjectValue(AArgs.GetElement(0)).IsSealed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalObject.ObjectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.preventExtensions', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := AArgs.GetElement(0);
    Exit;
  end;

  TGocciaObjectValue(AArgs.GetElement(0)).PreventExtensions;
  Result := AArgs.GetElement(0);
end;

function TGocciaGlobalObject.ObjectIsExtensible(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Object.isExtensible', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if TGocciaObjectValue(AArgs.GetElement(0)).IsExtensible then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalObject.ObjectSetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ProtoArg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Object.setPrototypeOf', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
    ThrowError('Object.setPrototypeOf called on non-object', 0, 0);

  ProtoArg := AArgs.GetElement(1);
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowError('Object prototype may only be an Object or null', 0, 0);

  if not TGocciaObjectValue(AArgs.GetElement(0)).IsExtensible then
    ThrowError('Object.setPrototypeOf called on non-extensible object', 0, 0);

  if ProtoArg is TGocciaNullLiteralValue then
    TGocciaObjectValue(AArgs.GetElement(0)).Prototype := nil
  else
    TGocciaObjectValue(AArgs.GetElement(0)).Prototype := TGocciaObjectValue(ProtoArg);

  Result := AArgs.GetElement(0);
end;

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
    ThrowError('Object.groupBy requires an iterable as first argument', 0, 0);
  if not AArgs.GetElement(1).IsCallable then
    ThrowError('Object.groupBy requires a callback function as second argument', 0, 0);

  Items := TGocciaArrayValue(AArgs.GetElement(0));
  Callback := AArgs.GetElement(1);
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.Prototype := nil;

  I := 0;
  while I < Items.Elements.Count do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Items.Elements[I]);
      CallArgs.Add(TGocciaNumberLiteralValue.SmallInt(I));

      if Callback is TGocciaFunctionValue then
        KeyValue := TGocciaFunctionValue(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else if Callback is TGocciaNativeFunctionValue then
        KeyValue := TGocciaNativeFunctionValue(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        ThrowError('Object.groupBy callback is not callable', 0, 0);
    finally
      CallArgs.Free;
    end;

    GroupKey := KeyValue.ToStringLiteral.Value;

    if ResultObj.HasOwnProperty(GroupKey) then
      GroupArray := TGocciaArrayValue(ResultObj.GetProperty(GroupKey))
    else
    begin
      GroupArray := TGocciaArrayValue.Create;
      ResultObj.AssignProperty(GroupKey, GroupArray);
    end;

    GroupArray.Elements.Add(Items.Elements[I]);
    Inc(I);
  end;

  Result := ResultObj;
end;

end.
