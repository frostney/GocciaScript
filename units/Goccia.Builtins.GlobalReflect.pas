unit Goccia.Builtins.GlobalReflect;

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
  TGocciaGlobalReflect = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
  published
    function ReflectApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectConstruct(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectDeleteProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectGetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectIsExtensible(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectOwnKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReflectSetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

{ Helper: validate target is an object, throw TypeError if not }

procedure RequireObjectTarget(const ATarget: TGocciaValue; const AMethodName: string);
begin
  if not (ATarget is TGocciaObjectValue) then
    ThrowTypeError(Format('%s: target must be an object', [AMethodName]));
end;

{ TGocciaGlobalReflect }

constructor TGocciaGlobalReflect.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(ReflectApply, 3, gmkStaticMethod);
    Members.AddMethod(ReflectConstruct, 2, gmkStaticMethod);
    Members.AddMethod(ReflectDefineProperty, 3, gmkStaticMethod);
    Members.AddMethod(ReflectDeleteProperty, 2, gmkStaticMethod);
    Members.AddMethod(ReflectGet, 2, gmkStaticMethod);
    Members.AddMethod(ReflectGetOwnPropertyDescriptor, 2, gmkStaticMethod);
    Members.AddMethod(ReflectGetPrototypeOf, 1, gmkStaticMethod);
    Members.AddMethod(ReflectHas, 2, gmkStaticMethod);
    Members.AddMethod(ReflectIsExtensible, 1, gmkStaticMethod);
    Members.AddMethod(ReflectOwnKeys, 1, gmkStaticMethod);
    Members.AddMethod(ReflectPreventExtensions, 1, gmkStaticMethod);
    Members.AddMethod(ReflectSet, 3, gmkStaticMethod);
    Members.AddMethod(ReflectSetPrototypeOf, 2, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Reflect'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtConst);
end;

// ES2026 §28.1.1 Reflect.apply(target, thisArgument, argumentsList)
function TGocciaGlobalReflect.ReflectApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, ThisArg: TGocciaValue;
  ArgsList: TGocciaValue;
  ArgsArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.apply', ThrowError);

  Target := AArgs.GetElement(0);
  ThisArg := AArgs.GetElement(1);
  ArgsList := AArgs.GetElement(2);

  // Step 1: If IsCallable(target) is false, throw a TypeError exception
  if not (Target is TGocciaFunctionBase) then
    ThrowTypeError('Reflect.apply: target must be a function');

  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList)
  if not (ArgsList is TGocciaArrayValue) then
    ThrowTypeError('Reflect.apply: argumentsList must be an array');

  ArgsArray := TGocciaArrayValue(ArgsList);
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to ArgsArray.Elements.Count - 1 do
      CallArgs.Add(ArgsArray.Elements[I]);

    // Step 3: Return ? Call(target, thisArgument, args)
    Result := TGocciaFunctionBase(Target).Call(CallArgs, ThisArg);
  finally
    CallArgs.Free;
  end;
end;

// ES2026 §28.1.2 Reflect.construct(target, argumentsList [, newTarget])
function TGocciaGlobalReflect.ReflectConstruct(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
  ArgsList: TGocciaValue;
  NewTarget: TGocciaValue;
  ArgsArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  Instance: TGocciaValue;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.construct', ThrowError);

  Target := AArgs.GetElement(0);
  ArgsList := AArgs.GetElement(1);

  // Step 1: If IsConstructor(target) is false, throw a TypeError exception
  if not (Target is TGocciaClassValue) then
    ThrowTypeError('Reflect.construct: target must be a constructor');

  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList)
  if not (ArgsList is TGocciaArrayValue) then
    ThrowTypeError('Reflect.construct: argumentsList must be an array');

  // Step 3: If IsConstructor(newTarget) is false, throw a TypeError exception
  if AArgs.Length >= 3 then
  begin
    NewTarget := AArgs.GetElement(2);
    if not (NewTarget is TGocciaClassValue) then
      ThrowTypeError('Reflect.construct: newTarget must be a constructor');
  end
  else
    NewTarget := Target;

  ArgsArray := TGocciaArrayValue(ArgsList);
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to ArgsArray.Elements.Count - 1 do
      CallArgs.Add(ArgsArray.Elements[I]);

    // Step 4: Return ? Construct(target, args, newTarget)
    Instance := TGocciaClassValue(Target).Instantiate(CallArgs);

    // If newTarget differs from target, set prototype to newTarget.prototype
    if (NewTarget <> Target) and (Instance is TGocciaObjectValue) then
    begin
      if Assigned(TGocciaClassValue(NewTarget).Prototype) then
        TGocciaObjectValue(Instance).Prototype := TGocciaClassValue(NewTarget).Prototype;
    end;
  finally
    CallArgs.Free;
  end;

  Result := Instance;
end;

// ES2026 §28.1.3 Reflect.defineProperty(target, propertyKey, attributes)
function TGocciaGlobalReflect.ReflectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey, Attrs: TGocciaValue;
  Obj, DescriptorObject: TGocciaObjectValue;
  PropertyName: string;
  Descriptor: TGocciaPropertyDescriptor;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  Enumerable, Configurable, Writable: Boolean;
  Value, Getter, Setter: TGocciaValue;
  PropertyFlags: TPropertyFlags;
  IsSymbolKey: Boolean;
  SymbolKey: TGocciaSymbolValue;
  HasValue, HasGet, HasSet: Boolean;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.defineProperty', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);
  Attrs := AArgs.GetElement(2);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.defineProperty');

  if not (Attrs is TGocciaObjectValue) then
    ThrowTypeError('Reflect.defineProperty: attributes must be an object');

  Obj := TGocciaObjectValue(Target);
  DescriptorObject := TGocciaObjectValue(Attrs);

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  IsSymbolKey := PropKey is TGocciaSymbolValue;
  SymbolKey := nil;
  PropertyName := '';
  ExistingDescriptor := nil;

  if IsSymbolKey then
  begin
    SymbolKey := TGocciaSymbolValue(PropKey);
    ExistingDescriptor := Obj.GetOwnSymbolPropertyDescriptor(SymbolKey);
  end
  else
  begin
    PropertyName := PropKey.ToStringLiteral.Value;
    ExistingDescriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  end;

  // Step 3: Let desc be ? ToPropertyDescriptor(Attributes)
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

  HasValue := DescriptorObject.HasProperty(PROP_VALUE);
  HasGet := DescriptorObject.HasProperty(PROP_GET);
  HasSet := DescriptorObject.HasProperty(PROP_SET);

  if DescriptorObject.HasProperty(PROP_ENUMERABLE) then
    Enumerable := DescriptorObject.GetProperty(PROP_ENUMERABLE).ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty(PROP_CONFIGURABLE) then
    Configurable := DescriptorObject.GetProperty(PROP_CONFIGURABLE).ToBooleanLiteral.Value;
  if DescriptorObject.HasProperty(PROP_WRITABLE) then
    Writable := DescriptorObject.GetProperty(PROP_WRITABLE).ToBooleanLiteral.Value;
  if HasValue then
    Value := DescriptorObject.GetProperty(PROP_VALUE);
  if HasGet then
  begin
    Getter := DescriptorObject.GetProperty(PROP_GET);
    if not (Getter is TGocciaUndefinedLiteralValue) and not (Getter is TGocciaFunctionBase) then
      ThrowTypeError('Reflect.defineProperty: getter must be a function or undefined');
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;
  if HasSet then
  begin
    Setter := DescriptorObject.GetProperty(PROP_SET);
    if not (Setter is TGocciaUndefinedLiteralValue) and not (Setter is TGocciaFunctionBase) then
      ThrowTypeError('Reflect.defineProperty: setter must be a function or undefined');
    if Setter is TGocciaUndefinedLiteralValue then
      Setter := nil;
  end;

  // ES2026 §6.2.5.5 ToPropertyDescriptor step 10: mixed data+accessor is invalid
  if (HasValue or DescriptorObject.HasProperty(PROP_WRITABLE)) and (HasGet or HasSet) then
    ThrowTypeError('Reflect.defineProperty: descriptor cannot have both accessor and data properties');

  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  if HasValue or DescriptorObject.HasProperty(PROP_WRITABLE) or
     (Assigned(ExistingDescriptor) and (ExistingDescriptor is TGocciaPropertyDescriptorData) and not HasGet and not HasSet) or
     (not Assigned(ExistingDescriptor) and not HasGet and not HasSet) then
    Descriptor := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags)
  else
    Descriptor := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);

  // Step 4: Return target.[[DefineOwnProperty]](key, desc) — returns boolean
  try
    if IsSymbolKey then
      Obj.DefineSymbolProperty(SymbolKey, Descriptor)
    else
      Obj.DefineProperty(PropertyName, Descriptor);
    // DefineProperty/DefineSymbolProperty takes ownership on success
    Descriptor := nil;
    Result := TGocciaBooleanLiteralValue.TrueValue;
  except
    on TGocciaThrowValue do
    begin
      Descriptor.Free;
      Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  end;
end;

// ES2026 §28.1.4 Reflect.deleteProperty(target, propertyKey)
function TGocciaGlobalReflect.ReflectDeleteProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey: TGocciaValue;
  Obj: TGocciaObjectValue;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.deleteProperty', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.deleteProperty');

  Obj := TGocciaObjectValue(Target);

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  // Step 3: Return ? target.[[Delete]](key)
  if PropKey is TGocciaSymbolValue then
  begin
    if Obj.DeleteSymbolProperty(TGocciaSymbolValue(PropKey)) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  PropertyName := PropKey.ToStringLiteral.Value;
  if Obj.DeleteProperty(PropertyName) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §28.1.5 Reflect.get(target, propertyKey [, receiver])
function TGocciaGlobalReflect.ReflectGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey: TGocciaValue;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.get', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.get');

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  if PropKey is TGocciaSymbolValue then
  begin
    Result := TGocciaObjectValue(Target).GetSymbolProperty(TGocciaSymbolValue(PropKey));
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  PropertyName := PropKey.ToStringLiteral.Value;

  // Step 3-4: Return ? target.[[Get]](key, receiver)
  Result := TGocciaObjectValue(Target).GetProperty(PropertyName);
end;

// ES2026 §28.1.6 Reflect.getOwnPropertyDescriptor(target, propertyKey)
function TGocciaGlobalReflect.ReflectGetOwnPropertyDescriptor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey: TGocciaValue;
  Obj, DescriptorObj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.getOwnPropertyDescriptor', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.getOwnPropertyDescriptor');

  Obj := TGocciaObjectValue(Target);

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  // Step 3: Let desc be ? target.[[GetOwnProperty]](key)
  if PropKey is TGocciaSymbolValue then
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(PropKey))
  else
  begin
    PropertyName := PropKey.ToStringLiteral.Value;
    Descriptor := Obj.GetOwnPropertyDescriptor(PropertyName);
  end;

  // Step 4: Return FromPropertyDescriptor(desc)
  if Descriptor = nil then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  DescriptorObj := TGocciaObjectValue.Create;
  if Descriptor.Enumerable then
    DescriptorObj.AssignProperty(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.TrueValue)
  else
    DescriptorObj.AssignProperty(PROP_ENUMERABLE, TGocciaBooleanLiteralValue.FalseValue);
  if Descriptor.Configurable then
    DescriptorObj.AssignProperty(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.TrueValue)
  else
    DescriptorObj.AssignProperty(PROP_CONFIGURABLE, TGocciaBooleanLiteralValue.FalseValue);

  if Descriptor is TGocciaPropertyDescriptorData then
  begin
    DescriptorObj.AssignProperty(PROP_VALUE, TGocciaPropertyDescriptorData(Descriptor).Value);
    if Descriptor.Writable then
      DescriptorObj.AssignProperty(PROP_WRITABLE, TGocciaBooleanLiteralValue.TrueValue)
    else
      DescriptorObj.AssignProperty(PROP_WRITABLE, TGocciaBooleanLiteralValue.FalseValue);
  end
  else if Descriptor is TGocciaPropertyDescriptorAccessor then
  begin
    if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
      DescriptorObj.AssignProperty(PROP_GET, TGocciaPropertyDescriptorAccessor(Descriptor).Getter)
    else
      DescriptorObj.AssignProperty(PROP_GET, TGocciaUndefinedLiteralValue.UndefinedValue);
    if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
      DescriptorObj.AssignProperty(PROP_SET, TGocciaPropertyDescriptorAccessor(Descriptor).Setter)
    else
      DescriptorObj.AssignProperty(PROP_SET, TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Result := DescriptorObj;
end;

// ES2026 §28.1.7 Reflect.getPrototypeOf(target)
function TGocciaGlobalReflect.ReflectGetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Reflect.getPrototypeOf', ThrowError);

  Target := AArgs.GetElement(0);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.getPrototypeOf');

  // Step 2: Return ? target.[[GetPrototypeOf]]()
  if Assigned(TGocciaObjectValue(Target).Prototype) then
    Result := TGocciaObjectValue(Target).Prototype
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §28.1.8 Reflect.has(target, propertyKey)
function TGocciaGlobalReflect.ReflectHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey: TGocciaValue;
  Obj: TGocciaObjectValue;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.has', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.has');

  Obj := TGocciaObjectValue(Target);

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  if PropKey is TGocciaSymbolValue then
  begin
    if Obj.HasSymbolProperty(TGocciaSymbolValue(PropKey)) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  PropertyName := PropKey.ToStringLiteral.Value;

  // Step 3: Return ? target.[[HasProperty]](key)
  if Obj.HasProperty(PropertyName) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §28.1.9 Reflect.isExtensible(target)
function TGocciaGlobalReflect.ReflectIsExtensible(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Reflect.isExtensible', ThrowError);

  Target := AArgs.GetElement(0);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.isExtensible');

  // Step 2: Return ? target.[[IsExtensible]]()
  if TGocciaObjectValue(Target).Extensible then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §28.1.10 Reflect.ownKeys(target)
function TGocciaGlobalReflect.ReflectOwnKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  PropertyNames: TArray<string>;
  OwnSymbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Reflect.ownKeys', ThrowError);

  Target := AArgs.GetElement(0);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.ownKeys');

  Obj := TGocciaObjectValue(Target);
  Keys := TGocciaArrayValue.Create;

  // Step 2: Let keys be ? target.[[OwnPropertyKeys]]()
  // String keys first, then symbol keys (per spec ordering)
  PropertyNames := Obj.GetAllPropertyNames;
  for I := 0 to High(PropertyNames) do
    Keys.Elements.Add(TGocciaStringLiteralValue.Create(PropertyNames[I]));

  OwnSymbols := Obj.GetOwnSymbols;
  for I := 0 to High(OwnSymbols) do
    Keys.Elements.Add(OwnSymbols[I]);

  // Step 3: Return CreateArrayFromList(keys)
  Result := Keys;
end;

// ES2026 §28.1.11 Reflect.preventExtensions(target)
function TGocciaGlobalReflect.ReflectPreventExtensions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Reflect.preventExtensions', ThrowError);

  Target := AArgs.GetElement(0);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.preventExtensions');

  // Step 2: Return ? target.[[PreventExtensions]]()
  TGocciaObjectValue(Target).PreventExtensions;
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §28.1.12 Reflect.set(target, propertyKey, V [, receiver])
function TGocciaGlobalReflect.ReflectSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey, Value: TGocciaValue;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.set', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);
  Value := AArgs.GetElement(2);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.set');

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  // Step 3-4: Return ? target.[[Set]](key, V, receiver)
  try
    if PropKey is TGocciaSymbolValue then
      TGocciaObjectValue(Target).AssignSymbolProperty(
        TGocciaSymbolValue(PropKey), Value)
    else
    begin
      PropertyName := PropKey.ToStringLiteral.Value;
      TGocciaObjectValue(Target).AssignProperty(PropertyName, Value);
    end;
    Result := TGocciaBooleanLiteralValue.TrueValue;
  except
    on TGocciaThrowValue do
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

// ES2026 §28.1.13 Reflect.setPrototypeOf(target, proto)
function TGocciaGlobalReflect.ReflectSetPrototypeOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, ProtoArg: TGocciaValue;
  Walker: TGocciaObjectValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.setPrototypeOf', ThrowError);

  Target := AArgs.GetElement(0);
  ProtoArg := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.setPrototypeOf');

  // Step 2: If Type(proto) is neither Object nor Null, throw a TypeError exception
  if not (ProtoArg is TGocciaObjectValue) and not (ProtoArg is TGocciaNullLiteralValue) then
    ThrowTypeError('Reflect.setPrototypeOf: proto must be an object or null');

  // Step 3: Return ? target.[[SetPrototypeOf]](proto)
  // ES2026 §10.1.2 OrdinarySetPrototypeOf step 2: If SameValue(V, current) return true
  if ProtoArg is TGocciaNullLiteralValue then
  begin
    if not Assigned(TGocciaObjectValue(Target).Prototype) then
    begin
      Result := TGocciaBooleanLiteralValue.TrueValue;
      Exit;
    end;
  end
  else if (ProtoArg is TGocciaObjectValue) and
          (TGocciaObjectValue(Target).Prototype = TGocciaObjectValue(ProtoArg)) then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // ES2026 §10.1.2 OrdinarySetPrototypeOf step 3: If extensible is false, return false
  if not TGocciaObjectValue(Target).Extensible then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // ES2026 §10.1.2 OrdinarySetPrototypeOf step 4: Cycle detection
  if ProtoArg is TGocciaObjectValue then
  begin
    Walker := TGocciaObjectValue(ProtoArg);
    while Assigned(Walker) do
    begin
      if Walker = TGocciaObjectValue(Target) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      Walker := Walker.Prototype;
    end;
  end;

  if ProtoArg is TGocciaNullLiteralValue then
    TGocciaObjectValue(Target).Prototype := nil
  else
    TGocciaObjectValue(Target).Prototype := TGocciaObjectValue(ProtoArg);

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

end.
