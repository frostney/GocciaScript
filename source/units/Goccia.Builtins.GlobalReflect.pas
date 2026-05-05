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

  Goccia.Arguments.ArrayLike,
  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

{ Helper: validate target is an object, throw TypeError if not }

procedure RequireObjectTarget(const ATarget: TGocciaValue; const AMethodName: string);
begin
  if not (ATarget is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorReflectTargetMustBeObject, [AMethodName]), SSuggestReflectObjectArg);
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
  CallArgs: TGocciaArgumentsCollection;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.apply', ThrowError);

  Target := AArgs.GetElement(0);
  ThisArg := AArgs.GetElement(1);
  ArgsList := AArgs.GetElement(2);

  // Step 1: If IsCallable(target) is false, throw a TypeError exception
  if not (Target is TGocciaFunctionBase) then
    ThrowTypeError(SErrorReflectApplyTargetMustBeFunction, SSuggestNotFunctionType);

  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList)
  CallArgs := CreateListFromArrayLike(ArgsList, 'Reflect.apply');
  try
    // Step 3: Return ? Call(target, thisArgument, args)
    Result := TGocciaFunctionBase(Target).Call(CallArgs, ThisArg);
  finally
    CallArgs.Free;
  end;
end;

// ES2026 §28.1.2 Reflect.construct(target, argumentsList [, newTarget])
// ES2026 §7.2.4 IsConstructor(argument): defer to the value's virtual
// IsConstructable. Each TGocciaFunctionBase descendant overrides it to
// match the spec — function declarations/expressions return true (they own
// a `prototype` data property), while arrow, async, generator, async-
// generator, concise method, and revoked-proxy targets return false.
// Bound functions delegate to their underlying [[BoundTargetFunction]],
// matching ES2026 §10.4.1.2.
function IsConstructorValue(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue.IsConstructable;
end;

// ES2026 §10.1.14 GetPrototypeFromConstructor: Get(constructor, "prototype")
// directly — no bound-function unwrap. If the property is not an Object,
// fall back to %Object.prototype%. A plain bound function whose user has
// not assigned an own `prototype` walks its prototype chain to undefined,
// which lands on the fallback per spec.
function GetProtoFromConstructor(const ANewTarget: TGocciaValue): TGocciaObjectValue;
var
  ProtoValue: TGocciaValue;
begin
  if ANewTarget is TGocciaObjectValue then
    ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
  else
    ProtoValue := nil;

  if ProtoValue is TGocciaObjectValue then
    Result := TGocciaObjectValue(ProtoValue)
  else
  begin
    if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
      TGocciaObjectValue.InitializeSharedPrototype;
    Result := TGocciaObjectValue.SharedObjectPrototype;
  end;
end;

// ES2026 §10.2.2 [[Construct]] for ordinary function objects: call the body
// with the receiver as `this`; if the body returns an Object, that wins —
// otherwise the receiver is the result.
function ConstructOrdinaryWithReceiver(const ATarget: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection;
  const AReceiver: TGocciaObjectValue): TGocciaValue;
var
  ReturnValue: TGocciaValue;
begin
  TGarbageCollector.Instance.AddTempRoot(AReceiver);
  try
    ReturnValue := ATarget.Call(AArguments, AReceiver);
    if ReturnValue is TGocciaObjectValue then
      Result := ReturnValue
    else
      Result := AReceiver;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(AReceiver);
  end;
end;

function TGocciaGlobalReflect.ReflectConstruct(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
  ArgsList: TGocciaValue;
  NewTarget: TGocciaValue;
  CallArgs, CombinedArgs: TGocciaArgumentsCollection;
  NewTargetClass: TGocciaClassValue;
  Receiver: TGocciaObjectValue;
  EffectiveTarget: TGocciaValue;
  BoundFn: TGocciaBoundFunctionValue;
  I: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.construct', ThrowError);

  Target := AArgs.GetElement(0);
  ArgsList := AArgs.GetElement(1);

  // Step 1: If IsConstructor(target) is false, throw a TypeError exception
  if not IsConstructorValue(Target) then
    ThrowTypeError(SErrorReflectConstructTargetMustBeConstructor, SSuggestNotConstructorType);

  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList)
  CallArgs := CreateListFromArrayLike(ArgsList, 'Reflect.construct');
  try
    // Step 3: If newTarget provided, validate IsConstructor(newTarget)
    if AArgs.Length >= 3 then
    begin
      NewTarget := AArgs.GetElement(2);
      if not IsConstructorValue(NewTarget) then
        ThrowTypeError(SErrorReflectConstructNewTargetMustBeConstructor, SSuggestNotConstructorType);
    end
    else
      NewTarget := Target;

    // Bound function target (§10.4.1.2): at each level, merge the bound
    // arguments into the call list and apply step 5's substitution —
    // SameValue(F, newTarget) ⇒ newTarget := F.[[BoundTargetFunction]] —
    // before recursing into the underlying target. The substitution preserves
    // an explicit non-bound newTarget (e.g. Reflect.construct(Bound, [], C))
    // while default-newTarget calls follow the bound chain to the eventual
    // ordinary constructor.
    EffectiveTarget := Target;
    while EffectiveTarget is TGocciaBoundFunctionValue do
    begin
      BoundFn := TGocciaBoundFunctionValue(EffectiveTarget);
      if NewTarget = EffectiveTarget then
        NewTarget := BoundFn.OriginalFunction;
      CombinedArgs := TGocciaArgumentsCollection.CreateWithCapacity(
        BoundFn.BoundArgCount + CallArgs.Length);
      try
        for I := 0 to BoundFn.BoundArgCount - 1 do
          CombinedArgs.Add(BoundFn.GetBoundArg(I));
        for I := 0 to CallArgs.Length - 1 do
          CombinedArgs.Add(CallArgs.GetElement(I));
      except
        CombinedArgs.Free;
        raise;
      end;
      CallArgs.Free;
      CallArgs := CombinedArgs;
      EffectiveTarget := BoundFn.OriginalFunction;
    end;

    // Step 4: Return ? Construct(target, args, newTarget)
    if EffectiveTarget is TGocciaProxyValue then
      Result := TGocciaProxyValue(EffectiveTarget).ConstructTrap(CallArgs, NewTarget)
    else if EffectiveTarget is TGocciaClassValue then
    begin
      if NewTarget is TGocciaClassValue then
      begin
        NewTargetClass := TGocciaClassValue(NewTarget);
        if NewTargetClass <> TGocciaClassValue(EffectiveTarget) then
          Result := TGocciaClassValue(EffectiveTarget).Instantiate(CallArgs, NewTargetClass)
        else
          Result := TGocciaClassValue(EffectiveTarget).Instantiate(CallArgs);
      end
      else
      begin
        // newTarget is a non-class constructor — patch the instance prototype
        // from newTarget.prototype after instantiation (Instantiate only
        // accepts a class for its newTarget parameter). GetProtoFromConstructor
        // applies the §10.1.14 fallback to %Object.prototype% when
        // newTarget.prototype is not an Object.
        Result := TGocciaClassValue(EffectiveTarget).Instantiate(CallArgs);
        if Result is TGocciaObjectValue then
          TGocciaObjectValue(Result).Prototype := GetProtoFromConstructor(NewTarget);
      end;
    end
    else if EffectiveTarget is TGocciaNativeFunctionValue then
    begin
      // Native constructors create their own internal-slot-backed instance;
      // the receiver is unused so we pass HoleValue. newTarget-driven
      // prototype propagation for native constructors is not yet wired here.
      Result := TGocciaNativeFunctionValue(EffectiveTarget).Call(CallArgs,
        TGocciaHoleValue.HoleValue);
    end
    else if EffectiveTarget is TGocciaFunctionBase then
    begin
      Receiver := TGocciaObjectValue.Create(GetProtoFromConstructor(NewTarget));
      Result := ConstructOrdinaryWithReceiver(TGocciaFunctionBase(EffectiveTarget),
        CallArgs, Receiver);
    end
    else
      ThrowTypeError(SErrorReflectConstructTargetMustBeConstructor,
        SSuggestNotConstructorType);
  finally
    CallArgs.Free;
  end;
end;

// ES2026 §28.1.3 Reflect.defineProperty(target, propertyKey, attributes)
function TGocciaGlobalReflect.ReflectDefineProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target, PropKey, Attrs: TGocciaValue;
  Obj: TGocciaObjectValue;
  DescriptorObject: TGocciaObjectValue;
  PropertyName: string;
  Descriptor: TGocciaPropertyDescriptor;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  IsSymbolKey: Boolean;
  SymbolKey: TGocciaSymbolValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.defineProperty', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);
  Attrs := AArgs.GetElement(2);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.defineProperty');

  if not (Attrs is TGocciaObjectValue) then
    ThrowTypeError(SErrorReflectDefinePropertyAttrsMustBeObject, SSuggestPropertyDescriptorObject);

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
  Descriptor := ToPropertyDescriptor(DescriptorObject, ExistingDescriptor);

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
  Target, PropKey, Receiver: TGocciaValue;
  PropertyName: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'Reflect.get', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.get');

  // Step 3: If receiver is not present, set receiver to target
  if AArgs.Length >= 3 then
    Receiver := AArgs.GetElement(2)
  else
    Receiver := Target;

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  // Step 4: Return ? target.[[Get]](key, receiver)
  if PropKey is TGocciaSymbolValue then
  begin
    Result := TGocciaObjectValue(Target).GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue(PropKey), Receiver);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  PropertyName := PropKey.ToStringLiteral.Value;

  // Step 4: Return ? target.[[Get]](key, receiver)
  Result := TGocciaObjectValue(Target).GetPropertyWithContext(PropertyName, Receiver);
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
  Target, PropKey, Value, Receiver: TGocciaValue;
  PropertyName: string;
  Success: Boolean;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Reflect.set', ThrowError);

  Target := AArgs.GetElement(0);
  PropKey := AArgs.GetElement(1);
  Value := AArgs.GetElement(2);

  // Step 1: If target is not an Object, throw a TypeError exception
  RequireObjectTarget(Target, 'Reflect.set');

  // Step 4: If receiver is not present, set receiver to target
  if AArgs.Length >= 4 then
    Receiver := AArgs.GetElement(3)
  else
    Receiver := Target;

  // Step 2: Let key be ? ToPropertyKey(propertyKey)
  // Step 3, 5: Return ? target.[[Set]](key, V, receiver)
  if PropKey is TGocciaSymbolValue then
    Success := TGocciaObjectValue(Target).AssignSymbolPropertyWithReceiver(
      TGocciaSymbolValue(PropKey), Value, Receiver)
  else
  begin
    PropertyName := PropKey.ToStringLiteral.Value;
    Success := TGocciaObjectValue(Target).AssignPropertyWithReceiver(
      PropertyName, Value, Receiver);
  end;

  if Success then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
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
    ThrowTypeError(SErrorReflectSetPrototypeOfProtoType, SSuggestReflectObjectArg);

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
