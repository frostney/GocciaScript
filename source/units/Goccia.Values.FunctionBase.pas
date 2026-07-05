unit Goccia.Values.FunctionBase;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // Forward declarations
  TGocciaBoundFunctionValue = class;

  TGocciaProxyPredicate = function(const AValue: TGocciaValue): Boolean;
  TGocciaProxyApplyHook = function(const AProxy: TGocciaValue;
    const AArguments: TGocciaArgumentsCollection;
    const AThisValue: TGocciaValue): TGocciaValue;
  TGocciaProxyConstructHook = function(const AProxy: TGocciaValue;
    const AArguments: TGocciaArgumentsCollection;
    const ANewTarget: TGocciaValue): TGocciaValue;
  TGocciaProxyGetPrototypeHook = function(
    const AProxy: TGocciaObjectValue): TGocciaValue;
  TGocciaProxyGetFunctionRealmHook = function(
    const AProxy: TGocciaValue): TGocciaRealm;
  TGocciaFunctionConstructRedirectHook = function(
    const ATarget: TGocciaValue;
    const AArguments: TGocciaArgumentsCollection;
    const ANewTarget: TGocciaValue;
    out AResult: TGocciaValue): Boolean;

  TGocciaFunctionSharedPrototype = class(TGocciaObjectValue)
  public
    constructor Create;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IsCallable: Boolean; override;
    function TypeOf: string; override;

    // Function prototype methods that are available on all functions
  public
    function RestrictedFunctionPropertyThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionBind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionHasInstance(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  // Base class for all callable functions
  TGocciaFunctionBase = class(TGocciaObjectValue)
  protected
    FStrictThis: Boolean;
    FStrictCode: Boolean;
    FHasOwnLengthProperty: Boolean;
    FHasOwnNameProperty: Boolean;
    FCreationRealm: TGocciaRealm;
    // Subclasses should override these to provide name/length
    function GetFunctionLength: Integer; virtual;
    function GetFunctionName: string; virtual;
    function GetSourceText: string; virtual;
    procedure MaterializeIntrinsicProperty(const AName: string);
  public
    constructor Create;
    class procedure SetSharedPrototypeParent(const AParent: TGocciaObjectValue);
    class function GetSharedPrototype: TGocciaFunctionSharedPrototype; static;

    // Override GetProperty to provide call, apply, bind methods and length/name
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;

    // Abstract method that subclasses must implement
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function ConstructWithReceiver(const AArguments: TGocciaArgumentsCollection;
      const AReceiver: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue; virtual;
    function CallPreparedArgs(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallThreeArgs(const AArg0, AArg1, AArg2, AThisValue: TGocciaValue): TGocciaValue; virtual;

    // name/length as own properties per ECMAScript spec
    function HasOwnProperty(const AName: string): Boolean; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function DeleteProperty(const AName: string): Boolean; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    procedure InstallSloppyFunctionCallerArgumentsProperties;

    // VMT-based type discrimination
    function IsCallable: Boolean; override;
    function IsConstructable: Boolean; override;

    // Override TypeName and TypeOf for all functions
    function TypeName: string; override;
    function TypeOf: string; override;

    // ES2026 §10.2.1.1 OrdinaryCallBindThis: when true, undefined/null this
    // stays as-is; when false, coerced to globalThis. Defaults to strict mode
    // behavior; non-strict compatibility and Function constructor calls clear it.
    property StrictThis: Boolean read FStrictThis write FStrictThis;
    property StrictCode: Boolean read FStrictCode write FStrictCode;
    property CreationRealm: TGocciaRealm read FCreationRealm;
  end;

  // Helper class for bound functions
  TGocciaBoundFunctionValue = class(TGocciaFunctionBase)
  private
    FOriginalFunction: TGocciaValue; // The function being bound
    FBoundThis: TGocciaValue; // The bound 'this' value
    FBoundArgs: TGocciaValueList; // Pre-filled arguments when count > 1
    FSingleBoundArg: TGocciaValue;
    FBoundArgCount: Integer;
    FBoundLength: Double;
    FBoundName: string;
    procedure InitializeBoundFunctionProperties;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor CreateWithoutArgs(const AOriginalFunction: TGocciaValue;
      const ABoundThis: TGocciaValue);
    constructor CreateWithSingleArg(const AOriginalFunction: TGocciaValue;
      const ABoundThis, ABoundArg: TGocciaValue);
    constructor Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
    destructor Destroy; override;
    function GetBoundArg(const AIndex: Integer): TGocciaValue;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    property OriginalFunction: TGocciaValue read FOriginalFunction;
    property BoundThis: TGocciaValue read FBoundThis;
    property BoundArgCount: Integer read FBoundArgCount;
  end;

// ES2026 §7.3.14 Call(F, V, argumentsList)
function DispatchCall(const ACallee: TGocciaValue;
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
// ES2026 §13.10.2 InstanceofOperator(value, target)
function InstanceofOperatorResult(const AInstance, ATarget: TGocciaValue): Boolean;
// ES2026 §7.3.21 OrdinaryHasInstance(constructor, instance)
function OrdinaryHasInstance(const AConstructor, AInstance: TGocciaValue): Boolean;

// ES2026 §10.1.14 GetPrototypeFromConstructor: Get(constructor, "prototype")
// directly — no bound-function unwrap. If the property is not an Object,
// fall back to %Object.prototype%. Shared by Reflect.construct (§28.1.2)
// and the proxy [[Construct]] fallback (§10.5.13).
function GetProtoFromConstructor(const ANewTarget: TGocciaValue): TGocciaObjectValue;

// ES2026 §10.1.14 variant with a per-constructor intrinsic default.
// Native constructors pass their own %Foo.prototype% so the fallback
// is correct (e.g. Error → %Error.prototype%, not %Object.prototype%).
function GetProtoFromConstructorWithIntrinsic(const ANewTarget: TGocciaValue;
  const AIntrinsicDefault: TGocciaObjectValue;
  const AIntrinsicSlot: TGocciaRealmSlotId = -1): TGocciaObjectValue;
function GetFunctionPrototypeFromConstructor(const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
function GetFunctionRealm(const AValue: TGocciaValue): TGocciaRealm;

// ES2026 §10.2.4.1 %ThrowTypeError%: one frozen thrower per realm, reused by
// AddRestrictedFunctionProperties and unmapped arguments objects.
function GetThrowTypeErrorIntrinsic: TGocciaFunctionBase;

// ES2026 §10.2.2 [[Construct]] for ordinary function objects: call the body
// with the receiver as `this`; if the body returns an Object, that wins —
// otherwise the receiver is the result. The receiver is rooted across the
// call so a GC during the body cannot collect a reachable instance.
function ConstructOrdinaryWithReceiver(const ATarget: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection;
  const AReceiver: TGocciaObjectValue;
  const ANewTarget: TGocciaValue): TGocciaValue;

// ES2026 §7.3.14 Construct(F, argumentsList, newTarget): single dispatch
// across all callable forms — proxies, classes, native constructors,
// ordinary functions, and bound functions. Walks the §10.4.1.2 bound chain
// (merging bound arguments and applying the SameValue(F, newTarget) ⇒
// newTarget := F.[[BoundTargetFunction]] substitution) before routing the
// unwrapped target to its [[Construct]] implementation. Callers must
// pre-validate IsConstructable(ATarget); the trailing else-branch only
// fires defensively. Used by Reflect.construct (§28.1.2) and the proxy
// [[Construct]] fallback (§10.5.13).
function ConstructValue(const ATarget: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
procedure RegisterProxyDispatchHooks(
  const APredicate: TGocciaProxyPredicate;
  const AApply: TGocciaProxyApplyHook;
  const AConstruct: TGocciaProxyConstructHook;
  const AGetPrototype: TGocciaProxyGetPrototypeHook;
  const AGetFunctionRealm: TGocciaProxyGetFunctionRealmHook);
procedure RegisterFunctionConstructRedirectHook(
  const AHook: TGocciaFunctionConstructRedirectHook);

// ES2026 §10.2.9 SetFunctionName property-key formatting shared by
// interpreter and bytecode named-evaluation paths.
function FunctionNameFromPropertyKey(const AKey: TGocciaValue;
  const APrefix: string = ''): string;

implementation

uses
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Arguments.ArrayLike,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SymbolValue;

const
  MAX_FAST_APPLY_ARGUMENTS_LIST_LENGTH = 1048576;

// Function.prototype lives in a per-realm slot.  JS-visible mutations
// (Function.prototype.foo = ...) must not leak across engine recreation.
var
  GFunctionPrototypeSlot: TGocciaRealmSlotId;
  GThrowTypeErrorSlot: TGocciaRealmSlotId;
  GProxyPredicate: TGocciaProxyPredicate;
  GProxyApplyHook: TGocciaProxyApplyHook;
  GProxyConstructHook: TGocciaProxyConstructHook;
  GProxyGetPrototypeHook: TGocciaProxyGetPrototypeHook;
  GProxyGetFunctionRealmHook: TGocciaProxyGetFunctionRealmHook;
  GFunctionConstructRedirectHook: TGocciaFunctionConstructRedirectHook;

procedure RegisterProxyDispatchHooks(
  const APredicate: TGocciaProxyPredicate;
  const AApply: TGocciaProxyApplyHook;
  const AConstruct: TGocciaProxyConstructHook;
  const AGetPrototype: TGocciaProxyGetPrototypeHook;
  const AGetFunctionRealm: TGocciaProxyGetFunctionRealmHook);
begin
  GProxyPredicate := APredicate;
  GProxyApplyHook := AApply;
  GProxyConstructHook := AConstruct;
  GProxyGetPrototypeHook := AGetPrototype;
  GProxyGetFunctionRealmHook := AGetFunctionRealm;
end;

procedure RegisterFunctionConstructRedirectHook(
  const AHook: TGocciaFunctionConstructRedirectHook);
begin
  GFunctionConstructRedirectHook := AHook;
end;

function IsRegisteredProxyValue(const AValue: TGocciaValue): Boolean; inline;
begin
  Result := Assigned(GProxyPredicate) and GProxyPredicate(AValue);
end;

function GetSharedFunctionPrototype: TGocciaFunctionSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaFunctionSharedPrototype(CurrentRealm.GetSlot(GFunctionPrototypeSlot))
  else
    Result := nil;
end;

function GetProtoFromConstructor(const ANewTarget: TGocciaValue): TGocciaObjectValue;
var
  FallbackRealm: TGocciaRealm;
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
    FallbackRealm := GetFunctionRealm(ANewTarget);
    Result := TGocciaObjectValue.GetSharedObjectPrototypeForRealm(
      FallbackRealm);
    if Assigned(Result) then
      Exit;

    if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
      TGocciaObjectValue.InitializeSharedPrototype;
    Result := TGocciaObjectValue.SharedObjectPrototype;
  end;
end;

function GetProtoFromConstructorWithIntrinsic(const ANewTarget: TGocciaValue;
  const AIntrinsicDefault: TGocciaObjectValue;
  const AIntrinsicSlot: TGocciaRealmSlotId): TGocciaObjectValue;
var
  FallbackRealm: TGocciaRealm;
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
    FallbackRealm := GetFunctionRealm(ANewTarget);
    if AIntrinsicSlot >= 0 then
    begin
      if Assigned(FallbackRealm) then
      begin
        Result := TGocciaObjectValue(FallbackRealm.GetSlot(AIntrinsicSlot));
        if Assigned(Result) then
          Exit;
      end;
    end;
    Result := AIntrinsicDefault;
  end;
end;

function GetFunctionPrototypeFromConstructor(const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := GetProtoFromConstructorWithIntrinsic(ANewTarget,
    ACurrentRealmDefault, GFunctionPrototypeSlot);
end;

function GetFunctionRealm(const AValue: TGocciaValue): TGocciaRealm;
begin
  if AValue is TGocciaBoundFunctionValue then
    Exit(GetFunctionRealm(TGocciaBoundFunctionValue(AValue).OriginalFunction));

  if AValue is TGocciaFunctionBase then
    Exit(TGocciaFunctionBase(AValue).CreationRealm);

  if AValue is TGocciaClassValue then
    Exit(TGocciaClassValue(AValue).CreationRealm);

  if IsRegisteredProxyValue(AValue) and Assigned(GProxyGetFunctionRealmHook) then
    Exit(GProxyGetFunctionRealmHook(AValue));

  Result := nil;
end;

function GetThrowTypeErrorIntrinsic: TGocciaFunctionBase;
var
  Shared: TGocciaFunctionSharedPrototype;
  Thrower: TGocciaNativeFunctionValue;
begin
  if not Assigned(CurrentRealm) then
    Exit(nil);

  Result := TGocciaFunctionBase(CurrentRealm.GetSlot(GThrowTypeErrorSlot));
  if Assigned(Result) then
    Exit;

  Shared := GetSharedFunctionPrototype;
  if not Assigned(Shared) then
  begin
    Shared := TGocciaFunctionSharedPrototype.Create;
    Result := TGocciaFunctionBase(CurrentRealm.GetSlot(GThrowTypeErrorSlot));
    if Assigned(Result) then
      Exit;
  end;

  Thrower := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    Shared.RestrictedFunctionPropertyThrow, '', 0);
  Thrower.DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorData.Create(
    TGocciaNumberLiteralValue.ZeroValue, []));
  Thrower.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(''), []));
  Thrower.Freeze;
  CurrentRealm.SetSlot(GThrowTypeErrorSlot, Thrower);
  Result := Thrower;
end;

function ConstructOrdinaryWithReceiver(const ATarget: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection;
  const AReceiver: TGocciaObjectValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  ReturnValue: TGocciaValue;
begin
  TGarbageCollector.Instance.AddTempRoot(AReceiver);
  try
    ReturnValue := ATarget.ConstructWithReceiver(AArguments, AReceiver,
      ANewTarget);
    if ReturnValue is TGocciaObjectValue then
      Result := ReturnValue
    else
      Result := AReceiver;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(AReceiver);
  end;
end;

function ConstructValue(const ATarget: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  EffectiveTarget: TGocciaValue;
  EffectiveNewTarget: TGocciaValue;
  WorkingArgs, NextArgs: TGocciaArgumentsCollection;
  BoundFn: TGocciaBoundFunctionValue;
  I: Integer;
begin
  EffectiveTarget := ATarget;
  EffectiveNewTarget := ANewTarget;

  WorkingArgs := TGocciaArgumentsCollection.CreateWithCapacity(AArguments.Length);
  try
    for I := 0 to AArguments.Length - 1 do
      WorkingArgs.Add(AArguments.GetElement(I));

    while EffectiveTarget is TGocciaBoundFunctionValue do
    begin
      BoundFn := TGocciaBoundFunctionValue(EffectiveTarget);
      if EffectiveNewTarget = EffectiveTarget then
        EffectiveNewTarget := BoundFn.OriginalFunction;
      NextArgs := TGocciaArgumentsCollection.CreateWithCapacity(
        BoundFn.BoundArgCount + WorkingArgs.Length);
      try
        for I := 0 to BoundFn.BoundArgCount - 1 do
          NextArgs.Add(BoundFn.GetBoundArg(I));
        for I := 0 to WorkingArgs.Length - 1 do
          NextArgs.Add(WorkingArgs.GetElement(I));
      except
        NextArgs.Free;
        raise;
      end;
      WorkingArgs.Free;
      WorkingArgs := NextArgs;
      EffectiveTarget := BoundFn.OriginalFunction;
    end;

    if IsRegisteredProxyValue(EffectiveTarget) then
      Result := GProxyConstructHook(EffectiveTarget, WorkingArgs,
        EffectiveNewTarget)
    else if EffectiveTarget is TGocciaClassValue then
      Result := TGocciaClassValue(EffectiveTarget).Instantiate(WorkingArgs,
        EffectiveNewTarget)
    else if EffectiveTarget is TGocciaNativeFunctionValue then
      Result := TGocciaNativeFunctionValue(EffectiveTarget).Construct(WorkingArgs,
        EffectiveNewTarget)
    else if Assigned(GFunctionConstructRedirectHook) and
            GFunctionConstructRedirectHook(EffectiveTarget, WorkingArgs,
              EffectiveNewTarget, Result) then
      Exit
    else if EffectiveTarget is TGocciaFunctionBase then
      Result := ConstructOrdinaryWithReceiver(TGocciaFunctionBase(EffectiveTarget),
        WorkingArgs,
        TGocciaObjectValue.Create(GetProtoFromConstructor(EffectiveNewTarget)),
        EffectiveNewTarget)
    else
      ThrowTypeError(SErrorReflectConstructTargetMustBeConstructor,
        SSuggestNotConstructorType);
  finally
    WorkingArgs.Free;
  end;
end;

function IsCallableForHasInstance(const AValue: TGocciaValue): Boolean; inline;
begin
  Result := AValue.IsCallable;
end;

function GetPrototypeOfObject(const AObject: TGocciaObjectValue): TGocciaValue;
begin
  if IsRegisteredProxyValue(AObject) then
    Result := GProxyGetPrototypeHook(AObject)
  else if Assigned(AObject.Prototype) then
    Result := AObject.Prototype
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §7.3.21 OrdinaryHasInstance(constructor, instance)
function OrdinaryHasInstance(const AConstructor, AInstance: TGocciaValue): Boolean;
var
  ConstructorPrototype: TGocciaValue;
  CurrentObject: TGocciaObjectValue;
  CurrentPrototype: TGocciaValue;
begin
  if not IsCallableForHasInstance(AConstructor) then
    Exit(False);

  if AConstructor is TGocciaBoundFunctionValue then
    Exit(InstanceofOperatorResult(AInstance,
      TGocciaBoundFunctionValue(AConstructor).OriginalFunction));

  if not (AInstance is TGocciaObjectValue) then
    Exit(False);

  if AConstructor is TGocciaObjectValue then
    ConstructorPrototype := TGocciaObjectValue(AConstructor).GetProperty(PROP_PROTOTYPE)
  else
    ConstructorPrototype := nil;

  if not (ConstructorPrototype is TGocciaObjectValue) then
    ThrowTypeError('Function has non-object prototype',
      'set the constructor prototype property to an object');

  CurrentObject := TGocciaObjectValue(AInstance);
  while True do
  begin
    CurrentPrototype := GetPrototypeOfObject(CurrentObject);
    if (CurrentPrototype = nil) or
       (CurrentPrototype is TGocciaNullLiteralValue) then
      Exit(False);
    if CurrentPrototype = ConstructorPrototype then
      Exit(True);
    if not (CurrentPrototype is TGocciaObjectValue) then
      Exit(False);
    CurrentObject := TGocciaObjectValue(CurrentPrototype);
  end;
end;

// ES2026 §13.10.2 InstanceofOperator(value, target)
function InstanceofOperatorResult(const AInstance, ATarget: TGocciaValue): Boolean;
var
  Args: TGocciaArgumentsCollection;
  Handler: TGocciaValue;
  HandlerResult: TGocciaValue;
begin
  if not (ATarget is TGocciaObjectValue) then
    ThrowTypeError('Right-hand side of instanceof is not an object',
      'use a constructor function or an object with Symbol.hasInstance');

  Handler := TGocciaObjectValue(ATarget).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownHasInstance);
  if Assigned(Handler) and
     not (Handler is TGocciaUndefinedLiteralValue) and
     not (Handler is TGocciaNullLiteralValue) then
  begin
    if not Handler.IsCallable then
      ThrowTypeError('Symbol.hasInstance must be callable',
        'set Symbol.hasInstance to a function or remove it');

    Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
    try
      Args.Add(AInstance);
      HandlerResult := DispatchCall(Handler, Args, ATarget);
      Exit(HandlerResult.ToBooleanLiteral.Value);
    finally
      Args.Free;
    end;
  end;

  if not IsCallableForHasInstance(ATarget) then
    ThrowTypeError('Right-hand side of instanceof is not callable',
      'use a constructor function or define Symbol.hasInstance');

  Result := OrdinaryHasInstance(ATarget, AInstance);
end;

{ TGocciaFunctionBase }

function SnapshotBoundFunctionLength(const ATarget: TGocciaValue;
  const ABoundArgCount: Integer): Double;
var
  TargetObject: TGocciaObjectValue;
  LengthValue: TGocciaValue;
  LengthNumber: TGocciaNumberLiteralValue;
begin
  // ES2026 §20.2.3.2 Function.prototype.bind steps 7-8: read target.length
  // once while binding, then derive the bound function's own length.
  Result := 0;
  if not (ATarget is TGocciaObjectValue) then
    Exit;

  TargetObject := TGocciaObjectValue(ATarget);
  if not TargetObject.HasOwnProperty(PROP_LENGTH) then
    Exit;

  LengthValue := TargetObject.GetProperty(PROP_LENGTH);
  if not (LengthValue is TGocciaNumberLiteralValue) then
    Exit;

  LengthNumber := TGocciaNumberLiteralValue(LengthValue);
  if LengthNumber.IsInfinity then
  begin
    Result := Math.Infinity;
    Exit;
  end;
  if LengthNumber.IsNaN or LengthNumber.IsNegativeInfinity then
    Exit;

  Result := Int(LengthNumber.Value) - ABoundArgCount;
  if Result < 0 then
    Result := 0;
  if Result = 0 then
    Result := 0;
end;

function SnapshotBoundFunctionName(const ATarget: TGocciaValue): string;
var
  TargetObject: TGocciaObjectValue;
  NameValue: TGocciaValue;
begin
  // ES2026 §20.2.3.2 steps 9-10: read target.name once while binding.
  Result := '';
  if ATarget is TGocciaObjectValue then
  begin
    TargetObject := TGocciaObjectValue(ATarget);
    NameValue := TargetObject.GetProperty(PROP_NAME);
    if NameValue is TGocciaStringLiteralValue then
      Result := TGocciaStringLiteralValue(NameValue).Value;
  end;
  Result := 'bound ' + Result;
end;

function FunctionNameFromPropertyKey(const AKey: TGocciaValue;
  const APrefix: string = ''): string;
var
  Symbol: TGocciaSymbolValue;
begin
  if AKey is TGocciaSymbolValue then
  begin
    Symbol := TGocciaSymbolValue(AKey);
    if Symbol.HasDescription then
      Result := '[' + Symbol.Description + ']'
    else
      Result := '';
  end
  else if AKey is TGocciaStringLiteralValue then
    Result := TGocciaStringLiteralValue(AKey).Value
  else
    Result := AKey.ToStringLiteral.Value;

  if APrefix <> '' then
    Result := APrefix + ' ' + Result;
end;

constructor TGocciaFunctionBase.Create;
var
  Shared: TGocciaFunctionSharedPrototype;
begin
  inherited Create;
  FCreationRealm := CurrentRealm;
  FStrictThis := True;
  FStrictCode := True;
  FHasOwnLengthProperty := True;
  FHasOwnNameProperty := True;

  Shared := GetSharedFunctionPrototype;
  if not Assigned(Shared) and Assigned(CurrentRealm) then
  begin
    // Constructor pins via realm slot internally - see SetSlot.
    Shared := TGocciaFunctionSharedPrototype.Create;
  end;

  if Assigned(Shared) then
    Self.Prototype := Shared;
end;

procedure TGocciaFunctionBase.InstallSloppyFunctionCallerArgumentsProperties;
begin
  if not HasOwnProperty(PROP_CALLER) then
    DefineProperty(PROP_CALLER, TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, [pfConfigurable]));
  if not HasOwnProperty(PROP_ARGUMENTS) then
    DefineProperty(PROP_ARGUMENTS, TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, [pfConfigurable]));
end;

function TGocciaFunctionBase.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFunctionBase.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  // ECMAScript: Function.length and Function.name
  if AName = PROP_LENGTH then
  begin
    if FProperties.ContainsKey(AName) then
      Result := inherited GetPropertyWithContext(AName, AThisContext)
    else if FHasOwnLengthProperty then
      Result := TGocciaNumberLiteralValue.Create(GetFunctionLength)
    else
      Result := inherited GetPropertyWithContext(AName, AThisContext);
    Exit;
  end;
  if AName = PROP_NAME then
  begin
    if FProperties.ContainsKey(AName) then
      Result := inherited GetPropertyWithContext(AName, AThisContext)
    else if FHasOwnNameProperty then
      Result := TGocciaStringLiteralValue.Create(GetFunctionName)
    else
      Result := inherited GetPropertyWithContext(AName, AThisContext);
    Exit;
  end;
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaFunctionBase.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaFunctionBase.GetFunctionName: string;
begin
  Result := '';
end;

function TGocciaFunctionBase.GetSourceText: string;
begin
  Result := '';
end;

function TGocciaFunctionBase.HasOwnProperty(const AName: string): Boolean;
begin
  if (AName = PROP_LENGTH) and FHasOwnLengthProperty then
    Result := True
  else if (AName = PROP_NAME) and FHasOwnNameProperty then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

function TGocciaFunctionBase.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  Result := inherited GetOwnPropertyDescriptor(AName);
  if Assigned(Result) then
    Exit;

  if (AName = PROP_LENGTH) and FHasOwnLengthProperty then
    Result := TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(GetFunctionLength), [pfConfigurable])
  else if (AName = PROP_NAME) and FHasOwnNameProperty then
    Result := TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(GetFunctionName), [pfConfigurable])
  else
    Result := nil;
end;

function TGocciaFunctionBase.GetAllPropertyNames: TArray<string>;
var
  OwnNames: TArray<string>;
  Count, I: Integer;

  function IsArrayIndexKey(const AName: string): Boolean;
  var
    Digit, Index: UInt64;
    K: Integer;
  begin
    Index := 0;
    Result := False;
    if AName = '' then
      Exit;
    if (Length(AName) > 1) and (AName[1] = '0') then
      Exit;
    for K := 1 to Length(AName) do
    begin
      if (AName[K] < '0') or (AName[K] > '9') then
        Exit;
      Digit := Ord(AName[K]) - Ord('0');
      if Index > (High(UInt32) - Digit) div 10 then
        Exit;
      Index := Index * 10 + Digit;
    end;
    Result := Index < High(UInt32);
  end;

  procedure AppendName(const AName: string);
  var
    J: Integer;
  begin
    for J := 0 to Count - 1 do
      if Result[J] = AName then
        Exit;
    if Count >= Length(Result) then
      SetLength(Result, Count + 8);
    Result[Count] := AName;
    Inc(Count);
  end;

begin
  OwnNames := inherited GetAllPropertyNames;
  SetLength(Result, Length(OwnNames) + 2);
  Count := 0;

  for I := 0 to High(OwnNames) do
    if IsArrayIndexKey(OwnNames[I]) then
      AppendName(OwnNames[I]);

  if FHasOwnLengthProperty then
    AppendName(PROP_LENGTH);
  if FHasOwnNameProperty then
    AppendName(PROP_NAME);

  for I := 0 to High(OwnNames) do
    if not IsArrayIndexKey(OwnNames[I]) then
      AppendName(OwnNames[I]);

  SetLength(Result, Count);
end;

function TGocciaFunctionBase.GetOwnPropertyKeys: TArray<string>;
begin
  Result := GetAllPropertyNames;
end;

procedure TGocciaFunctionBase.MaterializeIntrinsicProperty(
  const AName: string);
begin
  if FProperties.ContainsKey(AName) then
    Exit;
  if (AName = PROP_LENGTH) and FHasOwnLengthProperty then
    FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(GetFunctionLength), [pfConfigurable]))
  else if (AName = PROP_NAME) and FHasOwnNameProperty then
    FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(GetFunctionName), [pfConfigurable]));
end;

function TGocciaFunctionBase.DeleteProperty(const AName: string): Boolean;
begin
  if FProperties.ContainsKey(AName) then
  begin
    Result := inherited DeleteProperty(AName);
    if Result then
    begin
      if AName = PROP_LENGTH then
        FHasOwnLengthProperty := False
      else if AName = PROP_NAME then
        FHasOwnNameProperty := False;
    end;
    Exit;
  end;
  if (AName = PROP_LENGTH) and FHasOwnLengthProperty then
  begin
    FHasOwnLengthProperty := False;
    Exit(True);
  end;
  if (AName = PROP_NAME) and FHasOwnNameProperty then
  begin
    FHasOwnNameProperty := False;
    Exit(True);
  end;
  Result := inherited DeleteProperty(AName);
end;

procedure TGocciaFunctionBase.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  MaterializeIntrinsicProperty(AName);
  inherited DefineProperty(AName, ADescriptor);
  if AName = PROP_LENGTH then
    FHasOwnLengthProperty := True
  else if AName = PROP_NAME then
    FHasOwnNameProperty := True;
end;

function TGocciaFunctionBase.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  MaterializeIntrinsicProperty(AName);
  Result := inherited TryDefineProperty(AName, ADescriptor);
  if Result then
  begin
    if AName = PROP_LENGTH then
      FHasOwnLengthProperty := True
    else if AName = PROP_NAME then
      FHasOwnNameProperty := True;
  end;
end;

function TGocciaFunctionBase.IsCallable: Boolean;
begin
  Result := True;
end;

function TGocciaFunctionBase.IsConstructable: Boolean;
begin
  if (Self is TGocciaBoundFunctionValue) and
     Assigned(TGocciaBoundFunctionValue(Self).OriginalFunction) then
    Exit(TGocciaBoundFunctionValue(Self).OriginalFunction.IsConstructable);

  Result := HasOwnProperty(PROP_PROTOTYPE);
end;

function TGocciaFunctionBase.TypeName: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaFunctionBase.TypeOf: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaFunctionBase.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFunctionBase.ConstructWithReceiver(
  const AArguments: TGocciaArgumentsCollection; const AReceiver: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := Call(AArguments, AReceiver);
end;

function TGocciaFunctionBase.CallPreparedArgs(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Call(AArguments, AThisValue);
end;

function TGocciaFunctionBase.CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
  try
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    Args.Add(AArg0);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
  try
    Args.Add(AArg0);
    Args.Add(AArg1);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallThreeArgs(const AArg0, AArg1, AArg2,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
  try
    Args.Add(AArg0);
    Args.Add(AArg1);
    Args.Add(AArg2);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

class procedure TGocciaFunctionBase.SetSharedPrototypeParent(
  const AParent: TGocciaObjectValue);
var
  Shared: TGocciaFunctionSharedPrototype;
begin
  Shared := GetSharedFunctionPrototype;
  if not Assigned(Shared) and Assigned(CurrentRealm) then
    Shared := TGocciaFunctionSharedPrototype.Create;
  if Assigned(Shared) then
    Shared.Prototype := AParent;
end;

class function TGocciaFunctionBase.GetSharedPrototype: TGocciaFunctionSharedPrototype;
begin
  Result := GetSharedFunctionPrototype;
end;

constructor TGocciaFunctionSharedPrototype.Create;
var
  Members: array[0..4] of TGocciaMemberDefinition;
  Thrower: TGocciaNativeFunctionValue;
begin
  inherited Create;

  // Install into the realm slot before registering members - creating native
  // function values for the prototype methods will recursively call
  // TGocciaFunctionBase.Create, which expects to find the prototype already
  // installed.  On exception we reset the slot to nil so a later Create can
  // try again cleanly.
  if Assigned(CurrentRealm) then
    CurrentRealm.SetSlot(GFunctionPrototypeSlot, Self);
  try
    DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.ZeroValue, [pfConfigurable]));
    DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(''), [pfConfigurable]));
    Thrower := TGocciaNativeFunctionValue(GetThrowTypeErrorIntrinsic);
    DefineProperty(PROP_CALLER, TGocciaPropertyDescriptorAccessor.Create(
      Thrower, Thrower, [pfConfigurable]));
    DefineProperty(PROP_ARGUMENTS, TGocciaPropertyDescriptorAccessor.Create(
      Thrower, Thrower, [pfConfigurable]));
    Members[0] := DefineNamedMethod('call', FunctionCall, 1);
    Members[1] := DefineNamedMethod('apply', FunctionApply, 2);
    Members[2] := DefineNamedMethod('bind', FunctionBind, 1);
    Members[3] := DefineNamedMethod(PROP_TO_STRING, FunctionToString, 0);
    Members[4] := DefineSymbolMethod(
      TGocciaSymbolValue.WellKnownHasInstance,
      '[Symbol.hasInstance]',
      FunctionHasInstance,
      1,
      []);
    RegisterMemberDefinitions(Self, Members);
  except
    if Assigned(CurrentRealm) and (CurrentRealm.GetSlot(GFunctionPrototypeSlot) = Self) then
      CurrentRealm.SetSlot(GFunctionPrototypeSlot, nil);
    raise;
  end;
end;

function TGocciaFunctionSharedPrototype.TypeOf: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaFunctionSharedPrototype.IsCallable: Boolean;
begin
  Result := True;
end;

// ES2026 §20.2.2.1 Function.prototype
function TGocciaFunctionSharedPrototype.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFunctionSharedPrototype.RestrictedFunctionPropertyThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('caller and arguments are restricted function properties');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// Dispatch a call to a function object, class constructor, or callable Proxy.
function DispatchCall(const ACallee: TGocciaValue;
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if ACallee is TGocciaFunctionBase then
    Result := TGocciaFunctionBase(ACallee).Call(AArgs, AThisValue)
  else if ACallee is TGocciaFunctionSharedPrototype then
    Result := TGocciaFunctionSharedPrototype(ACallee).Call(AArgs, AThisValue)
  else if ACallee is TGocciaClassValue then
    Result := TGocciaClassValue(ACallee).Call(AArgs, AThisValue)
  else if IsRegisteredProxyValue(ACallee) and ACallee.IsCallable then
    Result := GProxyApplyHook(ACallee, AArgs, AThisValue)
  else
    raise TGocciaError.Create('not callable', 0, 0, '', nil);
end;

function TryCallStringFromCodePointApplyFast(const ACallee, AArgArray: TGocciaValue;
  out AResult: TGocciaValue): Boolean;
var
  ArrVal: TGocciaArrayValue;
  CodePoint: Cardinal;
  Element: TGocciaValue;
  I: Integer;
  NativeFunc: TGocciaNativeFunctionValue;
  NumberValue: TGocciaNumberLiteralValue;
  RawValue: Double;
  SB: TStringBuffer;
begin
  Result := False;
  if not (ACallee is TGocciaNativeFunctionValue) or
     not (AArgArray is TGocciaArrayValue) then
    Exit;

  NativeFunc := TGocciaNativeFunctionValue(ACallee);
  if (NativeFunc.Name <> 'fromCodePoint') or (NativeFunc.Arity <> 1) then
    Exit;

  ArrVal := TGocciaArrayValue(AArgArray);
  if (ArrVal.Elements.Count > MAX_FAST_APPLY_ARGUMENTS_LIST_LENGTH) or
     not ArrVal.HasDenseElementLength then
    Exit;

  SB := TStringBuffer.Create(ArrVal.Elements.Count * 4);
  for I := 0 to ArrVal.Elements.Count - 1 do
  begin
    Element := ArrVal.Elements[I];
    if (Element = TGocciaHoleValue.HoleValue) or
       not (Element is TGocciaNumberLiteralValue) then
      Exit;

    NumberValue := TGocciaNumberLiteralValue(Element);
    RawValue := NumberValue.Value;
    if IsNan(RawValue) or IsInfinite(RawValue) then
      ThrowRangeError(SErrorInvalidCodePoint, SSuggestCodePointRange);
    if (RawValue < 0) or (RawValue > $10FFFF) then
      ThrowRangeError(Format(SErrorNotValidCodePoint, [FormatDouble(RawValue)]),
        SSuggestCodePointRange);
    CodePoint := Trunc(RawValue);
    if RawValue <> CodePoint then
      ThrowRangeError(Format(SErrorNotValidCodePoint, [FormatDouble(RawValue)]),
        SSuggestCodePointRange);

    if CodePoint < $80 then
      SB.AppendChar(AnsiChar(CodePoint))
    else if CodePoint < $800 then
    begin
      SB.AppendChar(AnsiChar($C0 or (CodePoint shr 6)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end
    else if CodePoint < $10000 then
    begin
      SB.AppendChar(AnsiChar($E0 or (CodePoint shr 12)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 6) and $3F)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end
    else
    begin
      SB.AppendChar(AnsiChar($F0 or (CodePoint shr 18)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 12) and $3F)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 6) and $3F)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end;
  end;

  AResult := TGocciaStringLiteralValue.Create(SB.ToString);
  Result := True;
end;

function TGocciaFunctionSharedPrototype.FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewThisValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Function.prototype.call(thisArg, ...args)
  // AThisValue is the function being called

  if not AThisValue.IsCallable then
    ThrowTypeError('Function.prototype.call called on non-function',
      SSuggestNotFunctionType);

  // First argument is the 'this' value for the call
  if AArgs.Length > 0 then
    NewThisValue := AArgs.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Fast path for TGocciaFunctionBase (most common case)
  if AThisValue is TGocciaFunctionBase then
  begin
    case AArgs.Length of
      0, 1:
        Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));
      2:
        Exit(TGocciaFunctionBase(AThisValue).CallOneArg(AArgs.GetElement(1), NewThisValue));
      3:
        Exit(TGocciaFunctionBase(AThisValue).CallTwoArgs(AArgs.GetElement(1),
          AArgs.GetElement(2), NewThisValue));
      4:
        Exit(TGocciaFunctionBase(AThisValue).CallThreeArgs(AArgs.GetElement(1),
          AArgs.GetElement(2), AArgs.GetElement(3), NewThisValue));
    end;
  end;

  CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(Max(0, AArgs.Length - 1));
  try
    for I := 1 to AArgs.Length - 1 do
      CallArgs.Add(AArgs.GetElement(I));

    Result := DispatchCall(AThisValue, CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

// ES2026 §20.2.3.1 Function.prototype.apply(thisArg, argArray)
function TGocciaFunctionSharedPrototype.FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  NewThisValue: TGocciaValue;
  ArgArray: TGocciaValue;
  ArrVal: TGocciaArrayValue;
begin
  // Step 1: Perform ? RequireObjectCoercible(this)
  if not AThisValue.IsCallable then
    ThrowTypeError(SErrorFunctionApplyNonFunction, SSuggestFunctionApply);

  // thisArg
  if AArgs.Length > 0 then
    NewThisValue := AArgs.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 2: If argArray is undefined or null, return F.[[Call]](thisArg, <<>>)
  if AArgs.Length <= 1 then
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      Exit(DispatchCall(AThisValue, CallArgs, NewThisValue));
    finally
      CallArgs.Free;
    end;
  end;

  ArgArray := AArgs.GetElement(1);
  if (ArgArray is TGocciaUndefinedLiteralValue) or
     (ArgArray is TGocciaNullLiteralValue) then
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      Exit(DispatchCall(AThisValue, CallArgs, NewThisValue));
    finally
      CallArgs.Free;
    end;
  end;

  if TryCallStringFromCodePointApplyFast(AThisValue, ArgArray, Result) then
    Exit;

  // Fast path: small arrays use specialized call methods (FunctionBase only)
  if (AThisValue is TGocciaFunctionBase) and (ArgArray is TGocciaArrayValue) then
  begin
    ArrVal := TGocciaArrayValue(ArgArray);
    case ArrVal.Elements.Count of
      0:
        Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));
      1:
        Exit(TGocciaFunctionBase(AThisValue).CallOneArg(ArrVal.Elements[0], NewThisValue));
      2:
        Exit(TGocciaFunctionBase(AThisValue).CallTwoArgs(ArrVal.Elements[0],
          ArrVal.Elements[1], NewThisValue));
      3:
        Exit(TGocciaFunctionBase(AThisValue).CallThreeArgs(ArrVal.Elements[0],
          ArrVal.Elements[1], ArrVal.Elements[2], NewThisValue));
    end;
  end;

  // Step 3: Let argList be ? CreateListFromArrayLike(argArray)
  CallArgs := CreateListFromArrayLike(ArgArray, 'Function.prototype.apply');
  try
    // Step 4: Return ? Call(func, thisArg, argList)
    Result := DispatchCall(AThisValue, CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionBind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BoundThis: TGocciaValue;
  BoundArgs: TGocciaValueList;
  I: Integer;
begin
  // Function.prototype.bind(thisArg, ...args)
  // AThisValue is the function being bound

  if not AThisValue.IsCallable then
    ThrowTypeError('Function.prototype.bind called on non-function');

  // First argument is the 'this' value to bind
  if AArgs.Length > 0 then
    BoundThis := AArgs.GetElement(0)
  else
    BoundThis := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Remaining arguments are pre-filled arguments
  case AArgs.Length of
    0, 1:
      Exit(TGocciaBoundFunctionValue.CreateWithoutArgs(AThisValue, BoundThis));
    2:
      Exit(TGocciaBoundFunctionValue.CreateWithSingleArg(AThisValue,
        BoundThis, AArgs.GetElement(1)));
  end;

  BoundArgs := TGocciaValueList.Create(False);
  try
    BoundArgs.Capacity := AArgs.Length - 1;
    for I := 1 to AArgs.Length - 1 do
      BoundArgs.Add(AArgs.GetElement(I));

    Result := TGocciaBoundFunctionValue.Create(AThisValue, BoundThis, BoundArgs);
    BoundArgs := nil;
  finally
    if Assigned(BoundArgs) then
      BoundArgs.Free;
  end;
end;

// ES2026 §20.2.3.5 Function.prototype.toString()
function TGocciaFunctionSharedPrototype.FunctionToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  FuncName, SrcText: string;
begin
  if not AThisValue.IsCallable then
    ThrowTypeError('Function.prototype.toString requires that ''this'' be a Function');

  // User-defined functions: return preserved source text
  if AThisValue is TGocciaFunctionBase then
  begin
    SrcText := TGocciaFunctionBase(AThisValue).GetSourceText;
    if SrcText <> '' then
    begin
      Result := TGocciaStringLiteralValue.Create(SrcText);
      Exit;
    end;
  end;
  if AThisValue is TGocciaClassValue then
  begin
    SrcText := TGocciaClassValue(AThisValue).GetSourceText;
    if SrcText <> '' then
    begin
      Result := TGocciaStringLiteralValue.Create(SrcText);
      Exit;
    end;
  end;

  // Built-in functions and bound functions: NativeFunction string
  if AThisValue is TGocciaBoundFunctionValue then
    FuncName := ''
  else if AThisValue is TGocciaFunctionBase then
    FuncName := TGocciaFunctionBase(AThisValue).GetFunctionName
  else if AThisValue is TGocciaClassValue then
    FuncName := TGocciaClassValue(AThisValue).Name
  else
    FuncName := '';

  Result := TGocciaStringLiteralValue.Create('function ' + FuncName + '() { [native code] }');
end;

// ES2026 §20.2.3.6 Function.prototype [ @@hasInstance ] ( value )
function TGocciaFunctionSharedPrototype.FunctionHasInstance(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if OrdinaryHasInstance(AThisValue, Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ TGocciaBoundFunctionValue }

constructor TGocciaBoundFunctionValue.CreateWithoutArgs(
  const AOriginalFunction, ABoundThis: TGocciaValue);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := nil;
  FBoundArgCount := 0;
  InitializeBoundFunctionProperties;
end;

constructor TGocciaBoundFunctionValue.CreateWithSingleArg(
  const AOriginalFunction, ABoundThis, ABoundArg: TGocciaValue);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := ABoundArg;
  FBoundArgCount := 1;
  InitializeBoundFunctionProperties;
end;

constructor TGocciaBoundFunctionValue.Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := nil;
  FBoundArgCount := ABoundArgs.Count;
  case ABoundArgs.Count of
    0:
      begin
        InitializeBoundFunctionProperties;
      end;
    1:
      begin
        FSingleBoundArg := ABoundArgs[0];
        InitializeBoundFunctionProperties;
      end;
  else
    FBoundArgs := ABoundArgs;
    InitializeBoundFunctionProperties;
    Exit;
  end;
  ABoundArgs.Free;
end;

procedure TGocciaBoundFunctionValue.InitializeBoundFunctionProperties;
begin
  FBoundLength := SnapshotBoundFunctionLength(FOriginalFunction,
    FBoundArgCount);
  FBoundName := SnapshotBoundFunctionName(FOriginalFunction);
  if FOriginalFunction is TGocciaObjectValue then
    Self.Prototype := TGocciaObjectValue(FOriginalFunction).Prototype;

  FProperties.Add(PROP_LENGTH, TGocciaPropertyDescriptorData.Create(
    TGocciaNumberLiteralValue.Create(FBoundLength), [pfConfigurable]));
  FProperties.Add(PROP_NAME, TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(FBoundName), [pfConfigurable]));
end;

destructor TGocciaBoundFunctionValue.Destroy;
begin
  FBoundArgs.Free;
  inherited;
end;

function TGocciaBoundFunctionValue.GetBoundArg(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= FBoundArgCount) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if FBoundArgCount = 1 then
    Exit(FSingleBoundArg);

  if Assigned(FBoundArgs) then
    Exit(FBoundArgs[AIndex]);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaBoundFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CombinedArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Combine bound arguments with call arguments
  CombinedArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    FBoundArgCount + AArguments.Length);
  try
    // Add bound arguments first
    if FBoundArgCount = 1 then
      CombinedArgs.Add(FSingleBoundArg)
    else if Assigned(FBoundArgs) then
      for I := 0 to FBoundArgs.Count - 1 do
        CombinedArgs.Add(FBoundArgs[I]);

    // Add call arguments
    for I := 0 to AArguments.Length - 1 do
      CombinedArgs.Add(AArguments.GetElement(I));

    if Assigned(FOriginalFunction) and FOriginalFunction.IsCallable then
      Result := DispatchCall(FOriginalFunction, CombinedArgs, FBoundThis)
    else
      raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);
  finally
    CombinedArgs.Free;
  end;
end;

function TGocciaBoundFunctionValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  if not FOriginalFunction.IsCallable then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  // Fast path for TGocciaFunctionBase targets only
  if not (FOriginalFunction is TGocciaFunctionBase) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Exit(Call(Args, AThisValue));
    finally
      Args.Free;
    end;
  end;

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallNoArgs(FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallOneArg(FSingleBoundArg, FBoundThis);
  else
    Result := inherited CallNoArgs(AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  if not FOriginalFunction.IsCallable then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  // Route non-TGocciaFunctionBase through generic Call path
  if not (FOriginalFunction is TGocciaFunctionBase) then
  begin
    Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
    try
      Args.Add(AArg0);
      Exit(Call(Args, AThisValue));
    finally
      Args.Free;
    end;
  end;

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallOneArg(AArg0, FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallTwoArgs(FSingleBoundArg, AArg0, FBoundThis);
  else
    Result := inherited CallOneArg(AArg0, AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  if not FOriginalFunction.IsCallable then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  // Route non-TGocciaFunctionBase through generic Call path
  if not (FOriginalFunction is TGocciaFunctionBase) then
  begin
    Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
    try
      Args.Add(AArg0);
      Args.Add(AArg1);
      Exit(Call(Args, AThisValue));
    finally
      Args.Free;
    end;
  end;

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallTwoArgs(AArg0, AArg1, FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallThreeArgs(FSingleBoundArg, AArg0, AArg1, FBoundThis);
  else
    Result := inherited CallTwoArgs(AArg0, AArg1, AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.GetFunctionLength: Integer;
begin
  if (FBoundLength <= 0) or IsNan(FBoundLength) then
    Exit(0);
  if FBoundLength >= MaxInt then
    Exit(MaxInt);
  Result := Trunc(FBoundLength);
end;

function TGocciaBoundFunctionValue.GetFunctionName: string;
begin
  Result := FBoundName;
end;

procedure TGocciaBoundFunctionValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark the original function and bound this
  if Assigned(FOriginalFunction) then
    FOriginalFunction.MarkReferences;
  if Assigned(FBoundThis) then
    FBoundThis.MarkReferences;

  // Mark bound arguments
  if FBoundArgCount = 1 then
  begin
    if Assigned(FSingleBoundArg) then
      FSingleBoundArg.MarkReferences;
  end
  else if Assigned(FBoundArgs) then
    for I := 0 to FBoundArgs.Count - 1 do
      if Assigned(FBoundArgs[I]) then
        FBoundArgs[I].MarkReferences;
end;

initialization
  GFunctionPrototypeSlot := RegisterRealmSlot('Function.prototype');
  GThrowTypeErrorSlot := RegisterRealmSlot('%ThrowTypeError%');

end.
