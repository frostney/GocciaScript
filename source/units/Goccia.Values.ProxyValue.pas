unit Goccia.Values.ProxyValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  // Sealed so a prototype walk can recognize a Proxy by exact class.
  TGocciaProxyValue = class sealed(TGocciaObjectValue)
  private
    FTarget: TGocciaValue;
    FHandler: TGocciaObjectValue;
    FRevoked: Boolean;

    procedure CheckRevoked;
    procedure CollectOwnPropertyTrapKeys(out AStringKeys: TArray<string>;
      out ASymbolKeys: TArray<TGocciaSymbolValue>;
      out AOrderedKeys: TArray<TGocciaValue>);
    function GetTrap(const ATrapName: string): TGocciaValue;
    function InvokeTrap(const ATrap: TGocciaValue;
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;

    // The target's internal methods, for a caller that knows FTarget is an
    // object. When the target is itself a Proxy the call is counted against
    // MAX_PROPERTY_DELEGATION_DEPTH (Goccia.StackLimit): Proxies nested as
    // targets would otherwise recurse until the native stack ends.
    function TargetGetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor;
    function TargetGetOwnSymbolPropertyDescriptor(
      const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
    function TargetGetOwnPropertyDescriptorForKey(
      const AKey: TGocciaValue): TGocciaPropertyDescriptor;
    // [[OwnPropertyKeys]]: string and symbol keys in one ordered list.
    function TargetOwnPropertyKeyValues: TArray<TGocciaValue>;
    function TargetDeleteProperty(const AName: string): Boolean;
    procedure TargetDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor);
    procedure TargetDefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor);
    function TargetTryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean;
    function TargetTryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean;
    function TargetTryPreventExtensions: Boolean;

    // Roots everything a [[DefineOwnProperty]] dispatch reads on both sides of
    // guest code. The caller hands the descriptor over as a plain class the
    // collector does not trace, and three separate guest-code safe points sit
    // inside the dispatch: GetTrap reads the trap off the handler (an accessor
    // or a nested proxy's get trap), InvokeTrap runs the trap itself, and the
    // validation afterwards re-reads the descriptor across
    // ProxyTargetIsExtensible and a target [[GetOwnProperty]], either of which
    // is another trap when the target is itself a proxy. The descriptor's
    // value is therefore live across guest code both before it is read into
    // the trap descriptor object and after the trap returns.
    //
    // The gate-side rooting the ordinary store path relies on does not reach
    // here at all: a proxy target never touches a property map, so nothing
    // downstream ever opens a window over this descriptor.
    procedure PushDefineTrapRoots(var AFrame: TGocciaActiveRootFrame;
      const ADescriptor: TGocciaPropertyDescriptor);
  public
    constructor Create(const ATarget: TGocciaValue;
      const AHandler: TGocciaObjectValue);

    // ES2026 §28.1.1 [[Get]](P, Receiver)
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;

    // ES2026 §28.1.1 [[Set]](P, V, Receiver)
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;

    // ES2026 §28.1.1 [[HasProperty]](P)
    function HasProperty(const AName: string): Boolean; override;
    function HasTrap(const AName: string): Boolean;
    function HasSymbolTrap(const ASymbol: TGocciaSymbolValue): Boolean;

    // ES2026 §28.1.1 [[Delete]](P)
    function DeleteProperty(const AName: string): Boolean; override;

    // ES2026 §28.1.1 [[GetOwnProperty]](P)
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    function GetOwnSymbolPropertyDescriptor(
      const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; override;

    // ES2026 §28.1.1 [[DefineOwnProperty]](P, Desc)
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;

    // ES2026 §28.1.1 [[OwnPropertyKeys]]()
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetOwnPropertyKeyValues: TArray<TGocciaValue>;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>; override;
    function HasOwnProperty(const AName: string): Boolean; override;

    // ES2026 §10.5.9 [[Set]](P, V, Receiver) — receiver-aware
    function AssignPropertyWithReceiver(const AName: string;
      const AValue: TGocciaValue;
      const AReceiver: TGocciaValue): Boolean; override;

    // Symbol-keyed property access via get/has traps
    function GetSymbolProperty(
      const ASymbol: TGocciaSymbolValue): TGocciaValue; override;
    function GetSymbolPropertyWithReceiver(
      const ASymbol: TGocciaSymbolValue;
      const AReceiver: TGocciaValue): TGocciaValue; override;
    function HasSymbolProperty(
      const ASymbol: TGocciaSymbolValue): Boolean; override;
    function AssignSymbolPropertyWithReceiver(
      const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue;
      const AReceiver: TGocciaValue): Boolean; override;

    // ES2026 §28.1.1 [[GetPrototypeOf]]()
    function GetPrototypeTrap: TGocciaValue;

    // ES2026 §28.1.1 [[SetPrototypeOf]](V)
    function SetPrototypeTrap(const AProto: TGocciaValue): Boolean;

    // ES2026 §28.1.1 [[IsExtensible]]()
    function IsExtensibleTrap: Boolean;

    // ES2026 §10.5.4 [[PreventExtensions]]()
    function TryPreventExtensions: Boolean; override;
    procedure PreventExtensions; override;

    // ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
    function ApplyTrap(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // ES2026 §28.1.1 [[Construct]](argumentsList, newTarget). ANewTarget is
    // forwarded to the construct trap and to a nested proxy fallback. nil
    // means "use this proxy as newTarget" — the default for `new proxy(...)`.
    function ConstructTrap(
      const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue;

    function TypeOf: string; override;
    function IsCallable: Boolean; override;
    function IsConstructable: Boolean; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    procedure Revoke;

    property Target: TGocciaValue read FTarget;
    property Handler: TGocciaObjectValue read FHandler;
    property Revoked: Boolean read FRevoked;
  end;

  { Helper class for Proxy.revocable — captures the proxy reference
    so the revoke callback can set FRevoked without closures. }
  TGocciaProxyRevoker = class(TGocciaObjectValue)
  private
    FProxy: TGocciaProxyValue;
  public
    constructor Create(const AProxy: TGocciaProxyValue);
    function RevokeCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  { Result object for Proxy.revocable — holds the Revoker via an
    internal field (not a JS-visible property) so the GC can trace it
    without exposing implementation state through reflection APIs. }
  TGocciaRevocableProxyResult = class(TGocciaObjectValue)
  private
    FRevoker: TGocciaProxyRevoker;
  public
    constructor Create(const ARevoker: TGocciaProxyRevoker);
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.StackLimit,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ToObject;

{ TGocciaProxyValue }

constructor TGocciaProxyValue.Create(const ATarget: TGocciaValue;
  const AHandler: TGocciaObjectValue);
begin
  inherited Create;
  FTarget := ATarget;
  FHandler := AHandler;
  FRevoked := False;
end;

procedure TGocciaProxyValue.CheckRevoked;
begin
  if FRevoked then
    ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
end;

function TGocciaProxyValue.GetTrap(const ATrapName: string): TGocciaValue;
var
  TrapValue: TGocciaValue;
begin
  // A handler that is itself a Proxy answers through its get trap or target,
  // a native call that a chain of such handlers would repeat; count it like a
  // forward to a Proxy target.
  if FHandler.ClassType = TGocciaProxyValue then
    TrapValue := DelegateGetProperty(FHandler, ATrapName, FHandler)
  else
    TrapValue := FHandler.GetProperty(ATrapName);
  if (TrapValue is TGocciaUndefinedLiteralValue) or
     (TrapValue is TGocciaNullLiteralValue) then
    Result := nil
  else if not TrapValue.IsCallable then
    ThrowTypeError(Format(SErrorProxyTrapNotFunction, [ATrapName]), SSuggestProxyTargetType)
  else
    Result := TrapValue;
end;

function TGocciaProxyValue.InvokeTrap(const ATrap: TGocciaValue;
  const AArgs: TGocciaArgumentsCollection): TGocciaValue;
begin
  // A trap can reach another Proxy without a JavaScript frame in between (a
  // native function such as Reflect.ownKeys used as the trap), so the call is
  // counted like a forward to a Proxy target; see MAX_PROPERTY_DELEGATION_DEPTH.
  EnterPropertyDelegation;
  try
    // Mirror the VM dispatch order: proxy-wrapped traps first, then
    // functions, then classes.
    if ATrap is TGocciaProxyValue then
      Result := TGocciaProxyValue(ATrap).ApplyTrap(AArgs, FHandler)
    else if ATrap is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(ATrap).Call(AArgs, FHandler)
    else if ATrap is TGocciaClassValue then
      Result := TGocciaClassValue(ATrap).Call(AArgs, FHandler)
    else
      ThrowTypeError(SErrorProxyTrapNotCallable, SSuggestProxyTargetType);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetGetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName));
  EnterPropertyDelegation;
  try
    Result := TGocciaProxyValue(FTarget).GetOwnPropertyDescriptor(AName);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetGetOwnSymbolPropertyDescriptor(
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol));
  EnterPropertyDelegation;
  try
    Result := TGocciaProxyValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetGetOwnPropertyDescriptorForKey(
  const AKey: TGocciaValue): TGocciaPropertyDescriptor;
begin
  if AKey is TGocciaSymbolValue then
    Result := TargetGetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey))
  else
    Result := TargetGetOwnPropertyDescriptor(
      TGocciaStringLiteralValue(AKey).Value);
end;

function TGocciaProxyValue.TargetOwnPropertyKeyValues: TArray<TGocciaValue>;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).OwnPropertyKeyValues);
  EnterPropertyDelegation;
  try
    Result := TGocciaProxyValue(FTarget).GetOwnPropertyKeyValues;
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetDeleteProperty(const AName: string): Boolean;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).DeleteProperty(AName));
  EnterPropertyDelegation;
  try
    Result := TGocciaProxyValue(FTarget).DeleteProperty(AName);
  finally
    LeavePropertyDelegation;
  end;
end;

procedure TGocciaProxyValue.TargetDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if FTarget.ClassType <> TGocciaProxyValue then
  begin
    TGocciaObjectValue(FTarget).DefineProperty(AName, ADescriptor);
    Exit;
  end;
  EnterPropertyDelegation;
  try
    TGocciaProxyValue(FTarget).DefineProperty(AName, ADescriptor);
  finally
    LeavePropertyDelegation;
  end;
end;

procedure TGocciaProxyValue.TargetDefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if FTarget.ClassType <> TGocciaProxyValue then
  begin
    TGocciaObjectValue(FTarget).DefineSymbolProperty(ASymbol, ADescriptor);
    Exit;
  end;
  EnterPropertyDelegation;
  try
    TGocciaProxyValue(FTarget).DefineSymbolProperty(ASymbol, ADescriptor);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetTryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).TryDefineProperty(AName, ADescriptor));
  // The call takes ownership of ADescriptor, so it is freed here when the
  // bound stops the call before it starts.
  try
    EnterPropertyDelegation;
  except
    ADescriptor.Free;
    raise;
  end;
  try
    Result := TGocciaProxyValue(FTarget).TryDefineProperty(AName, ADescriptor);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetTryDefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).TryDefineSymbolProperty(ASymbol,
      ADescriptor));
  // See TargetTryDefineProperty.
  try
    EnterPropertyDelegation;
  except
    ADescriptor.Free;
    raise;
  end;
  try
    Result := TGocciaProxyValue(FTarget).TryDefineSymbolProperty(ASymbol,
      ADescriptor);
  finally
    LeavePropertyDelegation;
  end;
end;

function TGocciaProxyValue.TargetTryPreventExtensions: Boolean;
begin
  if FTarget.ClassType <> TGocciaProxyValue then
    Exit(TGocciaObjectValue(FTarget).TryPreventExtensions);
  EnterPropertyDelegation;
  try
    Result := TGocciaProxyValue(FTarget).TryPreventExtensions;
  finally
    LeavePropertyDelegation;
  end;
end;

procedure TGocciaProxyValue.PushDefineTrapRoots(
  var AFrame: TGocciaActiveRootFrame;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  // Self and the handler as well as the descriptor: a proxy reached through a
  // native local alone — the properties object of an Object.defineProperties
  // batch, a target one proxy deep — is not otherwise rooted while its own
  // trap runs, and the handler is reachable only through Self.
  AFrame.Add(Self);
  AFrame.Add(FHandler);
  AFrame.Add(FTarget);
  if Assigned(ADescriptor) then
  begin
    { A lazy descriptor is materialized here rather than rooted as it stands.
      PushRoots deliberately pushes the unmaterialized placeholder — the right
      call on the store path, where the map keeps the lazy descriptor and
      materializes it on first read — but a proxy has no map to keep it in:
      the descriptor is read into the trap's descriptor object immediately
      below and freed once the trap returns, so leaving it lazy would hand the
      trap `value: undefined` and drop the factory's value entirely. Forcing
      the factory before the push is what makes that value both correct and
      rooted, and it happens after Self/handler/target are on the frame so the
      factory's own allocations run inside a protected window. }
    if ADescriptor is TGocciaLazyPropertyDescriptorData then
      TGocciaLazyPropertyDescriptorData(ADescriptor).Materialize;
    ADescriptor.PushRoots(AFrame);
  end;
end;

function CompleteProxyTrapPropertyDescriptor(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
var
  Flags: TPropertyFlags;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
begin
  Flags := [];
  if ADescriptor.HasEnumerableField and ADescriptor.Enumerable then
    Include(Flags, pfEnumerable);
  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Include(Flags, pfConfigurable);

  if IsAccessorDescriptor(ADescriptor) then
  begin
    Getter := nil;
    Setter := nil;
    if ADescriptor.HasGet then
      Getter := TGocciaPropertyDescriptorAccessor(ADescriptor).Getter;
    if ADescriptor.HasSet then
      Setter := TGocciaPropertyDescriptorAccessor(ADescriptor).Setter;
    Exit(TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
  end;

  Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasValue then
    Value := TGocciaPropertyDescriptorData(ADescriptor).Value;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasWritableField and
     ADescriptor.Writable then
    Include(Flags, pfWritable);

  Result := TGocciaPropertyDescriptorData.Create(Value, Flags);
end;

function IsCompatibleProxyTrapPropertyDescriptor(
  const AExtensible: Boolean; const AResultDesc: TGocciaPropertyDescriptor;
  const ATargetDesc: TGocciaPropertyDescriptor): Boolean;
var
  ResultData: TGocciaPropertyDescriptorData;
  ResultAccessor: TGocciaPropertyDescriptorAccessor;
  TargetData: TGocciaPropertyDescriptorData;
  TargetAccessor: TGocciaPropertyDescriptorAccessor;
begin
  if not Assigned(ATargetDesc) then
    Exit(AExtensible);

  if ATargetDesc.Configurable then
    Exit(True);

  if AResultDesc.Configurable then
    Exit(False);
  if ATargetDesc.Enumerable <> AResultDesc.Enumerable then
    Exit(False);
  if IsDataDescriptor(ATargetDesc) <> IsDataDescriptor(AResultDesc) then
    Exit(False);

  if IsAccessorDescriptor(ATargetDesc) then
  begin
    if not IsAccessorDescriptor(AResultDesc) then
      Exit(False);
    TargetAccessor := TGocciaPropertyDescriptorAccessor(ATargetDesc);
    ResultAccessor := TGocciaPropertyDescriptorAccessor(AResultDesc);
    Exit((TargetAccessor.Getter = ResultAccessor.Getter) and
      (TargetAccessor.Setter = ResultAccessor.Setter));
  end;

  if IsDataDescriptor(ATargetDesc) then
  begin
    if not IsDataDescriptor(AResultDesc) then
      Exit(False);
    TargetData := TGocciaPropertyDescriptorData(ATargetDesc);
    ResultData := TGocciaPropertyDescriptorData(AResultDesc);
    if not TargetData.Writable then
    begin
      if ResultData.Writable then
        Exit(False);
      if not IsSameValue(TargetData.Value, ResultData.Value) then
        Exit(False);
    end;
  end;

  Result := True;
end;

function IsCompatibleProxyDefineDescriptor(const AExtensible: Boolean;
  const ADescriptor: TGocciaPropertyDescriptor;
  const ATargetDesc: TGocciaPropertyDescriptor): Boolean;
var
  DescriptorAccessor: TGocciaPropertyDescriptorAccessor;
  DescriptorData: TGocciaPropertyDescriptorData;
  TargetAccessor: TGocciaPropertyDescriptorAccessor;
  TargetData: TGocciaPropertyDescriptorData;
begin
  if not Assigned(ATargetDesc) then
    Exit(AExtensible);

  if ADescriptor.Fields = [] then
    Exit(True);

  if ATargetDesc.Configurable then
    Exit(True);

  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Exit(False);

  if ADescriptor.HasEnumerableField and
     (ATargetDesc.Enumerable <> ADescriptor.Enumerable) then
    Exit(False);

  if IsGenericDescriptor(ADescriptor) then
    Exit(True);

  if IsDataDescriptor(ATargetDesc) <> IsDataDescriptor(ADescriptor) then
    Exit(False);

  if IsAccessorDescriptor(ATargetDesc) and IsAccessorDescriptor(ADescriptor) then
  begin
    TargetAccessor := TGocciaPropertyDescriptorAccessor(ATargetDesc);
    DescriptorAccessor := TGocciaPropertyDescriptorAccessor(ADescriptor);
    if ADescriptor.HasGet and
       (TargetAccessor.Getter <> DescriptorAccessor.Getter) then
      Exit(False);
    if ADescriptor.HasSet and
       (TargetAccessor.Setter <> DescriptorAccessor.Setter) then
      Exit(False);
  end;

  if IsDataDescriptor(ATargetDesc) and IsDataDescriptor(ADescriptor) then
  begin
    TargetData := TGocciaPropertyDescriptorData(ATargetDesc);
    DescriptorData := TGocciaPropertyDescriptorData(ADescriptor);
    if not TargetData.Writable then
    begin
      if ADescriptor.HasWritableField and DescriptorData.Writable then
        Exit(False);
      if ADescriptor.HasValue and
         not IsSameValue(TargetData.Value, DescriptorData.Value) then
        Exit(False);
    end;
  end;

  Result := True;
end;

function ProxyTargetIsExtensible(const ATarget: TGocciaValue): Boolean;
begin
  if ATarget is TGocciaProxyValue then
  begin
    // A Proxy target is counted; see MAX_PROPERTY_DELEGATION_DEPTH.
    EnterPropertyDelegation;
    try
      Result := TGocciaProxyValue(ATarget).IsExtensibleTrap;
    finally
      LeavePropertyDelegation;
    end;
  end
  else if ATarget is TGocciaObjectValue then
    Result := TGocciaObjectValue(ATarget).Extensible
  else
    Result := False;
end;

function ProxyTargetGetPrototype(const ATarget: TGocciaValue): TGocciaValue;
begin
  if ATarget is TGocciaProxyValue then
  begin
    // A Proxy target is a native call into its [[GetPrototypeOf]]; Proxies
    // nested deeply as targets would otherwise recurse until the native
    // stack ends. See MAX_PROPERTY_DELEGATION_DEPTH.
    EnterPropertyDelegation;
    try
      Exit(TGocciaProxyValue(ATarget).GetPrototypeTrap);
    finally
      LeavePropertyDelegation;
    end;
  end;

  if ATarget is TGocciaObjectValue then
  begin
    if Assigned(TGocciaObjectValue(ATarget).Prototype) then
      Exit(TGocciaObjectValue(ATarget).Prototype);
    Exit(TGocciaNullLiteralValue.NullValue);
  end;

  Result := TGocciaNullLiteralValue.NullValue;
end;

function ProxyTargetSetPrototype(const ATarget, AProto: TGocciaValue): Boolean;
var
  TargetObject: TGocciaObjectValue;
  Walker: TGocciaObjectValue;
begin
  if ATarget is TGocciaProxyValue then
  begin
    // A Proxy target is counted; see MAX_PROPERTY_DELEGATION_DEPTH.
    EnterPropertyDelegation;
    try
      Exit(TGocciaProxyValue(ATarget).SetPrototypeTrap(AProto));
    finally
      LeavePropertyDelegation;
    end;
  end;

  if not (ATarget is TGocciaObjectValue) then
    Exit(False);

  TargetObject := TGocciaObjectValue(ATarget);

  if AProto is TGocciaObjectValue then
  begin
    if TargetObject.Prototype = TGocciaObjectValue(AProto) then
      Exit(True);
  end
  else if AProto is TGocciaNullLiteralValue then
  begin
    if not Assigned(TargetObject.Prototype) then
      Exit(True);
  end
  else
    Exit(False);

  if TargetObject = TGocciaObjectValue.SharedObjectPrototype then
    Exit(False);

  if not TargetObject.Extensible then
    Exit(False);

  if AProto is TGocciaObjectValue then
  begin
    Walker := TGocciaObjectValue(AProto);
    while Assigned(Walker) do
    begin
      if Walker = TargetObject then
        Exit(False);
      if Walker is TGocciaProxyValue then
        Break;
      Walker := Walker.Prototype;
    end;
  end;

  if AProto is TGocciaObjectValue then
    TargetObject.Prototype := TGocciaObjectValue(AProto)
  else
    TargetObject.Prototype := nil;

  Result := True;
end;

function ProxyTargetHasSymbolProperty(
  const ATarget: TGocciaValue;
  const ASymbol: TGocciaSymbolValue): Boolean;
begin
  if not (ATarget is TGocciaObjectValue) then
    Exit(False);

  if ATarget is TGocciaProxyValue then
  begin
    // A Proxy target is a native call into its [[HasProperty]]; see
    // MAX_PROPERTY_DELEGATION_DEPTH.
    EnterPropertyDelegation;
    try
      Exit(TGocciaProxyValue(ATarget).HasSymbolTrap(ASymbol));
    finally
      LeavePropertyDelegation;
    end;
  end;
  Result := TGocciaObjectValue(ATarget).HasSymbolPropertyInChain(ASymbol);
end;

function CreateProxyTrapDescriptorObject(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    if ADescriptor.HasGet then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
        Result.AssignProperty(PROP_GET,
          TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
      else
        Result.AssignProperty(PROP_GET,
          TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
    if ADescriptor.HasSet then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
        Result.AssignProperty(PROP_SET,
          TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
      else
        Result.AssignProperty(PROP_SET,
          TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  end
  else if ADescriptor is TGocciaPropertyDescriptorData then
  begin
    if ADescriptor.HasValue then
      Result.AssignProperty(PROP_VALUE,
        TGocciaPropertyDescriptorData(ADescriptor).Value);
    if ADescriptor.HasWritableField then
      Result.AssignProperty(PROP_WRITABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
  end;
  if ADescriptor.HasEnumerableField then
    Result.AssignProperty(PROP_ENUMERABLE,
      TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
  if ADescriptor.HasConfigurableField then
    Result.AssignProperty(PROP_CONFIGURABLE,
      TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));
end;

procedure ValidateProxyDefineTrapResult(const APropertyLabel: string;
  const ATarget: TGocciaValue;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  ExtensibleTarget: Boolean;
  SettingConfigFalse: Boolean;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  TargetDesc := nil;
  if ATarget is TGocciaObjectValue then
    TargetDesc := TGocciaObjectValue(ATarget).GetOwnPropertyDescriptor(
      APropertyLabel);

  ExtensibleTarget := ProxyTargetIsExtensible(ATarget);
  SettingConfigFalse := ADescriptor.HasConfigurableField and
    not ADescriptor.Configurable;

  if not Assigned(TargetDesc) then
  begin
    if (not ExtensibleTarget) or SettingConfigFalse then
      ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [APropertyLabel]),
        SSuggestProxyTrapInvariant);
    Exit;
  end;

  if not IsCompatibleProxyDefineDescriptor(ExtensibleTarget, ADescriptor,
    TargetDesc) then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [APropertyLabel]),
      SSuggestProxyTrapInvariant);

  if SettingConfigFalse and TargetDesc.Configurable then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [APropertyLabel]),
      SSuggestProxyTrapInvariant);

  if IsDataDescriptor(TargetDesc) and not TargetDesc.Configurable and
     TargetDesc.Writable and ADescriptor.HasWritableField and
     not ADescriptor.Writable then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [APropertyLabel]),
      SSuggestProxyTrapInvariant);
end;

procedure ValidateProxyDefineSymbolTrapResult(
  const ASymbol: TGocciaSymbolValue; const ATarget: TGocciaValue;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  ExtensibleTarget: Boolean;
  PropertyLabel: string;
  SettingConfigFalse: Boolean;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  TargetDesc := nil;
  if ATarget is TGocciaObjectValue then
    TargetDesc := TGocciaObjectValue(ATarget).GetOwnSymbolPropertyDescriptor(
      ASymbol);

  PropertyLabel := ASymbol.ToDisplayString.Value;
  ExtensibleTarget := ProxyTargetIsExtensible(ATarget);
  SettingConfigFalse := ADescriptor.HasConfigurableField and
    not ADescriptor.Configurable;

  if not Assigned(TargetDesc) then
  begin
    if (not ExtensibleTarget) or SettingConfigFalse then
      ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [PropertyLabel]),
        SSuggestProxyTrapInvariant);
    Exit;
  end;

  if not IsCompatibleProxyDefineDescriptor(ExtensibleTarget, ADescriptor,
    TargetDesc) then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [PropertyLabel]),
      SSuggestProxyTrapInvariant);

  if SettingConfigFalse and TargetDesc.Configurable then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [PropertyLabel]),
      SSuggestProxyTrapInvariant);

  if IsDataDescriptor(TargetDesc) and not TargetDesc.Configurable and
     TargetDesc.Writable and ADescriptor.HasWritableField and
     not ADescriptor.Writable then
    ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [PropertyLabel]),
      SSuggestProxyTrapInvariant);
end;

procedure ValidateProxyGetOwnTrapDescriptor(const APropertyLabel: string;
  const AExtensibleTarget: Boolean;
  const ATargetDesc, AResultDesc: TGocciaPropertyDescriptor);
begin
  if not IsCompatibleProxyTrapPropertyDescriptor(AExtensibleTarget,
    AResultDesc, ATargetDesc) then
    ThrowTypeError(Format(SErrorProxyGetOwnIncompatible, [APropertyLabel]),
      SSuggestProxyTrapInvariant);

  if not AResultDesc.Configurable then
  begin
    if not Assigned(ATargetDesc) or ATargetDesc.Configurable then
      ThrowTypeError(Format(SErrorProxyGetOwnInvalidNonConfigurable,
        [APropertyLabel]), SSuggestProxyTrapInvariant);

    if IsDataDescriptor(AResultDesc) and not AResultDesc.Writable then
    begin
      if (not IsDataDescriptor(ATargetDesc)) or ATargetDesc.Writable then
        ThrowTypeError(Format(SErrorProxyGetOwnInvalidNonWritable,
          [APropertyLabel]), SSuggestProxyTrapInvariant);
    end;
  end;
end;

function IsSamePropertyKey(const A, B: TGocciaValue): Boolean;
begin
  if A is TGocciaSymbolValue then
    Result := A = B
  else
    Result := (B is TGocciaStringLiteralValue) and
      (TGocciaStringLiteralValue(A).Value = TGocciaStringLiteralValue(B).Value);
end;

function PropertyKeyLabel(const AKey: TGocciaValue): string;
begin
  if AKey is TGocciaSymbolValue then
    Result := TGocciaSymbolValue(AKey).ToStringLiteral.Value
  else
    Result := TGocciaStringLiteralValue(AKey).Value;
end;

procedure SplitPropertyKeys(const AOrderedKeys: TArray<TGocciaValue>;
  out AStringKeys: TArray<string>;
  out ASymbolKeys: TArray<TGocciaSymbolValue>);
var
  I, StringCount, SymbolCount: Integer;
begin
  SetLength(AStringKeys, Length(AOrderedKeys));
  SetLength(ASymbolKeys, Length(AOrderedKeys));
  StringCount := 0;
  SymbolCount := 0;
  for I := 0 to High(AOrderedKeys) do
  begin
    if AOrderedKeys[I] is TGocciaStringLiteralValue then
    begin
      AStringKeys[StringCount] := TGocciaStringLiteralValue(AOrderedKeys[I]).Value;
      Inc(StringCount);
    end
    else if AOrderedKeys[I] is TGocciaSymbolValue then
    begin
      ASymbolKeys[SymbolCount] := TGocciaSymbolValue(AOrderedKeys[I]);
      Inc(SymbolCount);
    end;
  end;
  SetLength(AStringKeys, StringCount);
  SetLength(ASymbolKeys, SymbolCount);
end;

// ES2026 §10.5.11 [[OwnPropertyKeys]] ( )
procedure TGocciaProxyValue.CollectOwnPropertyTrapKeys(
  out AStringKeys: TArray<string>;
  out ASymbolKeys: TArray<TGocciaSymbolValue>;
  out AOrderedKeys: TArray<TGocciaValue>);
var
  Args: TGocciaArgumentsCollection;
  Count: Integer;
  Element: TGocciaValue;
  ExtensibleTarget: Boolean;
  HasNonconfigurableKey: Boolean;
  I, J: Integer;
  OrderedCount: Integer;
  ResultChecked: TArray<Boolean>;
  ResultLength: Integer;
  ResultObject: TGocciaObjectValue;
  Roots: TGocciaActiveRootFrame;
  SymbolCount: Integer;
  SymbolElement: TGocciaSymbolValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TargetKeyNonconfigurable: TArray<Boolean>;
  TargetKeys: TArray<TGocciaValue>;
  Trap: TGocciaValue;
  TrapResult: TGocciaValue;

  // ES2026 §10.5.11 steps 19.a-b and 21.a-b: AKey must be in
  // uncheckedResultKeys; remove it. trapResult holds no duplicates (step 9),
  // so marking its one occurrence checked is the removal.
  procedure CheckResultKey(const AKey: TGocciaValue);
  var
    K: Integer;
  begin
    for K := 0 to High(AOrderedKeys) do
      if not ResultChecked[K] and IsSamePropertyKey(AKey, AOrderedKeys[K]) then
      begin
        ResultChecked[K] := True;
        Exit;
      end;
    ThrowTypeError(Format(SErrorProxyOwnKeysMissing, [PropertyKeyLabel(AKey)]),
      SSuggestProxyTrapInvariant);
  end;

begin
  CheckRevoked;
  SetLength(AStringKeys, 0);
  SetLength(ASymbolKeys, 0);
  SetLength(AOrderedKeys, 0);

  Trap := GetTrap(PROP_OWN_KEYS);
  if not Assigned(Trap) then
  begin
    // ES2026 §10.5.11 step 6.a: Return ? target.[[OwnPropertyKeys]]().
    if FTarget is TGocciaProxyValue then
    begin
      AOrderedKeys := TargetOwnPropertyKeyValues;
      SplitPropertyKeys(AOrderedKeys, AStringKeys, ASymbolKeys);
    end
    else if FTarget is TGocciaObjectValue then
    begin
      AStringKeys := TGocciaObjectValue(FTarget).GetOwnPropertyKeys;
      ASymbolKeys := TGocciaObjectValue(FTarget).GetOwnSymbols;
      SetLength(AOrderedKeys, Length(AStringKeys) + Length(ASymbolKeys));
      OrderedCount := 0;
      for I := 0 to High(AStringKeys) do
      begin
        AOrderedKeys[OrderedCount] :=
          TGocciaStringLiteralValue.Create(AStringKeys[I]);
        Inc(OrderedCount);
      end;
      for I := 0 to High(ASymbolKeys) do
      begin
        AOrderedKeys[OrderedCount] := ASymbolKeys[I];
        Inc(OrderedCount);
      end;
    end;
    Exit;
  end;

  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(FTarget);
    TrapResult := InvokeTrap(Trap, Args);
  finally
    Args.Free;
  end;

  if not (TrapResult is TGocciaObjectValue) then
    ThrowTypeError(SErrorProxyOwnKeysArray, SSuggestProxyTrapReturnType);

  // The trap result, its keys and the target's keys are held only by this
  // frame while the target's traps and the result's element getters run.
  Roots.Initialize;
  try
    Roots.Add(Self);
    Roots.Add(TrapResult);

    ResultObject := TGocciaObjectValue(TrapResult);
    ResultLength := LengthOfArrayLike(ResultObject);

    SetLength(AStringKeys, ResultLength);
    SetLength(ASymbolKeys, ResultLength);
    SetLength(AOrderedKeys, ResultLength);
    Count := 0;
    SymbolCount := 0;
    OrderedCount := 0;
    for I := 0 to ResultLength - 1 do
    begin
      Element := ResultObject.GetProperty(IntToStr(I));
      if not (Element is TGocciaStringLiteralValue) and
         not (Element is TGocciaSymbolValue) then
        ThrowTypeError(SErrorProxyOwnKeysTypes, SSuggestProxyTrapReturnType);
      Roots.Add(Element);
      if Element is TGocciaStringLiteralValue then
      begin
        for J := 0 to Count - 1 do
          if AStringKeys[J] = TGocciaStringLiteralValue(Element).Value then
            ThrowTypeError(SErrorProxyOwnKeysDuplicate,
              SSuggestProxyTrapInvariant);
        AStringKeys[Count] := TGocciaStringLiteralValue(Element).Value;
        Inc(Count);
      end
      else
      begin
        SymbolElement := TGocciaSymbolValue(Element);
        for J := 0 to SymbolCount - 1 do
          if ASymbolKeys[J] = SymbolElement then
            ThrowTypeError(SErrorProxyOwnKeysDuplicate,
              SSuggestProxyTrapInvariant);
        ASymbolKeys[SymbolCount] := SymbolElement;
        Inc(SymbolCount);
      end;
      AOrderedKeys[OrderedCount] := Element;
      Inc(OrderedCount);
    end;
    SetLength(AStringKeys, Count);
    SetLength(ASymbolKeys, SymbolCount);
    SetLength(AOrderedKeys, OrderedCount);

    if not (FTarget is TGocciaObjectValue) then
      Exit;

    // ES2026 §10.5.11 step 10: Let extensibleTarget be ? IsExtensible(target).
    ExtensibleTarget := ProxyTargetIsExtensible(FTarget);
    // ES2026 §10.5.11 step 11: Let targetKeys be ? target.[[OwnPropertyKeys]]().
    // One read gives the string and symbol keys together, in target order.
    TargetKeys := TargetOwnPropertyKeyValues;
    for I := 0 to High(TargetKeys) do
      Roots.Add(TargetKeys[I]);

    // ES2026 §10.5.11 step 16: read each key's descriptor in targetKeys order
    // and sort it into the configurable or non-configurable keys.
    SetLength(TargetKeyNonconfigurable, Length(TargetKeys));
    HasNonconfigurableKey := False;
    for I := 0 to High(TargetKeys) do
    begin
      TargetDesc := TargetGetOwnPropertyDescriptorForKey(TargetKeys[I]);
      TargetKeyNonconfigurable[I] := Assigned(TargetDesc) and
        not TargetDesc.Configurable;
      if TargetKeyNonconfigurable[I] then
        HasNonconfigurableKey := True;
    end;

    // ES2026 §10.5.11 step 17
    if ExtensibleTarget and not HasNonconfigurableKey then
      Exit;

    // ES2026 §10.5.11 step 18: uncheckedResultKeys is trapResult.
    SetLength(ResultChecked, Length(AOrderedKeys));
    for I := 0 to High(ResultChecked) do
      ResultChecked[I] := False;

    // ES2026 §10.5.11 step 19: every non-configurable target key is reported.
    for I := 0 to High(TargetKeys) do
      if TargetKeyNonconfigurable[I] then
        CheckResultKey(TargetKeys[I]);

    // ES2026 §10.5.11 step 20
    if ExtensibleTarget then
      Exit;

    // ES2026 §10.5.11 step 21: a non-extensible target's configurable keys
    // are reported too.
    for I := 0 to High(TargetKeys) do
      if not TargetKeyNonconfigurable[I] then
        CheckResultKey(TargetKeys[I]);

    // ES2026 §10.5.11 step 22: and nothing else is.
    for I := 0 to High(ResultChecked) do
      if not ResultChecked[I] then
        ThrowTypeError(SErrorProxyOwnKeysExtra, SSuggestProxyTrapInvariant);
  finally
    Roots.Clear;
  end;
end;

// ES2026 §28.1.1 [[Get]](P, Receiver)
function TGocciaProxyValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaProxyValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      // ES2026 §10.5.8 step 7: Pass the receiver, not the proxy itself
      Args.Add(AThisContext);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.8 step 8-9: Invariant validation.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data: result must be SameValue
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError(Format(SErrorProxyGetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without getter: result must be undefined
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(Format(SErrorProxyGetNoGetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else if FTarget is TGocciaObjectValue then
    // ES2026 §10.5.8 step 10: Return ? target.[[Get]](P, Receiver)
    Result := DelegateGetProperty(TGocciaObjectValue(FTarget), AName,
      AThisContext)
  else
    Result := FTarget.GetProperty(AName);
end;

// ES2026 §28.1.1 [[Set]](P, V, Receiver)
procedure TGocciaProxyValue.AssignProperty(const AName: string;
  const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(AValue);
      Args.Add(Self);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError(Format(SErrorProxySetReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError(Format(SErrorProxySetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(Format(SErrorProxySetNoSetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else
  begin
    if (FTarget is TGocciaObjectValue) and
       DelegateSetProperty(TGocciaObjectValue(FTarget), AName, AValue, Self) then
      Exit;
    ThrowTypeError(Format(SErrorProxySetReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
  end;
end;

// ES2026 §10.5.9 [[Set]](P, V, Receiver) — receiver-aware, returns Boolean
function TGocciaProxyValue.AssignPropertyWithReceiver(const AName: string;
  const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(AValue);
      Args.Add(AReceiver);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        Exit(False);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.9 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError(Format(SErrorProxySetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(Format(SErrorProxySetNoSetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;

    Result := True;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := DelegateSetProperty(TGocciaObjectValue(FTarget), AName, AValue,
        AReceiver)
    else
      Result := False;
  end;
end;

function TGocciaProxyValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasTrap(AName);
end;

// ES2026 §28.1.1 [[HasProperty]](P)
function TGocciaProxyValue.HasTrap(const AName: string): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_HAS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 9-10: Invariant checks when trap returns false.
    if (not Result) and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
      // Cannot hide non-configurable own property
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError(Format(SErrorProxyHasNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
      // Cannot hide own property on non-extensible target
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyHasNonExtensible, SSuggestProxyTrapInvariant);
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := DelegateHasProperty(TGocciaObjectValue(FTarget), AName)
    else
      Result := False;
  end;
end;

// ES2026 §28.1.1 [[HasProperty]](P) — symbol key overload
function TGocciaProxyValue.HasSymbolTrap(
  const ASymbol: TGocciaSymbolValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_HAS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 9-10: Invariant checks (mirror HasTrap).
    if (not Result) and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TargetGetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError(SErrorProxyHasSymbolNonConfigurable, SSuggestProxyTrapInvariant);
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyHasSymbolNonExtensible, SSuggestProxyTrapInvariant);
    end;
  end
  else
  begin
    Result := ProxyTargetHasSymbolProperty(FTarget, ASymbol);
  end;
end;

function TGocciaProxyValue.HasOwnProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // ES2026 §28.1.1: Own-property check uses [[GetOwnProperty]], not
  // [[HasProperty]], so Object.hasOwn(proxy, key) only reports true
  // for own properties, not inherited ones.
  Descriptor := GetOwnPropertyDescriptor(AName);
  Result := Assigned(Descriptor);
end;

// ES2026 §28.1.1 [[Get]](P, Receiver) — symbol key
function TGocciaProxyValue.GetSymbolProperty(
  const ASymbol: TGocciaSymbolValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      Args.Add(Self);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 8-9: Invariant validation for symbol keys.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError(SErrorProxyGetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(SErrorProxyGetSymbolNoGetter, SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      // ES2026 §10.5.8 step 10: the receiver is the proxy.
      Result := DelegateGetSymbolProperty(TGocciaObjectValue(FTarget), ASymbol,
        Self)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §10.5.8 [[Get]](P, Receiver) — symbol key, receiver-aware
function TGocciaProxyValue.GetSymbolPropertyWithReceiver(
  const ASymbol: TGocciaSymbolValue;
  const AReceiver: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      Args.Add(AReceiver);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.8 step 8-9: Invariant validation for symbol keys.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError(SErrorProxyGetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(SErrorProxyGetSymbolNoGetter, SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := DelegateGetSymbolProperty(TGocciaObjectValue(FTarget), ASymbol,
        AReceiver)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §10.5.9 [[Set]](P, V, Receiver) — symbol key, receiver-aware
function TGocciaProxyValue.AssignSymbolPropertyWithReceiver(
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      Args.Add(AValue);
      Args.Add(AReceiver);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        Exit(False);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.9 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TargetGetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError(SErrorProxySetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(SErrorProxySetSymbolNoSetter, SSuggestProxyTrapInvariant);
      end;
    end;

    Result := True;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := DelegateSetSymbolProperty(TGocciaObjectValue(FTarget), ASymbol,
        AValue, AReceiver)
    else
      Result := False;
  end;
end;

// ES2026 §28.1.1 [[HasProperty]](P) — symbol key (virtual override)
function TGocciaProxyValue.HasSymbolProperty(
  const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := HasSymbolTrap(ASymbol);
end;

// ES2026 §28.1.1 [[Delete]](P)
function TGocciaProxyValue.DeleteProperty(const AName: string): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_DELETE_PROPERTY);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 11-12: Invariant checks when trap returns true.
    if Result and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
      // Cannot delete non-configurable own property
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError(Format(SErrorProxyDeleteNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
      // Cannot delete own property on non-extensible target (property still exists)
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyDeleteNonExtensible, SSuggestProxyTrapInvariant);
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TargetDeleteProperty(AName)
    else
      Result := True;
  end;
end;

// ES2026 §28.1.1 [[GetOwnProperty]](P)
function TGocciaProxyValue.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetObject: TGocciaObjectValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TrapDesc: TGocciaPropertyDescriptor;
  CompletedDesc: TGocciaPropertyDescriptor;
  Roots: TGocciaActiveRootFrame;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    TargetObject := nil;
    TargetDesc := nil;
    if FTarget is TGocciaObjectValue then
    begin
      TargetObject := TGocciaObjectValue(FTarget);
      TargetDesc := TargetGetOwnPropertyDescriptor(AName);
    end;

    // Per spec, trap must return an object or undefined (not null)
    if TrapResult is TGocciaUndefinedLiteralValue then
    begin
      // ES2026 §28.1.1 step 10-11: Cannot hide non-configurable property
      if Assigned(TargetObject) then
      begin
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
          ThrowTypeError(Format(SErrorProxyGetOwnNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
        if Assigned(TargetDesc) and not TargetObject.Extensible then
          ThrowTypeError(SErrorProxyGetOwnNonExtensible, SSuggestProxyTrapInvariant);
      end;
      Exit(nil);
    end;

    if not (TrapResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorProxyGetOwnReturnType, SSuggestProxyTrapReturnType);

    TrapDesc := ToPropertyDescriptor(TrapResult, TargetDesc);
    try
      CompletedDesc := CompleteProxyTrapPropertyDescriptor(TrapDesc);
    finally
      TrapDesc.Free;
    end;
    // CompletedDesc is a plain class and the sole holder of the value the
    // trap just produced. ProxyTargetIsExtensible is evaluated as an argument
    // below and runs the target's isExtensible trap when the target is itself
    // a proxy — guest code, with that value reachable from nowhere. Without
    // this the guest is handed back a recycled object rather than a crash,
    // which is the worse failure: a silently wrong descriptor value.
    Roots.Initialize;
    try
      Roots.Add(Self);
      Roots.Add(FTarget);
      CompletedDesc.PushRoots(Roots);
      try
        if Assigned(TargetObject) then
          ValidateProxyGetOwnTrapDescriptor(AName,
            ProxyTargetIsExtensible(FTarget), TargetDesc, CompletedDesc);
        Result := CompletedDesc;
      except
        CompletedDesc.Free;
        raise;
      end;
    finally
      Roots.Clear;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TargetGetOwnPropertyDescriptor(AName)
    else
      Result := nil;
  end;
end;

// ES2026 §10.5.5 [[GetOwnProperty]](P) — symbol key
function TGocciaProxyValue.GetOwnSymbolPropertyDescriptor(
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetObject: TGocciaObjectValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TrapDesc: TGocciaPropertyDescriptor;
  CompletedDesc: TGocciaPropertyDescriptor;
  PropertyLabel: string;
  Roots: TGocciaActiveRootFrame;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      TrapResult := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    TargetObject := nil;
    TargetDesc := nil;
    PropertyLabel := ASymbol.ToDisplayString.Value;
    if FTarget is TGocciaObjectValue then
    begin
      TargetObject := TGocciaObjectValue(FTarget);
      TargetDesc := TargetGetOwnSymbolPropertyDescriptor(ASymbol);
    end;

    if TrapResult is TGocciaUndefinedLiteralValue then
    begin
      if Assigned(TargetObject) then
      begin
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
          ThrowTypeError(Format(SErrorProxyGetOwnNonConfigurable,
            [PropertyLabel]), SSuggestProxyTrapInvariant);
        if Assigned(TargetDesc) and not TargetObject.Extensible then
          ThrowTypeError(SErrorProxyGetOwnNonExtensible,
            SSuggestProxyTrapInvariant);
      end;
      Exit(nil);
    end;

    if not (TrapResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorProxyGetOwnReturnType, SSuggestProxyTrapReturnType);

    TrapDesc := ToPropertyDescriptor(TrapResult, TargetDesc);
    try
      CompletedDesc := CompleteProxyTrapPropertyDescriptor(TrapDesc);
    finally
      TrapDesc.Free;
    end;
    // Same window as the string arm above.
    Roots.Initialize;
    try
      Roots.Add(Self);
      Roots.Add(FTarget);
      CompletedDesc.PushRoots(Roots);
      try
        if Assigned(TargetObject) then
          ValidateProxyGetOwnTrapDescriptor(PropertyLabel,
            ProxyTargetIsExtensible(FTarget), TargetDesc, CompletedDesc);
        Result := CompletedDesc;
      except
        CompletedDesc.Free;
        raise;
      end;
    finally
      Roots.Clear;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TargetGetOwnSymbolPropertyDescriptor(ASymbol)
    else
      Result := nil;
  end;
end;

// ES2026 §28.1.1 [[DefineOwnProperty]](P, Desc)
procedure TGocciaProxyValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DescObj: TGocciaObjectValue;
  TrapResult: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  CheckRevoked;
  // The frame opens before GetTrap: reading the trap off the handler is the
  // first guest-code safe point, and it happens before anything has read the
  // descriptor. See PushDefineTrapRoots.
  Roots.Initialize;
  try
    PushDefineTrapRoots(Roots, ADescriptor);
    Trap := GetTrap(PROP_DEFINE_PROPERTY);
    if Assigned(Trap) then
    begin
      DescObj := CreateProxyTrapDescriptorObject(ADescriptor);
      Roots.Add(DescObj);
      Args := TGocciaArgumentsCollection.Create;
      try
        Args.Add(FTarget);
        Args.Add(TGocciaStringLiteralValue.Create(AName));
        Args.Add(DescObj);
        TrapResult := InvokeTrap(Trap, Args);
        if not TrapResult.ToBooleanLiteral.Value then
          ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
        ValidateProxyDefineTrapResult(AName, FTarget, ADescriptor);
        ADescriptor.Free;
      finally
        Args.Free;
      end;
    end
    else
    begin
      if FTarget is TGocciaObjectValue then
        TargetDefineProperty(AName, ADescriptor)
      else
        ThrowTypeError(SErrorProxyDefineNonObject, SSuggestProxyTargetType);
    end;
  finally
    Roots.Clear;
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — symbol key
procedure TGocciaProxyValue.DefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DescObj: TGocciaObjectValue;
  TrapResult: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  CheckRevoked;
  // Same window as the string arm; see PushDefineTrapRoots.
  Roots.Initialize;
  try
    PushDefineTrapRoots(Roots, ADescriptor);
    Trap := GetTrap(PROP_DEFINE_PROPERTY);
    if Assigned(Trap) then
    begin
      DescObj := CreateProxyTrapDescriptorObject(ADescriptor);
      Roots.Add(DescObj);
      Args := TGocciaArgumentsCollection.Create;
      try
        Args.Add(FTarget);
        Args.Add(ASymbol);
        Args.Add(DescObj);
        TrapResult := InvokeTrap(Trap, Args);
        if not TrapResult.ToBooleanLiteral.Value then
          ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [ASymbol.ToDisplayString.Value]), SSuggestProxyTrapInvariant);
        ValidateProxyDefineSymbolTrapResult(ASymbol, FTarget, ADescriptor);
        ADescriptor.Free;
      finally
        Args.Free;
      end;
    end
    else
    begin
      if FTarget is TGocciaObjectValue then
        TargetDefineSymbolProperty(ASymbol, ADescriptor)
      else
        ThrowTypeError(SErrorProxyDefineNonObject, SSuggestProxyTargetType);
    end;
  finally
    Roots.Clear;
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — boolean variant
// Returns false instead of throwing when the trap returns a falsy value.
function TGocciaProxyValue.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DescObj: TGocciaObjectValue;
  TrapResult: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  // Same window as DefineProperty; see PushDefineTrapRoots.
  Roots.Initialize;
  try
    // ADescriptor is owned here until the trap call or the target takes it;
    // a revoked Proxy, or a trap lookup stopped by the delegation bound,
    // throws before then.
    try
      CheckRevoked;
      PushDefineTrapRoots(Roots, ADescriptor);
      Trap := GetTrap(PROP_DEFINE_PROPERTY);
      if Assigned(Trap) then
        DescObj := CreateProxyTrapDescriptorObject(ADescriptor);
    except
      ADescriptor.Free;
      raise;
    end;
    if Assigned(Trap) then
    begin
      Roots.Add(DescObj);
      Args := TGocciaArgumentsCollection.Create;
      try
        try
          Args.Add(FTarget);
          Args.Add(TGocciaStringLiteralValue.Create(AName));
          Args.Add(DescObj);
          TrapResult := InvokeTrap(Trap, Args);
          Result := TrapResult.ToBooleanLiteral.Value;
          if Result then
            ValidateProxyDefineTrapResult(AName, FTarget, ADescriptor);
        finally
          ADescriptor.Free;
        end;
      finally
        Args.Free;
      end;
    end
    else
    begin
      if FTarget is TGocciaObjectValue then
        Result := TargetTryDefineProperty(AName, ADescriptor)
      else
      begin
        ADescriptor.Free;
        Result := False;
      end;
    end;
  finally
    Roots.Clear;
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — symbol key, boolean variant
function TGocciaProxyValue.TryDefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DescObj: TGocciaObjectValue;
  TrapResult: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  // Same window as DefineProperty; see PushDefineTrapRoots.
  Roots.Initialize;
  try
    // See TryDefineProperty.
    try
      CheckRevoked;
      PushDefineTrapRoots(Roots, ADescriptor);
      Trap := GetTrap(PROP_DEFINE_PROPERTY);
      if Assigned(Trap) then
        DescObj := CreateProxyTrapDescriptorObject(ADescriptor);
    except
      ADescriptor.Free;
      raise;
    end;
    if Assigned(Trap) then
    begin
      Roots.Add(DescObj);
      Args := TGocciaArgumentsCollection.Create;
      try
        try
          Args.Add(FTarget);
          Args.Add(ASymbol);
          Args.Add(DescObj);
          TrapResult := InvokeTrap(Trap, Args);
          Result := TrapResult.ToBooleanLiteral.Value;
          if Result then
            ValidateProxyDefineSymbolTrapResult(ASymbol, FTarget, ADescriptor);
        finally
          ADescriptor.Free;
        end;
      finally
        Args.Free;
      end;
    end
    else
    begin
      if FTarget is TGocciaObjectValue then
        Result := TargetTryDefineSymbolProperty(ASymbol, ADescriptor)
      else
      begin
        ADescriptor.Free;
        Result := False;
      end;
    end;
  finally
    Roots.Clear;
  end;
end;

// ES2026 §28.1.1 [[OwnPropertyKeys]]()
function TGocciaProxyValue.GetOwnPropertyKeys: TArray<string>;
var
  SymbolKeys: TArray<TGocciaSymbolValue>;
  OrderedKeys: TArray<TGocciaValue>;
begin
  CollectOwnPropertyTrapKeys(Result, SymbolKeys, OrderedKeys);
end;

function TGocciaProxyValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaProxyValue.GetOwnPropertyKeyValues: TArray<TGocciaValue>;
var
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
begin
  CollectOwnPropertyTrapKeys(StringKeys, SymbolKeys, Result);
end;

function TGocciaProxyValue.GetOwnSymbols: TArray<TGocciaSymbolValue>;
var
  OrderedKeys: TArray<TGocciaValue>;
  StringKeys: TArray<string>;
begin
  CollectOwnPropertyTrapKeys(StringKeys, Result, OrderedKeys);
end;

function TGocciaProxyValue.GetEnumerablePropertyNames: TArray<string>;
var
  AllKeys: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  DescriptorTrap: TGocciaValue;
  FilteredKeys: TArray<string>;
  I, Count: Integer;
  TargetObj: TGocciaObjectValue;
begin
  AllKeys := GetOwnPropertyKeys;
  SetLength(FilteredKeys, Length(AllKeys));
  Count := 0;
  DescriptorTrap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if FTarget is TGocciaObjectValue then
    TargetObj := TGocciaObjectValue(FTarget)
  else
    TargetObj := nil;
  for I := 0 to Length(AllKeys) - 1 do
  begin
    if Assigned(DescriptorTrap) then
      Descriptor := GetOwnPropertyDescriptor(AllKeys[I])
    else if Assigned(TargetObj) then
      Descriptor := TargetGetOwnPropertyDescriptor(AllKeys[I])
    else
      Descriptor := nil;
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      FilteredKeys[Count] := AllKeys[I];
      Inc(Count);
    end;
  end;
  SetLength(FilteredKeys, Count);
  Result := FilteredKeys;
end;

function TGocciaProxyValue.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

// ES2026 §28.1.1 [[GetPrototypeOf]]()
function TGocciaProxyValue.GetPrototypeTrap: TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetProto: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_PROTOTYPE_OF);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.1 step 5: Result must be an Object or null.
    if not (Result is TGocciaObjectValue) and
       not (Result is TGocciaNullLiteralValue) then
      ThrowTypeError(SErrorProxyGetProtoReturnType, SSuggestProxyTrapReturnType);

    // ES2026 §28.1.1 step 8: If target is non-extensible, trap result
    // must be the same as the target's actual prototype.
    if not ProxyTargetIsExtensible(FTarget) then
    begin
      TargetProto := ProxyTargetGetPrototype(FTarget);
      if Result <> TargetProto then
        ThrowTypeError(SErrorProxyGetProtoMismatch, SSuggestProxyTrapInvariant);
    end;
  end
  else
    Result := ProxyTargetGetPrototype(FTarget);
end;

// ES2026 §28.1.1 [[SetPrototypeOf]](V)
function TGocciaProxyValue.SetPrototypeTrap(
  const AProto: TGocciaValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET_PROTOTYPE_OF);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(AProto);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 12: If trap returns true and target is
    // non-extensible, the new prototype must match the target's current one.
    if Result and not ProxyTargetIsExtensible(FTarget) then
    begin
      if ProxyTargetGetPrototype(FTarget) <> AProto then
        ThrowTypeError(SErrorProxySetProtoNonExtensible, SSuggestProxyTrapInvariant);
    end;
  end
  else
    Result := ProxyTargetSetPrototype(FTarget, AProto);
end;

// ES2026 §28.1.1 [[IsExtensible]]()
function TGocciaProxyValue.IsExtensibleTrap: Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetExtensible: Boolean;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_IS_EXTENSIBLE);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 7: Validate against target extensibility
    TargetExtensible := ProxyTargetIsExtensible(FTarget);
    if Result <> TargetExtensible then
      ThrowTypeError(SErrorProxyIsExtensibleMismatch, SSuggestProxyTrapInvariant);
  end
  else
    Result := ProxyTargetIsExtensible(FTarget);
end;

// ES2026 §10.5.4 [[PreventExtensions]]()
function TGocciaProxyValue.TryPreventExtensions: Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_PREVENT_EXTENSIONS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    if Result and ProxyTargetIsExtensible(FTarget) then
      ThrowTypeError(SErrorProxyPreventExtensionsStillExtensible, SSuggestProxyTrapInvariant);
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TargetTryPreventExtensions
    else
      Result := False;
  end;
end;

procedure TGocciaProxyValue.PreventExtensions;
begin
  if not TryPreventExtensions then
    ThrowTypeError(SErrorProxyPreventExtensionsFalse, SSuggestProxyTrapInvariant);
end;

// ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
function TGocciaProxyValue.ApplyTrap(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  ArgsArray: TGocciaArrayValue;
  I: Integer;
begin
  CheckRevoked;

  if not FTarget.IsCallable then
    ThrowTypeError(SErrorProxyApplyNonFunction, SSuggestProxyTargetType);

  Trap := GetTrap(PROP_APPLY);
  if Assigned(Trap) then
  begin
    // Build arguments array
    ArgsArray := TGocciaArrayValue.Create;
    for I := 0 to AArguments.Length - 1 do
      ArgsArray.Elements.Add(AArguments.GetElement(I));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(AThisValue);
      Args.Add(ArgsArray);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;
  end
  else
  begin
    // No apply trap: call the target directly (nested proxies first)
    if FTarget is TGocciaProxyValue then
    begin
      // A Proxy target is counted; see MAX_PROPERTY_DELEGATION_DEPTH.
      EnterPropertyDelegation;
      try
        Result := TGocciaProxyValue(FTarget).ApplyTrap(AArguments, AThisValue);
      finally
        LeavePropertyDelegation;
      end;
    end
    else if FTarget is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FTarget).Call(AArguments, AThisValue)
    else if FTarget is TGocciaClassValue then
      Result := TGocciaClassValue(FTarget).Call(AArguments, AThisValue)
    else
      ThrowTypeError(SErrorProxyTargetNotCallable, SSuggestProxyTargetType);
  end;
end;

// ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
function TGocciaProxyValue.ConstructTrap(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  ArgsArray: TGocciaArrayValue;
  EffectiveNewTarget: TGocciaValue;
  I: Integer;
begin
  CheckRevoked;

  // ES2026 §28.1.1 step 1: Proxy [[Construct]] only exists when target
  // is constructable. Validate before dispatching to the trap.
  if not FTarget.IsConstructable then
    ThrowTypeError(SErrorProxyTargetNotConstructor, SSuggestProxyTargetType);

  // Default newTarget for `new proxy(...)` is the proxy itself per ES2026.
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := Self;

  Trap := GetTrap(PROP_CONSTRUCT);
  if Assigned(Trap) then
  begin
    // Build arguments array
    ArgsArray := TGocciaArrayValue.Create;
    for I := 0 to AArguments.Length - 1 do
      ArgsArray.Elements.Add(AArguments.GetElement(I));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ArgsArray);
      Args.Add(EffectiveNewTarget);
      Result := InvokeTrap(Trap, Args);
      if Result.IsPrimitive then
        ThrowTypeError(SErrorProxyConstructReturnType, SSuggestProxyTrapReturnType);
    finally
      Args.Free;
    end;
  end
  else
    // No construct trap: forward to target's [[Construct]](args, EffectiveNewTarget)
    // through the shared dispatch — handles bound chain unwrap (so
    // `new Proxy(F.bind(obj), {})()` does not leak the bound `this` into the
    // synthetic receiver), nested proxies, classes, ordinary functions, and
    // native constructors. Native-constructor newTarget propagation remains
    // tracked in #530.
    if FTarget is TGocciaProxyValue then
    begin
      // A Proxy target is counted; see MAX_PROPERTY_DELEGATION_DEPTH.
      EnterPropertyDelegation;
      try
        Result := ConstructValue(FTarget, AArguments, EffectiveNewTarget);
      finally
        LeavePropertyDelegation;
      end;
    end
    else
      Result := ConstructValue(FTarget, AArguments, EffectiveNewTarget);
end;

// The innermost target of a nest of Proxies. typeof, IsCallable and
// IsConstructable cannot throw, so they step through the nest in a loop
// rather than recursing through it.
function InnermostProxyTarget(const AProxy: TGocciaProxyValue): TGocciaValue;
begin
  Result := AProxy.Target;
  while Result.ClassType = TGocciaProxyValue do
    Result := TGocciaProxyValue(Result).Target;
end;

function TGocciaProxyValue.TypeOf: string;
begin
  // ES2026 §28.1.1: Revocation disables operations, not type
  // inspection. typeof and IsCallable always reflect the target.
  Result := InnermostProxyTarget(Self).TypeOf;
end;

function TGocciaProxyValue.IsCallable: Boolean;
begin
  Result := InnermostProxyTarget(Self).IsCallable;
end;

function TGocciaProxyValue.IsConstructable: Boolean;
begin
  Result := InnermostProxyTarget(Self).IsConstructable;
end;

function TGocciaProxyValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_PROXY;
end;

procedure TGocciaProxyValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FTarget) then
    FTarget.MarkReferences;
  if Assigned(FHandler) then
    FHandler.MarkReferences;
end;

procedure TGocciaProxyValue.Revoke;
begin
  FRevoked := True;
end;

{ TGocciaProxyRevoker }

constructor TGocciaProxyRevoker.Create(const AProxy: TGocciaProxyValue);
begin
  inherited Create;
  FProxy := AProxy;
end;

function TGocciaProxyRevoker.RevokeCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(FProxy) then
  begin
    FProxy.Revoke;
    FProxy := nil;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaProxyRevoker.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FProxy) then
    FProxy.MarkReferences;
end;

{ TGocciaRevocableProxyResult }

constructor TGocciaRevocableProxyResult.Create(
  const ARevoker: TGocciaProxyRevoker);
begin
  inherited Create;
  FRevoker := ARevoker;
end;

procedure TGocciaRevocableProxyResult.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FRevoker) then
    FRevoker.MarkReferences;
end;

function IsProxyDispatchValue(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue is TGocciaProxyValue;
end;

function DispatchProxyApply(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).ApplyTrap(AArguments, AThisValue);
end;

function DispatchProxyConstruct(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).ConstructTrap(AArguments, ANewTarget);
end;

function DispatchProxyGetPrototype(const AProxy: TGocciaObjectValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).GetPrototypeTrap;
end;

function DispatchProxyGetFunctionRealm(
  const AProxy: TGocciaValue): TGocciaRealm;
var
  Proxy: TGocciaProxyValue;
begin
  // ES2026 §7.3.24 GetFunctionRealm steps through a nest of Proxies, each
  // of which must not be revoked; a loop, so a deep nest takes no stack.
  Proxy := TGocciaProxyValue(AProxy);
  Proxy.CheckRevoked;
  while Proxy.FTarget is TGocciaProxyValue do
  begin
    Proxy := TGocciaProxyValue(Proxy.FTarget);
    Proxy.CheckRevoked;
  end;

  if Proxy.FTarget is TGocciaFunctionBase then
    Exit(TGocciaFunctionBase(Proxy.FTarget).CreationRealm);

  if Proxy.FTarget is TGocciaClassValue then
    Exit(TGocciaClassValue(Proxy.FTarget).CreationRealm);

  Result := nil;
end;

initialization
  RegisterProxyDispatchHooks(IsProxyDispatchValue, DispatchProxyApply,
    DispatchProxyConstruct, DispatchProxyGetPrototype,
    DispatchProxyGetFunctionRealm);

end.
