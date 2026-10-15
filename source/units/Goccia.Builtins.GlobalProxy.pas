unit Goccia.Builtins.GlobalProxy;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalProxy = class
  private
    FConstructorValue: TGocciaValue;

    function ProxyConstruct(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ProxyRevocable(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AScope: TGocciaScope;
      const ADefineGlobalBinding: Boolean = True);

    property ConstructorValue: TGocciaValue read FConstructorValue;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.ProxyValue;

constructor TGocciaGlobalProxy.Create(const AScope: TGocciaScope;
  const ADefineGlobalBinding: Boolean = True);
var
  ConstructorFn: TGocciaNativeFunctionValue;
begin
  ConstructorFn := TGocciaNativeFunctionValue.Create(ProxyConstruct,
    CONSTRUCTOR_PROXY, 2);
  ConstructorFn.SetProperty(PROP_REVOCABLE,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ProxyRevocable,
      PROP_REVOCABLE, 2));
  FConstructorValue := ConstructorFn;

  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(CONSTRUCTOR_PROXY, FConstructorValue, dtConst, True);
end;

// ES2026 §28.2.1 Proxy(target, handler)
function TGocciaGlobalProxy.ProxyConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TargetArg, HandlerArg: TGocciaValue;
begin
  // Proxy must be called with new
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError(SErrorProxyRequiresNew, SSuggestRequiresNew);

  if AArgs.Length < 2 then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  TargetArg := AArgs.GetElement(0);
  HandlerArg := AArgs.GetElement(1);

  // Target must be an object (or function)
  if TargetArg.IsPrimitive then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  // Handler must be an object
  if HandlerArg.IsPrimitive then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  if not (HandlerArg is TGocciaObjectValue) then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  Result := TGocciaProxyValue.Create(TargetArg,
    TGocciaObjectValue(HandlerArg));
end;

// ES2026 §28.2.2 Proxy.revocable(target, handler)
function TGocciaGlobalProxy.ProxyRevocable(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TargetArg, HandlerArg: TGocciaValue;
  ProxyInstance: TGocciaProxyValue;
  Revoker: TGocciaProxyRevoker;
  ResultObj: TGocciaObjectValue;
  RevokeFunction: TGocciaNativeFunctionValue;
  Roots: TGocciaActiveRootFrame;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  TargetArg := AArgs.GetElement(0);
  HandlerArg := AArgs.GetElement(1);

  // Target must be an object (or function)
  if TargetArg.IsPrimitive then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  // Handler must be an object
  if HandlerArg.IsPrimitive then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  if not (HandlerArg is TGocciaObjectValue) then
    ThrowTypeError(SErrorProxyNonObjectTargetOrHandler, SSuggestProxyTargetType);

  // Each allocation and property store below is a collection safe point, and
  // until the result holds them the proxy, the revoker and the revoke
  // function are held only here (issue #1679).
  Roots.Initialize;
  try
    ProxyInstance := TGocciaProxyValue.Create(TargetArg,
      TGocciaObjectValue(HandlerArg));
    Roots.Add(ProxyInstance);
    Revoker := TGocciaProxyRevoker.Create(ProxyInstance);
    Roots.Add(Revoker);

    // Use TGocciaRevocableProxyResult to hold the Revoker via an
    // internal Pascal field (not a JS-visible property) so the GC can
    // trace it without exposing implementation state to reflection APIs.
    ResultObj := TGocciaRevocableProxyResult.Create(Revoker);
    Roots.Add(ResultObj);
    ResultObj.AssignProperty(PROP_PROXY, ProxyInstance);
    // The revoke function calls a method of Revoker, so it keeps Revoker
    // alive itself: a program can keep revoke and drop the result object
    // that also holds it.
    RevokeFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Revoker.RevokeCallback, '', 0);
    RevokeFunction.CapturedRoot := Revoker;
    ResultObj.AssignProperty(PROP_REVOKE, RevokeFunction);

    Result := ResultObj;
  finally
    Roots.Clear;
  end;
end;

end.
