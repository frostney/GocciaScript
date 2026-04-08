unit Goccia.Builtins.GlobalProxy;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalProxy = class
  private
    FConstructorValue: TGocciaValue;
    FThrowError: TGocciaThrowErrorCallback;

    function ProxyConstruct(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ProxyRevocable(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);

    property ConstructorValue: TGocciaValue read FConstructorValue;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.ProxyValue;

constructor TGocciaGlobalProxy.Create(const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  ConstructorFn: TGocciaNativeFunctionValue;
begin
  FThrowError := AThrowError;

  ConstructorFn := TGocciaNativeFunctionValue.Create(ProxyConstruct,
    CONSTRUCTOR_PROXY, 2);
  ConstructorFn.SetProperty(PROP_REVOCABLE,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ProxyRevocable,
      PROP_REVOCABLE, 2));
  FConstructorValue := ConstructorFn;

  AScope.DefineLexicalBinding(CONSTRUCTOR_PROXY, FConstructorValue, dtConst);
end;

// ES2026 $28.2.1 Proxy(target, handler)
function TGocciaGlobalProxy.ProxyConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TargetArg, HandlerArg: TGocciaValue;
begin
  // Proxy must be called with new
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Constructor Proxy requires ''new''');

  if AArgs.Length < 2 then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  TargetArg := AArgs.GetElement(0);
  HandlerArg := AArgs.GetElement(1);

  // Target must be an object (or function)
  if TargetArg.IsPrimitive then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  // Handler must be an object
  if HandlerArg.IsPrimitive then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  if not (HandlerArg is TGocciaObjectValue) then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  Result := TGocciaProxyValue.Create(TargetArg,
    TGocciaObjectValue(HandlerArg));
end;

// ES2026 $28.2.2 Proxy.revocable(target, handler)
function TGocciaGlobalProxy.ProxyRevocable(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TargetArg, HandlerArg: TGocciaValue;
  ProxyInstance: TGocciaProxyValue;
  Revoker: TGocciaProxyRevoker;
  ResultObj: TGocciaObjectValue;
begin
  if AArgs.Length < 2 then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  TargetArg := AArgs.GetElement(0);
  HandlerArg := AArgs.GetElement(1);

  // Target must be an object (or function)
  if TargetArg.IsPrimitive then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  // Handler must be an object
  if HandlerArg.IsPrimitive then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  if not (HandlerArg is TGocciaObjectValue) then
    ThrowTypeError('Cannot create proxy with a non-object as target or handler');

  ProxyInstance := TGocciaProxyValue.Create(TargetArg,
    TGocciaObjectValue(HandlerArg));
  Revoker := TGocciaProxyRevoker.Create(ProxyInstance);

  ResultObj := TGocciaObjectValue.Create;
  ResultObj.AssignProperty(PROP_PROXY, ProxyInstance);
  ResultObj.AssignProperty(PROP_REVOKE,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Revoker.RevokeCallback, PROP_REVOKE, 0));

  Result := ResultObj;
end;

end.
