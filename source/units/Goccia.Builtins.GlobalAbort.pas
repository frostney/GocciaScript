unit Goccia.Builtins.GlobalAbort;

// WHATWG DOM AbortController and AbortSignal global registration.

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalAbort = class(TGocciaBuiltin)
  private
    FAbortControllerConstructor: TGocciaNativeFunctionValue;
    FAbortControllerPrototype: TGocciaObjectValue;
    function AbortControllerCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AbortControllerConstruct(const AArgs: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
    function AbortSignalCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AbortSignalAbort(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AbortSignalTimeout(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.ObjectModel,
  Goccia.Values.AbortValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectPropertyDescriptor;

const
  UNSIGNED_LONG_LONG_BOUND = 1.8446744073709552E19; // 2^64

constructor TGocciaGlobalAbort.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  AbortSignalConstructor: TGocciaNativeFunctionValue;
  Members: TGocciaMemberCollection;
  StaticMembers: TArray<TGocciaMemberDefinition>;
begin
  inherited Create(AName, AScope, AThrowError);

  FAbortControllerConstructor := TGocciaNativeFunctionValue.Create(
    AbortControllerCall, CONSTRUCTOR_ABORT_CONTROLLER, 0);
  FAbortControllerConstructor.ConstructCallback := AbortControllerConstruct;
  TGocciaAbortControllerValue.ExposePrototype(FAbortControllerConstructor);
  FAbortControllerPrototype := TGocciaObjectValue(
    FAbortControllerConstructor.GetProperty(PROP_PROTOTYPE));
  AScope.DefineLexicalBinding(CONSTRUCTOR_ABORT_CONTROLLER,
    FAbortControllerConstructor, dtConst, True);

  AbortSignalConstructor := TGocciaNativeFunctionValue.Create(
    AbortSignalCall, CONSTRUCTOR_ABORT_SIGNAL, 0);
  AbortSignalConstructor.NotConstructable := True;
  TGocciaAbortSignalValue.ExposePrototype(AbortSignalConstructor);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_ABORT, AbortSignalAbort, 1,
      gmkStaticMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_TIMEOUT, AbortSignalTimeout, 1,
      gmkStaticMethod, [gmfNoFunctionPrototype]);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(AbortSignalConstructor, StaticMembers);
  AScope.DefineLexicalBinding(CONSTRUCTOR_ABORT_SIGNAL,
    AbortSignalConstructor, dtConst, True);
end;

function TGocciaGlobalAbort.AbortControllerCall(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Class constructor AbortController cannot be invoked without new');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGlobalAbort.AbortControllerConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Controller: TGocciaAbortControllerValue;
begin
  Controller := TGocciaAbortControllerValue.Create;
  Controller.Prototype := GetProtoFromConstructorWithIntrinsic(
    ANewTarget, FAbortControllerPrototype);
  Result := Controller;
end;

function TGocciaGlobalAbort.AbortSignalCall(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Illegal constructor');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// WHATWG DOM §3.2 AbortSignal.abort(reason).
function TGocciaGlobalAbort.AbortSignalAbort(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Signal: TGocciaAbortSignalValue;
begin
  Signal := TGocciaAbortSignalValue.Create;
  if AArgs.Length > 0 then
    Signal.SignalAbort(AArgs.GetElement(0))
  else
    Signal.SignalAbort;
  Result := Signal;
end;

// WHATWG DOM §3.2 AbortSignal.timeout(milliseconds).
function TGocciaGlobalAbort.AbortSignalTimeout(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Milliseconds: Double;
  Signal: TGocciaAbortSignalValue;
begin
  if AArgs.Length = 0 then
    ThrowTypeError('AbortSignal.timeout requires a timeout');
  Milliseconds := AArgs.GetElement(0).ToNumberLiteral.Value;
  if IsNan(Milliseconds) then
    Milliseconds := 0
  else if IsInfinite(Milliseconds) or (Milliseconds < 0) or
          (Milliseconds >= UNSIGNED_LONG_LONG_BOUND) then
    ThrowTypeError(
      'AbortSignal.timeout must be an unsigned long long integer');
  Milliseconds := Trunc(Milliseconds);

  Signal := TGocciaAbortSignalValue.Create;
  Signal.SetTimeout(Milliseconds);
  Result := Signal;
end;

end.
