unit Goccia.Builtins.GlobalEventTarget;

// WHATWG DOM EventTarget and Event global registration.

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
  TGocciaGlobalEventTarget = class(TGocciaBuiltin)
  private
    FEventTargetConstructor: TGocciaNativeFunctionValue;
    FEventTargetPrototype: TGocciaObjectValue;
    FEventConstructor: TGocciaNativeFunctionValue;
    FEventPrototype: TGocciaObjectValue;
    function EventTargetCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EventTargetConstruct(const AArgs: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
    function EventCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EventConstruct(const AArgs: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);

    property EventTargetConstructor: TGocciaNativeFunctionValue
      read FEventTargetConstructor;
    property EventTargetPrototype: TGocciaObjectValue
      read FEventTargetPrototype;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.EventTargetValue,
  Goccia.Values.EventValue,
  Goccia.Values.FunctionBase;

constructor TGocciaGlobalEventTarget.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FEventTargetConstructor := TGocciaNativeFunctionValue.Create(
    EventTargetCall, CONSTRUCTOR_EVENT_TARGET, 0);
  FEventTargetConstructor.ConstructCallback := EventTargetConstruct;
  TGocciaEventTargetValue.ExposePrototype(FEventTargetConstructor);
  FEventTargetPrototype := TGocciaObjectValue(
    FEventTargetConstructor.GetProperty(PROP_PROTOTYPE));
  AScope.DefineLexicalBinding(CONSTRUCTOR_EVENT_TARGET,
    FEventTargetConstructor, dtConst, True);

  FEventConstructor := TGocciaNativeFunctionValue.Create(
    EventCall, CONSTRUCTOR_EVENT, 1);
  FEventConstructor.ConstructCallback := EventConstruct;
  TGocciaEventValue.ExposePrototype(FEventConstructor);
  FEventPrototype := TGocciaObjectValue(
    FEventConstructor.GetProperty(PROP_PROTOTYPE));
  AScope.DefineLexicalBinding(CONSTRUCTOR_EVENT, FEventConstructor, dtConst,
    True);
end;

function TGocciaGlobalEventTarget.EventTargetCall(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Class constructor EventTarget cannot be invoked without new');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// WHATWG DOM §2.7 new EventTarget().
function TGocciaGlobalEventTarget.EventTargetConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Target: TGocciaEventTargetValue;
begin
  Target := TGocciaEventTargetValue.Create;
  Target.Prototype := GetProtoFromConstructorWithIntrinsic(ANewTarget,
    FEventTargetPrototype);
  Result := Target;
end;

function TGocciaGlobalEventTarget.EventCall(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Class constructor Event cannot be invoked without new');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// WHATWG DOM §2.2 new Event(type, eventInitDict).
function TGocciaGlobalEventTarget.EventConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Event: TGocciaEventValue;
  Options, Member: TGocciaValue;
begin
  if AArgs.Length = 0 then
    ThrowTypeError('Event constructor requires a type');

  Event := TGocciaEventValue.Create;
  Event.Prototype := GetProtoFromConstructorWithIntrinsic(ANewTarget,
    FEventPrototype);
  Event.EventType := AArgs.GetElement(0).ToStringLiteral.Value;

  if AArgs.Length > 1 then
  begin
    Options := AArgs.GetElement(1);
    if Options is TGocciaObjectValue then
    begin
      Member := TGocciaObjectValue(Options).GetProperty(PROP_BUBBLES);
      Event.Bubbles := Assigned(Member) and Member.ToBooleanLiteral.Value;
      Member := TGocciaObjectValue(Options).GetProperty(PROP_CANCELABLE);
      Event.Cancelable := Assigned(Member) and Member.ToBooleanLiteral.Value;
    end;
  end;

  Result := Event;
end;

end.
