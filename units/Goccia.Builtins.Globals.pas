unit Goccia.Builtins.Globals;

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
  TGocciaGlobals = class(TGocciaBuiltin)
  private
    FErrorProto: TGocciaObjectValue;
    FTypeErrorProto: TGocciaObjectValue;
    FReferenceErrorProto: TGocciaObjectValue;
    FRangeErrorProto: TGocciaObjectValue;
  protected
    function ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ErrorProto, TypeErrorProto, ReferenceErrorProto, RangeErrorProto: TGocciaObjectValue;
begin
  inherited Create(AName, AScope, AThrowError);

  // Global constants
  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtConst);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtConst);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtConst);

  // Set up Error prototype chain (ECMAScript spec):
  // TypeError.prototype -> Error.prototype -> Object.prototype
  FErrorProto := TGocciaObjectValue.Create;
  FErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('Error'));
  FErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FTypeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FTypeErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('TypeError'));
  FTypeErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FReferenceErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FReferenceErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('ReferenceError'));
  FReferenceErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FRangeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FRangeErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('RangeError'));
  FRangeErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  // Error constructors - store references so we can use them in the constructor functions
  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, 'Error', 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, 'TypeError', 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, 'ReferenceError', 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, 'RangeError', 1);

  // Set .prototype on each constructor (used by instanceof)
  ErrorConstructorFunc.AssignProperty('prototype', FErrorProto);
  TypeErrorConstructorFunc.AssignProperty('prototype', FTypeErrorProto);
  ReferenceErrorConstructorFunc.AssignProperty('prototype', FReferenceErrorProto);
  RangeErrorConstructorFunc.AssignProperty('prototype', FRangeErrorProto);

  AScope.DefineLexicalBinding('Error', ErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('TypeError', TypeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('ReferenceError', ReferenceErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('RangeError', RangeErrorConstructorFunc, dtConst);

  // Global functions
  AScope.DefineLexicalBinding('queueMicrotask',
    TGocciaNativeFunctionValue.Create(QueueMicrotaskCallback, 'queueMicrotask', 1), dtConst);

  // Note: parseInt, parseFloat, isNaN, isFinite are intentionally NOT registered as globals.
  // They are available only on the Number object (e.g. Number.parseInt, Number.isNaN).
  // If needed, they can be polyfilled:
  //   const parseInt = Number.parseInt;
  //   const parseFloat = Number.parseFloat;
  //   const isNaN = Number.isNaN;
  //   const isFinite = Number.isFinite;
end;

function TGocciaGlobals.ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if AArgs.Length > 0 then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('Error', Message);
  ErrorObj.Prototype := FErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if AArgs.Length > 0 then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('TypeError', Message);
  ErrorObj.Prototype := FTypeErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if AArgs.Length > 0 then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('ReferenceError', Message);
  ErrorObj.Prototype := FReferenceErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if AArgs.Length > 0 then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('RangeError', Message);
  ErrorObj.Prototype := FRangeErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Task: TGocciaMicrotask;
begin
  if AArgs.Length = 0 then
    ThrowTypeError('Failed to execute ''queueMicrotask'': 1 argument required, but only 0 present.');

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    ThrowTypeError('Failed to execute ''queueMicrotask'': parameter 1 is not of type ''Function''.');

  Task.Handler := Callback;
  Task.ResultPromise := nil;
  Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Task.ReactionType := prtFulfill;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Callback);
  TGocciaMicrotaskQueue.Instance.Enqueue(Task);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
