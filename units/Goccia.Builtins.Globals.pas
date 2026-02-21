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
    FSyntaxErrorProto: TGocciaObjectValue;
    FURIErrorProto: TGocciaObjectValue;
    FAggregateErrorProto: TGocciaObjectValue;

    function BuildErrorObject(const AName: string; const AProto: TGocciaObjectValue; const AArgs: TGocciaArgumentsCollection): TGocciaObjectValue;
  protected
    function ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SyntaxErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function URIErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AggregateErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
  SyntaxErrorConstructorFunc: TGocciaNativeFunctionValue;
  URIErrorConstructorFunc: TGocciaNativeFunctionValue;
  AggregateErrorConstructorFunc: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtConst);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtConst);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtConst);

  FErrorProto := TGocciaObjectValue.Create;
  FErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(ERROR_NAME));
  FErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FTypeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FTypeErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(TYPE_ERROR_NAME));
  FTypeErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FReferenceErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FReferenceErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(REFERENCE_ERROR_NAME));
  FReferenceErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FRangeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FRangeErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(RANGE_ERROR_NAME));
  FRangeErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FSyntaxErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FSyntaxErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(SYNTAX_ERROR_NAME));
  FSyntaxErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FURIErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FURIErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(URI_ERROR_NAME));
  FURIErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FAggregateErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FAggregateErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AGGREGATE_ERROR_NAME));
  FAggregateErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, ERROR_NAME, 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, TYPE_ERROR_NAME, 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, REFERENCE_ERROR_NAME, 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, RANGE_ERROR_NAME, 1);
  SyntaxErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SyntaxErrorConstructor, SYNTAX_ERROR_NAME, 1);
  URIErrorConstructorFunc := TGocciaNativeFunctionValue.Create(URIErrorConstructor, URI_ERROR_NAME, 1);
  AggregateErrorConstructorFunc := TGocciaNativeFunctionValue.Create(AggregateErrorConstructor, AGGREGATE_ERROR_NAME, 2);

  ErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FErrorProto);
  TypeErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FTypeErrorProto);
  ReferenceErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FReferenceErrorProto);
  RangeErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FRangeErrorProto);
  SyntaxErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FSyntaxErrorProto);
  URIErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FURIErrorProto);
  AggregateErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FAggregateErrorProto);

  AScope.DefineLexicalBinding(ERROR_NAME, ErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(TYPE_ERROR_NAME, TypeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(REFERENCE_ERROR_NAME, ReferenceErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(RANGE_ERROR_NAME, RangeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(SYNTAX_ERROR_NAME, SyntaxErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(URI_ERROR_NAME, URIErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(AGGREGATE_ERROR_NAME, AggregateErrorConstructorFunc, dtConst);

  AScope.DefineLexicalBinding('queueMicrotask',
    TGocciaNativeFunctionValue.Create(QueueMicrotaskCallback, 'queueMicrotask', 1), dtConst);
end;

{ NativeError ( message [ , options ] ) — §20.5.6.1.1 (shared by all NativeError constructors)
  1. If NewTarget is undefined, let newTarget be the active function object;
     else let newTarget be NewTarget.
  2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%NativeError.prototype%").
  3. If message is not undefined, then
     a. Let msg be ? ToString(message).
     b. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
  4. Perform ? InstallErrorCause(O, options).
     a. If options has a "cause" property, CreateNonEnumerableDataPropertyOrThrow(O, "cause", cause).
  5. Return O. }
function TGocciaGlobals.BuildErrorObject(const AName: string; const AProto: TGocciaObjectValue; const AArgs: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Message: string;
  OptionsArg, CauseValue: TGocciaValue;
  OptionsIndex: Integer;
begin
  { Step 3: If message is not undefined, let msg = ToString(message) }
  if AArgs.Length > 0 then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';

  { Step 2: Let O = OrdinaryCreateFromConstructor with prototype }
  Result := CreateErrorObject(AName, Message, 1);
  Result.Prototype := AProto;

  { Step 4: InstallErrorCause(O, options) }
  OptionsIndex := 1;
  if AArgs.Length > OptionsIndex then
  begin
    OptionsArg := AArgs.GetElement(OptionsIndex);
    if OptionsArg is TGocciaObjectValue then
    begin
      { Step 4a: If options has "cause", CreateDataPropertyOrThrow(O, "cause", cause) }
      CauseValue := OptionsArg.GetProperty(PROP_CAUSE);
      if (CauseValue <> nil) and not (CauseValue is TGocciaUndefinedLiteralValue) then
        Result.AssignProperty(PROP_CAUSE, CauseValue);
    end;
  end;
  { Step 5: Return O }
end;

{ Error ( message [ , options ] ) — §20.5.1.1
  Delegates to BuildErrorObject which implements the shared NativeError steps. }
function TGocciaGlobals.ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(ERROR_NAME, FErrorProto, AArgs);
end;

{ TypeError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(TYPE_ERROR_NAME, FTypeErrorProto, AArgs);
end;

{ ReferenceError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(REFERENCE_ERROR_NAME, FReferenceErrorProto, AArgs);
end;

{ RangeError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(RANGE_ERROR_NAME, FRangeErrorProto, AArgs);
end;

{ SyntaxError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.SyntaxErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(SYNTAX_ERROR_NAME, FSyntaxErrorProto, AArgs);
end;

{ URIError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.URIErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(URI_ERROR_NAME, FURIErrorProto, AArgs);
end;

{ AggregateError ( errors, message [ , options ] ) — §20.5.7.1
  1. If NewTarget is undefined, let newTarget be the active function object;
     else let newTarget be NewTarget.
  2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%AggregateError.prototype%").
  3. If message is not undefined, then
     a. Let msg be ? ToString(message).
     b. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
  4. Perform ? InstallErrorCause(O, options).
  5. Let errorsList be ? IterableToList(errors).
  6. Perform ! DefinePropertyOrThrow(O, "errors",
     PropertyDescriptor [[Value]]: CreateArrayFromList(errorsList)).
  7. Return O. }
function TGocciaGlobals.AggregateErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Errors: TGocciaValue;
  ErrorsArray: TGocciaArrayValue;
  OptionsArg, CauseValue: TGocciaValue;
  Message: string;
  I: Integer;
begin
  { Step 5: Let errorsList = IterableToList(errors) }
  ErrorsArray := TGocciaArrayValue.Create;
  if AArgs.Length > 0 then
  begin
    Errors := AArgs.GetElement(0);
    if Errors is TGocciaArrayValue then
    begin
      for I := 0 to TGocciaArrayValue(Errors).Elements.Count - 1 do
        ErrorsArray.Elements.Add(TGocciaArrayValue(Errors).Elements[I]);
    end;
  end;

  { Step 3: If message is not undefined, let msg = ToString(message) }
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Message := AArgs.GetElement(1).ToStringLiteral.Value
  else
    Message := '';

  { Step 2: Let O = OrdinaryCreateFromConstructor }
  ErrorObj := CreateErrorObject(AGGREGATE_ERROR_NAME, Message);
  ErrorObj.Prototype := FAggregateErrorProto;
  { Step 6: CreateDataPropertyOrThrow(O, "errors", CreateArrayFromList(errorsList)) }
  ErrorObj.AssignProperty(PROP_ERRORS, ErrorsArray);

  { Step 4: InstallErrorCause(O, options) }
  if AArgs.Length > 2 then
  begin
    OptionsArg := AArgs.GetElement(2);
    if OptionsArg is TGocciaObjectValue then
    begin
      CauseValue := OptionsArg.GetProperty(PROP_CAUSE);
      if (CauseValue <> nil) and not (CauseValue is TGocciaUndefinedLiteralValue) then
        ErrorObj.AssignProperty(PROP_CAUSE, CauseValue);
    end;
  end;

  { Step 7: Return O }
  Result := ErrorObj;
end;

{ queueMicrotask ( callback ) — §27.8.1 (HTML spec / §8.4 HostEnqueueGenericJob)
  1. If IsCallable(callback) is false, throw a TypeError exception.
  2. Perform HostEnqueueMicrotask(callback).
  3. Return undefined. }
function TGocciaGlobals.QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Task: TGocciaMicrotask;
begin
  { Step 1: If IsCallable(callback) is false, throw a TypeError }
  if AArgs.Length = 0 then
    ThrowTypeError('Failed to execute ''queueMicrotask'': 1 argument required, but only 0 present.');

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    ThrowTypeError('Failed to execute ''queueMicrotask'': parameter 1 is not of type ''Function''.');

  { Step 2: HostEnqueueMicrotask(callback) }
  Task.Handler := Callback;
  Task.ResultPromise := nil;
  Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Task.ReactionType := prtFulfill;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Callback);
  TGocciaMicrotaskQueue.Instance.Enqueue(Task);

  { Step 3: Return undefined }
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
