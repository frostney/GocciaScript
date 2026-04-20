unit Goccia.Builtins.Globals;

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

threadvar
  GErrorProto: TGocciaObjectValue;
  GTypeErrorProto: TGocciaObjectValue;
  GReferenceErrorProto: TGocciaObjectValue;
  GRangeErrorProto: TGocciaObjectValue;
  GSyntaxErrorProto: TGocciaObjectValue;
  GURIErrorProto: TGocciaObjectValue;
  GAggregateErrorProto: TGocciaObjectValue;
  GSuppressedErrorProto: TGocciaObjectValue;
  GDOMExceptionProto: TGocciaObjectValue;

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
    FSuppressedErrorProto: TGocciaObjectValue;
    FDOMExceptionProto: TGocciaObjectValue;

    function BuildErrorObject(const AName: string; const AProto: TGocciaObjectValue; const AArgs: TGocciaArgumentsCollection): TGocciaObjectValue;
  protected
  published
    function ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SyntaxErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function URIErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AggregateErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SuppressedErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DOMExceptionConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ErrorIsError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StructuredCloneCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function EncodeURICallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DecodeURICallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function EncodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DecodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Classes,
  SysUtils,

  HashMap,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.URI,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
  SyntaxErrorConstructorFunc: TGocciaNativeFunctionValue;
  URIErrorConstructorFunc: TGocciaNativeFunctionValue;
  AggregateErrorConstructorFunc: TGocciaNativeFunctionValue;
  SuppressedErrorConstructorFunc: TGocciaNativeFunctionValue;
  DOMExceptionConstructorFunc: TGocciaNativeFunctionValue;
  ErrorStaticMembers: TArray<TGocciaMemberDefinition>;
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

  FSuppressedErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FSuppressedErrorProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(SUPPRESSED_ERROR_NAME));
  FSuppressedErrorProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));

  FDOMExceptionProto := TGocciaObjectValue.Create(FErrorProto);
  FDOMExceptionProto.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(ERROR_NAME));
  FDOMExceptionProto.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(''));
  FDOMExceptionProto.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(0));

  GErrorProto := FErrorProto;
  GTypeErrorProto := FTypeErrorProto;
  GReferenceErrorProto := FReferenceErrorProto;
  GRangeErrorProto := FRangeErrorProto;
  GSyntaxErrorProto := FSyntaxErrorProto;
  GURIErrorProto := FURIErrorProto;
  GAggregateErrorProto := FAggregateErrorProto;
  GSuppressedErrorProto := FSuppressedErrorProto;
  GDOMExceptionProto := FDOMExceptionProto;

  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, ERROR_NAME, 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, TYPE_ERROR_NAME, 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, REFERENCE_ERROR_NAME, 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, RANGE_ERROR_NAME, 1);
  SyntaxErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SyntaxErrorConstructor, SYNTAX_ERROR_NAME, 1);
  URIErrorConstructorFunc := TGocciaNativeFunctionValue.Create(URIErrorConstructor, URI_ERROR_NAME, 1);
  AggregateErrorConstructorFunc := TGocciaNativeFunctionValue.Create(AggregateErrorConstructor, AGGREGATE_ERROR_NAME, 2);
  SuppressedErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SuppressedErrorConstructor, SUPPRESSED_ERROR_NAME, 3);
  DOMExceptionConstructorFunc := TGocciaNativeFunctionValue.Create(DOMExceptionConstructor, DOM_EXCEPTION_NAME, 2);

  ErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FErrorProto);
  with TGocciaMemberCollection.Create do
  try
    AddNamedMethod('isError', ErrorIsError, 1, gmkStaticMethod);
    ErrorStaticMembers := ToDefinitions;
  finally
    Free;
  end;
  RegisterMemberDefinitions(ErrorConstructorFunc, ErrorStaticMembers);
  TypeErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FTypeErrorProto);
  ReferenceErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FReferenceErrorProto);
  RangeErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FRangeErrorProto);
  SyntaxErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FSyntaxErrorProto);
  URIErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FURIErrorProto);
  AggregateErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FAggregateErrorProto);
  SuppressedErrorConstructorFunc.AssignProperty(PROP_PROTOTYPE, FSuppressedErrorProto);
  DOMExceptionConstructorFunc.AssignProperty(PROP_PROTOTYPE, FDOMExceptionProto);

  AScope.DefineLexicalBinding(ERROR_NAME, ErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(TYPE_ERROR_NAME, TypeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(REFERENCE_ERROR_NAME, ReferenceErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(RANGE_ERROR_NAME, RangeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(SYNTAX_ERROR_NAME, SyntaxErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(URI_ERROR_NAME, URIErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(AGGREGATE_ERROR_NAME, AggregateErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(SUPPRESSED_ERROR_NAME, SuppressedErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding(DOM_EXCEPTION_NAME, DOMExceptionConstructorFunc, dtConst);

  AScope.DefineLexicalBinding('queueMicrotask',
    TGocciaNativeFunctionValue.Create(QueueMicrotaskCallback, 'queueMicrotask', 1), dtConst);

  AScope.DefineLexicalBinding('structuredClone',
    TGocciaNativeFunctionValue.Create(StructuredCloneCallback, 'structuredClone', 1), dtConst);

  AScope.DefineLexicalBinding('encodeURI',
    TGocciaNativeFunctionValue.Create(EncodeURICallback, 'encodeURI', 1), dtConst);

  AScope.DefineLexicalBinding('decodeURI',
    TGocciaNativeFunctionValue.Create(DecodeURICallback, 'decodeURI', 1), dtConst);

  AScope.DefineLexicalBinding('encodeURIComponent',
    TGocciaNativeFunctionValue.Create(EncodeURIComponentCallback, 'encodeURIComponent', 1), dtConst);

  AScope.DefineLexicalBinding('decodeURIComponent',
    TGocciaNativeFunctionValue.Create(DecodeURIComponentCallback, 'decodeURIComponent', 1), dtConst);
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

  { Step 4: InstallErrorCause(O, options) — ES2026 §20.5.8.1 }
  OptionsIndex := 1;
  if AArgs.Length > OptionsIndex then
  begin
    OptionsArg := AArgs.GetElement(OptionsIndex);
    if OptionsArg is TGocciaObjectValue then
    begin
      { Step 4a: If HasProperty(options, "cause"), CreateNonEnumerableDataPropertyOrThrow(O, "cause", cause) }
      if TGocciaObjectValue(OptionsArg).HasProperty(PROP_CAUSE) then
      begin
        CauseValue := OptionsArg.GetProperty(PROP_CAUSE);
        if CauseValue = nil then
          CauseValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        Result.DefineProperty(PROP_CAUSE,
          TGocciaPropertyDescriptorData.Create(CauseValue, [pfConfigurable, pfWritable]));
      end;
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

// ES2026 §20.5.2.1 Error.isError(arg)
function TGocciaGlobals.ErrorIsError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  Arg := AArgs.GetElement(0);

  // ES2026 §20.5.2.1 step 2: If arg has [[ErrorData]], return true
  if (Arg is TGocciaObjectValue) and TGocciaObjectValue(Arg).HasErrorData then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
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

  { Step 4: InstallErrorCause(O, options) — ES2026 §20.5.8.1 }
  if AArgs.Length > 2 then
  begin
    OptionsArg := AArgs.GetElement(2);
    if OptionsArg is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(OptionsArg).HasProperty(PROP_CAUSE) then
      begin
        CauseValue := OptionsArg.GetProperty(PROP_CAUSE);
        if CauseValue = nil then
          CauseValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        ErrorObj.DefineProperty(PROP_CAUSE,
          TGocciaPropertyDescriptorData.Create(CauseValue, [pfConfigurable, pfWritable]));
      end;
    end;
  end;

  { Step 7: Return O }
  Result := ErrorObj;
end;

{ TC39 Explicit Resource Management §6.1 SuppressedError(error, suppressed [, message [, options]])
  1. If NewTarget is undefined, let newTarget be the active function object.
  2. Let O = OrdinaryCreateFromConstructor(newTarget, "%SuppressedError.prototype%").
  3. If message is not undefined, CreateNonEnumerableDataPropertyOrThrow(O, "message", ToString(message)).
  4. CreateNonEnumerableDataPropertyOrThrow(O, "error", error).
  5. CreateNonEnumerableDataPropertyOrThrow(O, "suppressed", suppressed).
  6. Return O. }
function TGocciaGlobals.SuppressedErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  ErrorArg, SuppressedArg: TGocciaValue;
  Message: string;
  OptionsArg, CauseValue: TGocciaValue;
begin
  { Step 4: error argument }
  if AArgs.Length > 0 then
    ErrorArg := AArgs.GetElement(0)
  else
    ErrorArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  { Step 5: suppressed argument }
  if AArgs.Length > 1 then
    SuppressedArg := AArgs.GetElement(1)
  else
    SuppressedArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  { Step 3: If message is not undefined, let msg = ToString(message) }
  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    Message := AArgs.GetElement(2).ToStringLiteral.Value
  else
    Message := '';

  { Step 2: Let O = OrdinaryCreateFromConstructor }
  ErrorObj := CreateErrorObject(SUPPRESSED_ERROR_NAME, Message, 1);
  ErrorObj.Prototype := FSuppressedErrorProto;

  { Step 4: CreateNonEnumerableDataPropertyOrThrow(O, "error", error) }
  ErrorObj.DefineProperty(PROP_ERROR,
    TGocciaPropertyDescriptorData.Create(ErrorArg, [pfConfigurable, pfWritable]));
  { Step 5: CreateNonEnumerableDataPropertyOrThrow(O, "suppressed", suppressed) }
  ErrorObj.DefineProperty(PROP_SUPPRESSED,
    TGocciaPropertyDescriptorData.Create(SuppressedArg, [pfConfigurable, pfWritable]));

  { InstallErrorCause(O, options) — ES2026 §20.5.8.1 }
  if AArgs.Length > 3 then
  begin
    OptionsArg := AArgs.GetElement(3);
    if OptionsArg is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(OptionsArg).HasProperty(PROP_CAUSE) then
      begin
        CauseValue := OptionsArg.GetProperty(PROP_CAUSE);
        if CauseValue = nil then
          CauseValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        ErrorObj.DefineProperty(PROP_CAUSE,
          TGocciaPropertyDescriptorData.Create(CauseValue, [pfConfigurable, pfWritable]));
      end;
    end;
  end;

  { Step 6: Return O }
  Result := ErrorObj;
end;

{ HTML spec §4.3: DOMException(message?, name?)
  1. Let message be the first argument, or "" if not provided.
  2. Let name be the second argument, or "Error" if not provided.
  3. Set this.name to name.
  4. Set this.message to message.
  5. Set this.code to the legacy code for name (e.g. DataCloneError -> 25), or 0. }
function DOMExceptionLegacyCode(const AName: string): Integer;
begin
  if AName = DATA_CLONE_ERROR_NAME then
    Result := 25
  else if AName = INVALID_CHARACTER_ERROR_NAME then
    Result := 5
  else
    Result := 0;
end;

function TGocciaGlobals.DOMExceptionConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message, Name: string;
begin
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Message := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Message := '';

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Name := AArgs.GetElement(1).ToStringLiteral.Value
  else
    Name := ERROR_NAME;

  ErrorObj := CreateErrorObject(Name, Message, 1);
  ErrorObj.HasErrorData := False;
  ErrorObj.Prototype := FDOMExceptionProto;
  ErrorObj.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(DOMExceptionLegacyCode(Name)));

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
    ThrowTypeError(SErrorQueueMicrotaskArgRequired, SSuggestCallbackRequired);

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    ThrowTypeError(SErrorQueueMicrotaskNotFunction, SSuggestCallbackRequired);

  { Step 2: HostEnqueueMicrotask(callback) }
  Task.Handler := Callback;
  Task.ResultPromise := nil;
  Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Task.ReactionType := prtFulfill;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Callback);
  TGocciaMicrotaskQueue.Instance.Enqueue(Task);

  { Step 3: Return undefined }
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ structuredClone(value) — HTML spec §2.7.3 StructuredSerializeInternal / StructuredDeserialize
  Deep-clones a value using the structured clone algorithm. Handles circular
  references via a memory map. Throws DOMException with name "DataCloneError"
  (code 25) for non-serializable types (functions, symbols). }

function StructuredCloneValue(const AValue: TGocciaValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaValue; forward;

function CloneObject(const AObj: TGocciaObjectValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaObjectValue;
var
  I: Integer;
  Keys: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  ClonedValue: TGocciaValue;
begin
  Result := TGocciaObjectValue.Create;
  AMemory.Add(AObj, Result);

  Keys := AObj.GetOwnPropertyKeys;
  for I := 0 to Length(Keys) - 1 do
  begin
    Descriptor := AObj.GetOwnPropertyDescriptor(Keys[I]);
    if not Assigned(Descriptor) then
      Continue;

    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      ClonedValue := StructuredCloneValue(TGocciaPropertyDescriptorData(Descriptor).Value, AMemory);
      Result.DefineProperty(Keys[I],
        TGocciaPropertyDescriptorData.Create(ClonedValue, Descriptor.Flags));
    end
    // HTML spec §2.7.3: accessor properties are read via getter and cloned as data properties
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      ClonedValue := StructuredCloneValue(AObj.GetProperty(Keys[I]), AMemory);
      Result.DefineProperty(Keys[I],
        TGocciaPropertyDescriptorData.Create(ClonedValue, Descriptor.Flags - [pfConfigurable, pfWritable] + [pfEnumerable]));
    end;
  end;
end;

function CloneArray(const AArr: TGocciaArrayValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaArrayValue;
var
  I: Integer;
  Element: TGocciaValue;
begin
  Result := TGocciaArrayValue.Create;
  AMemory.Add(AArr, Result);

  for I := 0 to AArr.Elements.Count - 1 do
  begin
    Element := AArr.Elements[I];
    if Element = TGocciaHoleValue.HoleValue then
      Result.Elements.Add(TGocciaHoleValue.HoleValue)
    else
      Result.Elements.Add(StructuredCloneValue(Element, AMemory));
  end;
end;

function CloneMap(const AMap: TGocciaMapValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaMapValue;
var
  I: Integer;
  Entry: TGocciaMapEntry;
begin
  Result := TGocciaMapValue.Create;
  AMemory.Add(AMap, Result);

  for I := 0 to AMap.Entries.Count - 1 do
  begin
    Entry := AMap.Entries[I];
    Result.SetEntry(
      StructuredCloneValue(Entry.Key, AMemory),
      StructuredCloneValue(Entry.Value, AMemory));
  end;
end;

function CloneSet(const ASet: TGocciaSetValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaSetValue;
var
  I: Integer;
begin
  Result := TGocciaSetValue.Create;
  AMemory.Add(ASet, Result);

  for I := 0 to ASet.Items.Count - 1 do
    Result.AddItem(StructuredCloneValue(ASet.Items[I], AMemory));
end;

function CloneArrayBuffer(const ABuf: TGocciaArrayBufferValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaArrayBufferValue;
var
  Len: Integer;
begin
  Len := Length(ABuf.Data);
  Result := TGocciaArrayBufferValue.Create(Len);
  AMemory.Add(ABuf, Result);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Result);

  if Len > 0 then
    Move(ABuf.Data[0], Result.Data[0], Len);
end;

function CloneSharedArrayBuffer(const ABuf: TGocciaSharedArrayBufferValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaSharedArrayBufferValue;
var
  Len: Integer;
begin
  Len := Length(ABuf.Data);
  Result := TGocciaSharedArrayBufferValue.Create(Len);
  AMemory.Add(ABuf, Result);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Result);

  if Len > 0 then
    Move(ABuf.Data[0], Result.Data[0], Len);
end;

function StructuredCloneValue(const AValue: TGocciaValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaValue;
var
  Existing: TGocciaValue;
begin
  if AValue = nil then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if AValue.IsPrimitive then
    Exit(AValue);

  if AValue is TGocciaSymbolValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [TGocciaSymbolValue(AValue).ToDisplayString.Value]), SSuggestStructuredClone);

  if AValue.IsCallable then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [AValue.ToStringLiteral.Value]), SSuggestStructuredClone);

  if AMemory.TryGetValue(AValue, Existing) then
    Exit(Existing);

  if AValue is TGocciaSharedArrayBufferValue then
    Result := CloneSharedArrayBuffer(TGocciaSharedArrayBufferValue(AValue), AMemory)
  else if AValue is TGocciaArrayBufferValue then
    Result := CloneArrayBuffer(TGocciaArrayBufferValue(AValue), AMemory)
  else if AValue is TGocciaArrayValue then
    Result := CloneArray(TGocciaArrayValue(AValue), AMemory)
  else if AValue is TGocciaMapValue then
    Result := CloneMap(TGocciaMapValue(AValue), AMemory)
  else if AValue is TGocciaSetValue then
    Result := CloneSet(TGocciaSetValue(AValue), AMemory)
  else if AValue is TGocciaObjectValue then
    Result := CloneObject(TGocciaObjectValue(AValue), AMemory)
  else
    ThrowDataCloneError(SErrorStructuredCloneValueNotCloneable, SSuggestStructuredClone);
end;

function TGocciaGlobals.StructuredCloneCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Memory: THashMap<TGocciaValue, TGocciaValue>;
  MemoryValues: array of TGocciaValue;
  I: Integer;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorStructuredCloneArgRequired, SSuggestObjectArgType);

  Memory := THashMap<TGocciaValue, TGocciaValue>.Create;
  try
    Result := StructuredCloneValue(AArgs.GetElement(0), Memory);
  finally
    if Assigned(TGarbageCollector.Instance) then
    begin
      MemoryValues := Memory.Values;
      for I := 0 to Length(MemoryValues) - 1 do
        TGarbageCollector.Instance.RemoveTempRoot(MemoryValues[I]);
    end;
    Memory.Free;
  end;
end;

// ES2026 §19.2.6.2 encodeURI(uriString)
function TGocciaGlobals.EncodeURICallback(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  URIString: string;
begin
  // Step 1: Let uriString be ? ToString(uriString)
  if AArgs.Length = 0 then
    URIString := 'undefined'
  else
    URIString := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 2: Let unescapedURISet be uriReserved + uriUnescaped + "#"
  // Step 3: Return ? Encode(uriString, unescapedURISet)
  Result := TGocciaStringLiteralValue.Create(EncodeURI(URIString));
end;

// ES2026 §19.2.6.3 decodeURI(encodedURI)
function TGocciaGlobals.DecodeURICallback(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  EncodedURI: string;
begin
  // Step 1: Let encodedURI be ? ToString(encodedURI)
  if AArgs.Length = 0 then
    EncodedURI := 'undefined'
  else
    EncodedURI := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 2: Let reservedURISet be uriReserved + "#"
  // Step 3: Return ? Decode(encodedURI, reservedURISet)
  Result := TGocciaStringLiteralValue.Create(DecodeURI(EncodedURI));
end;

// ES2026 §19.2.6.4 encodeURIComponent(uriComponent)
function TGocciaGlobals.EncodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ComponentString: string;
begin
  // Step 1: Let componentString be ? ToString(uriComponent)
  if AArgs.Length = 0 then
    ComponentString := 'undefined'
  else
    ComponentString := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 2: Let unescapedURIComponentSet be uriUnescaped
  // Step 3: Return ? Encode(componentString, unescapedURIComponentSet)
  Result := TGocciaStringLiteralValue.Create(EncodeURIComponent(ComponentString));
end;

// ES2026 §19.2.6.5 decodeURIComponent(encodedURIComponent)
function TGocciaGlobals.DecodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ComponentString: string;
begin
  // Step 1: Let componentString be ? ToString(encodedURIComponent)
  if AArgs.Length = 0 then
    ComponentString := 'undefined'
  else
    ComponentString := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 2: Let reservedURIComponentSet be the empty String
  // Step 3: Return ? Decode(componentString, reservedURIComponentSet)
  Result := TGocciaStringLiteralValue.Create(DecodeURIComponent(ComponentString));
end;

end.
