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
    function StructuredCloneCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Classes,
  Generics.Collections,

  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SetValue,
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
begin
  inherited Create(AName, AScope, AThrowError);

  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtConst);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtConst);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtConst);

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

  FSyntaxErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FSyntaxErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('SyntaxError'));
  FSyntaxErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FURIErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FURIErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('URIError'));
  FURIErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FAggregateErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FAggregateErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('AggregateError'));
  FAggregateErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, 'Error', 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, 'TypeError', 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, 'ReferenceError', 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, 'RangeError', 1);
  SyntaxErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SyntaxErrorConstructor, 'SyntaxError', 1);
  URIErrorConstructorFunc := TGocciaNativeFunctionValue.Create(URIErrorConstructor, 'URIError', 1);
  AggregateErrorConstructorFunc := TGocciaNativeFunctionValue.Create(AggregateErrorConstructor, 'AggregateError', 2);

  ErrorConstructorFunc.AssignProperty('prototype', FErrorProto);
  TypeErrorConstructorFunc.AssignProperty('prototype', FTypeErrorProto);
  ReferenceErrorConstructorFunc.AssignProperty('prototype', FReferenceErrorProto);
  RangeErrorConstructorFunc.AssignProperty('prototype', FRangeErrorProto);
  SyntaxErrorConstructorFunc.AssignProperty('prototype', FSyntaxErrorProto);
  URIErrorConstructorFunc.AssignProperty('prototype', FURIErrorProto);
  AggregateErrorConstructorFunc.AssignProperty('prototype', FAggregateErrorProto);

  AScope.DefineLexicalBinding('Error', ErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('TypeError', TypeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('ReferenceError', ReferenceErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('RangeError', RangeErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('SyntaxError', SyntaxErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('URIError', URIErrorConstructorFunc, dtConst);
  AScope.DefineLexicalBinding('AggregateError', AggregateErrorConstructorFunc, dtConst);

  AScope.DefineLexicalBinding('queueMicrotask',
    TGocciaNativeFunctionValue.Create(QueueMicrotaskCallback, 'queueMicrotask', 1), dtConst);

  AScope.DefineLexicalBinding('structuredClone',
    TGocciaNativeFunctionValue.Create(StructuredCloneCallback, 'structuredClone', 1), dtConst);
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
      CauseValue := OptionsArg.GetProperty('cause');
      if (CauseValue <> nil) and not (CauseValue is TGocciaUndefinedLiteralValue) then
        Result.AssignProperty('cause', CauseValue);
    end;
  end;
  { Step 5: Return O }
end;

{ Error ( message [ , options ] ) — §20.5.1.1
  Delegates to BuildErrorObject which implements the shared NativeError steps. }
function TGocciaGlobals.ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('Error', FErrorProto, AArgs);
end;

{ TypeError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('TypeError', FTypeErrorProto, AArgs);
end;

{ ReferenceError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('ReferenceError', FReferenceErrorProto, AArgs);
end;

{ RangeError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('RangeError', FRangeErrorProto, AArgs);
end;

{ SyntaxError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.SyntaxErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('SyntaxError', FSyntaxErrorProto, AArgs);
end;

{ URIError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.URIErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject('URIError', FURIErrorProto, AArgs);
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
  ErrorObj := CreateErrorObject('AggregateError', Message);
  ErrorObj.Prototype := FAggregateErrorProto;
  { Step 6: CreateDataPropertyOrThrow(O, "errors", CreateArrayFromList(errorsList)) }
  ErrorObj.AssignProperty('errors', ErrorsArray);

  { Step 4: InstallErrorCause(O, options) }
  if AArgs.Length > 2 then
  begin
    OptionsArg := AArgs.GetElement(2);
    if OptionsArg is TGocciaObjectValue then
    begin
      CauseValue := OptionsArg.GetProperty('cause');
      if (CauseValue <> nil) and not (CauseValue is TGocciaUndefinedLiteralValue) then
        ErrorObj.AssignProperty('cause', CauseValue);
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

{ structuredClone(value) — HTML spec §2.7.3 StructuredSerializeInternal / StructuredDeserialize
  Deep-clones a value using the structured clone algorithm. Handles circular
  references via a memory map. Throws DataCloneError (as a DOMException
  equivalent) for non-serializable types (functions, symbols). }

function StructuredCloneValue(const AValue: TGocciaValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaValue; forward;

function CloneObject(const AObj: TGocciaObjectValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaObjectValue;
var
  I: Integer;
  Keys: TStringList;
  Descriptor: TGocciaPropertyDescriptor;
  ClonedValue: TGocciaValue;
begin
  Result := TGocciaObjectValue.Create;
  AMemory.Add(AObj, Result);

  Keys := AObj.GetOwnPropertyKeys;
  for I := 0 to Keys.Count - 1 do
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
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
      ThrowTypeError('Failed to execute ''structuredClone'': accessor property ''' +
        Keys[I] + ''' could not be cloned.');
  end;
end;

function CloneArray(const AArr: TGocciaArrayValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaArrayValue;
var
  I: Integer;
  Element: TGocciaValue;
begin
  Result := TGocciaArrayValue.Create;
  AMemory.Add(AArr, Result);

  for I := 0 to AArr.Elements.Count - 1 do
  begin
    Element := AArr.Elements[I];
    if Element = nil then
      Result.Elements.Add(nil)
    else
      Result.Elements.Add(StructuredCloneValue(Element, AMemory));
  end;
end;

function CloneMap(const AMap: TGocciaMapValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaMapValue;
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
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaSetValue;
var
  I: Integer;
begin
  Result := TGocciaSetValue.Create;
  AMemory.Add(ASet, Result);

  for I := 0 to ASet.Items.Count - 1 do
    Result.AddItem(StructuredCloneValue(ASet.Items[I], AMemory));
end;

function StructuredCloneValue(const AValue: TGocciaValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaValue;
var
  Existing: TGocciaValue;
begin
  if AValue = nil then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if AValue.IsPrimitive then
    Exit(AValue);

  if AValue is TGocciaSymbolValue then
    ThrowTypeError('Failed to execute ''structuredClone'': ' + AValue.ToStringLiteral.Value + ' could not be cloned.');

  if AValue.IsCallable then
    ThrowTypeError('Failed to execute ''structuredClone'': function could not be cloned.');

  if AMemory.TryGetValue(AValue, Existing) then
    Exit(Existing);

  if AValue is TGocciaArrayValue then
    Result := CloneArray(TGocciaArrayValue(AValue), AMemory)
  else if AValue is TGocciaMapValue then
    Result := CloneMap(TGocciaMapValue(AValue), AMemory)
  else if AValue is TGocciaSetValue then
    Result := CloneSet(TGocciaSetValue(AValue), AMemory)
  else if AValue is TGocciaObjectValue then
    Result := CloneObject(TGocciaObjectValue(AValue), AMemory)
  else
    ThrowTypeError('Failed to execute ''structuredClone'': value could not be cloned.');
end;

function TGocciaGlobals.StructuredCloneCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Memory: TDictionary<TGocciaValue, TGocciaValue>;
begin
  if AArgs.Length = 0 then
    ThrowTypeError('Failed to execute ''structuredClone'': 1 argument required, but only 0 present.');

  Memory := TDictionary<TGocciaValue, TGocciaValue>.Create;
  try
    Result := StructuredCloneValue(AArgs.GetElement(0), Memory);
  finally
    Memory.Free;
  end;
end;

end.
