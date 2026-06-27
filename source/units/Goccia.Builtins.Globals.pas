unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// Per-realm Error.prototype lookups.  These replace the previous threadvars
// `GErrorProto` / `GTypeErrorProto` / ... so that JS-side mutations on the
// error prototypes don't survive engine teardown on a worker thread.  Each
// returns nil when no realm is active.
function GetErrorProto: TGocciaObjectValue;
function GetEvalErrorProto: TGocciaObjectValue;
function GetTypeErrorProto: TGocciaObjectValue;
function GetReferenceErrorProto: TGocciaObjectValue;
function GetRangeErrorProto: TGocciaObjectValue;
function GetSyntaxErrorProto: TGocciaObjectValue;
function GetURIErrorProto: TGocciaObjectValue;
function GetAggregateErrorProto: TGocciaObjectValue;
function GetSuppressedErrorProto: TGocciaObjectValue;
function GetDOMExceptionProto: TGocciaObjectValue;

type
  TGocciaGlobals = class(TGocciaBuiltin)
  private
    FErrorProto: TGocciaObjectValue;
    FEvalErrorProto: TGocciaObjectValue;
    FTypeErrorProto: TGocciaObjectValue;
    FReferenceErrorProto: TGocciaObjectValue;
    FRangeErrorProto: TGocciaObjectValue;
    FSyntaxErrorProto: TGocciaObjectValue;
    FURIErrorProto: TGocciaObjectValue;
    FAggregateErrorProto: TGocciaObjectValue;
    FSuppressedErrorProto: TGocciaObjectValue;
    FDOMExceptionProto: TGocciaObjectValue;

    function BuildErrorObject(const AName: string; const AProto: TGocciaObjectValue; const AArgs: TGocciaArgumentsCollection): TGocciaObjectValue;
    function BuildAggregateError(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;
    function BuildSuppressedError(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;
    function BuildDOMException(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;

    function ErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function EvalErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function TypeErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function RangeErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function SyntaxErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function URIErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function AggregateErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function SuppressedErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function DOMExceptionConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
  protected
  published
    function ErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function EvalErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SyntaxErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function URIErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AggregateErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SuppressedErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DOMExceptionConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ErrorIsError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ErrorPrototypeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function QueueMicrotaskCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StructuredCloneCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function EncodeURICallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DecodeURICallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function EncodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DecodeURIComponentCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    procedure RegisterUtilityRuntimeGlobals;
  end;

implementation

uses
  Classes,
  SysUtils,

  HashMap,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Realm,
  Goccia.URI,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FinalizationRegistryValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakRefValue,
  Goccia.Values.WeakSetValue;

var
  GErrorProtoSlot: TGocciaRealmSlotId;
  GEvalErrorProtoSlot: TGocciaRealmSlotId;
  GTypeErrorProtoSlot: TGocciaRealmSlotId;
  GReferenceErrorProtoSlot: TGocciaRealmSlotId;
  GRangeErrorProtoSlot: TGocciaRealmSlotId;
  GSyntaxErrorProtoSlot: TGocciaRealmSlotId;
  GURIErrorProtoSlot: TGocciaRealmSlotId;
  GAggregateErrorProtoSlot: TGocciaRealmSlotId;
  GSuppressedErrorProtoSlot: TGocciaRealmSlotId;
  GDOMExceptionProtoSlot: TGocciaRealmSlotId;

function GetErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GErrorProtoSlot))
  else
    Result := nil;
end;

function GetEvalErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GEvalErrorProtoSlot))
  else
    Result := nil;
end;

function GetTypeErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GTypeErrorProtoSlot))
  else
    Result := nil;
end;

function GetReferenceErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GReferenceErrorProtoSlot))
  else
    Result := nil;
end;

function GetRangeErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GRangeErrorProtoSlot))
  else
    Result := nil;
end;

function GetSyntaxErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GSyntaxErrorProtoSlot))
  else
    Result := nil;
end;

function GetURIErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GURIErrorProtoSlot))
  else
    Result := nil;
end;

function GetAggregateErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GAggregateErrorProtoSlot))
  else
    Result := nil;
end;

function GetSuppressedErrorProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GSuppressedErrorProtoSlot))
  else
    Result := nil;
end;

function GetDOMExceptionProto: TGocciaObjectValue;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GDOMExceptionProtoSlot))
  else
    Result := nil;
end;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  EvalErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
  SyntaxErrorConstructorFunc: TGocciaNativeFunctionValue;
  URIErrorConstructorFunc: TGocciaNativeFunctionValue;
  AggregateErrorConstructorFunc: TGocciaNativeFunctionValue;
  SuppressedErrorConstructorFunc: TGocciaNativeFunctionValue;
  DOMExceptionConstructorFunc: TGocciaNativeFunctionValue;
  ErrorStaticMembers: TArray<TGocciaMemberDefinition>;
  ErrorProtoMembers: TArray<TGocciaMemberDefinition>;
begin
  inherited Create(AName, AScope, AThrowError);

  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtConst, True);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtConst, True);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtConst, True);

  FErrorProto := TGocciaObjectValue.Create;
  FErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(ERROR_NAME), [pfConfigurable, pfWritable]));
  FErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));
  with TGocciaMemberCollection.Create do
  try
    AddNamedMethod(PROP_TO_STRING, ErrorPrototypeToString, 0,
      gmkPrototypeMethod, [gmfNotConstructable]);
    ErrorProtoMembers := ToDefinitions;
  finally
    Free;
  end;
  RegisterMemberDefinitions(FErrorProto, ErrorProtoMembers);

  FEvalErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FEvalErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(EVAL_ERROR_NAME), [pfConfigurable, pfWritable]));
  FEvalErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FTypeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FTypeErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(TYPE_ERROR_NAME), [pfConfigurable, pfWritable]));
  FTypeErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FReferenceErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FReferenceErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(REFERENCE_ERROR_NAME), [pfConfigurable, pfWritable]));
  FReferenceErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FRangeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FRangeErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(RANGE_ERROR_NAME), [pfConfigurable, pfWritable]));
  FRangeErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FSyntaxErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FSyntaxErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(SYNTAX_ERROR_NAME), [pfConfigurable, pfWritable]));
  FSyntaxErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FURIErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FURIErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(URI_ERROR_NAME), [pfConfigurable, pfWritable]));
  FURIErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FAggregateErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FAggregateErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(AGGREGATE_ERROR_NAME), [pfConfigurable, pfWritable]));
  FAggregateErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FSuppressedErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FSuppressedErrorProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(SUPPRESSED_ERROR_NAME), [pfConfigurable, pfWritable]));
  FSuppressedErrorProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));

  FDOMExceptionProto := TGocciaObjectValue.Create(FErrorProto);
  FDOMExceptionProto.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(ERROR_NAME), [pfConfigurable, pfWritable]));
  FDOMExceptionProto.DefineProperty(PROP_MESSAGE, TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(''), [pfConfigurable, pfWritable]));
  FDOMExceptionProto.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(0));

  // Publish the per-engine prototypes through the per-realm slot mechanism so
  // ErrorHelper / DisposalTracker / other readers see exactly the prototypes
  // owned by this engine.  When the engine is freed its realm is freed,
  // unpinning these objects in lockstep.
  if Assigned(CurrentRealm) then
  begin
    CurrentRealm.SetSlot(GErrorProtoSlot, FErrorProto);
    CurrentRealm.SetSlot(GEvalErrorProtoSlot, FEvalErrorProto);
    CurrentRealm.SetSlot(GTypeErrorProtoSlot, FTypeErrorProto);
    CurrentRealm.SetSlot(GReferenceErrorProtoSlot, FReferenceErrorProto);
    CurrentRealm.SetSlot(GRangeErrorProtoSlot, FRangeErrorProto);
    CurrentRealm.SetSlot(GSyntaxErrorProtoSlot, FSyntaxErrorProto);
    CurrentRealm.SetSlot(GURIErrorProtoSlot, FURIErrorProto);
    CurrentRealm.SetSlot(GAggregateErrorProtoSlot, FAggregateErrorProto);
    CurrentRealm.SetSlot(GSuppressedErrorProtoSlot, FSuppressedErrorProto);
    CurrentRealm.SetSlot(GDOMExceptionProtoSlot, FDOMExceptionProto);
  end;

  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, ERROR_NAME, 1);
  ErrorConstructorFunc.ConstructCallback := ErrorConstruct;
  EvalErrorConstructorFunc := TGocciaNativeFunctionValue.Create(EvalErrorConstructor, EVAL_ERROR_NAME, 1);
  EvalErrorConstructorFunc.ConstructCallback := EvalErrorConstruct;
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, TYPE_ERROR_NAME, 1);
  TypeErrorConstructorFunc.ConstructCallback := TypeErrorConstruct;
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, REFERENCE_ERROR_NAME, 1);
  ReferenceErrorConstructorFunc.ConstructCallback := ReferenceErrorConstruct;
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, RANGE_ERROR_NAME, 1);
  RangeErrorConstructorFunc.ConstructCallback := RangeErrorConstruct;
  SyntaxErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SyntaxErrorConstructor, SYNTAX_ERROR_NAME, 1);
  SyntaxErrorConstructorFunc.ConstructCallback := SyntaxErrorConstruct;
  URIErrorConstructorFunc := TGocciaNativeFunctionValue.Create(URIErrorConstructor, URI_ERROR_NAME, 1);
  URIErrorConstructorFunc.ConstructCallback := URIErrorConstruct;
  AggregateErrorConstructorFunc := TGocciaNativeFunctionValue.Create(AggregateErrorConstructor, AGGREGATE_ERROR_NAME, 2);
  AggregateErrorConstructorFunc.ConstructCallback := AggregateErrorConstruct;
  SuppressedErrorConstructorFunc := TGocciaNativeFunctionValue.Create(SuppressedErrorConstructor, SUPPRESSED_ERROR_NAME, 3);
  SuppressedErrorConstructorFunc.ConstructCallback := SuppressedErrorConstruct;
  DOMExceptionConstructorFunc := TGocciaNativeFunctionValue.Create(DOMExceptionConstructor, DOM_EXCEPTION_NAME, 2);
  DOMExceptionConstructorFunc.ConstructCallback := DOMExceptionConstruct;

  EvalErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  TypeErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  ReferenceErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  RangeErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  SyntaxErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  URIErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  AggregateErrorConstructorFunc.Prototype := ErrorConstructorFunc;
  SuppressedErrorConstructorFunc.Prototype := ErrorConstructorFunc;

  ErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FErrorProto, []));
  with TGocciaMemberCollection.Create do
  try
    AddNamedMethod('isError', ErrorIsError, 1, gmkStaticMethod);
    ErrorStaticMembers := ToDefinitions;
  finally
    Free;
  end;
  RegisterMemberDefinitions(ErrorConstructorFunc, ErrorStaticMembers);
  EvalErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FEvalErrorProto, []));
  TypeErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FTypeErrorProto, []));
  ReferenceErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FReferenceErrorProto, []));
  RangeErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FRangeErrorProto, []));
  SyntaxErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FSyntaxErrorProto, []));
  URIErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FURIErrorProto, []));
  AggregateErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FAggregateErrorProto, []));
  SuppressedErrorConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FSuppressedErrorProto, []));
  DOMExceptionConstructorFunc.DefineProperty(PROP_PROTOTYPE, TGocciaPropertyDescriptorData.Create(FDOMExceptionProto, []));

  FErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(ErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FEvalErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(EvalErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FTypeErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(TypeErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FReferenceErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(ReferenceErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FRangeErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(RangeErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FSyntaxErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(SyntaxErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FURIErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(URIErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FAggregateErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(AggregateErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FSuppressedErrorProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(SuppressedErrorConstructorFunc, [pfConfigurable, pfWritable]));
  FDOMExceptionProto.DefineProperty(PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(DOMExceptionConstructorFunc, [pfConfigurable, pfWritable]));

  AScope.DefineLexicalBinding(ERROR_NAME, ErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(EVAL_ERROR_NAME, EvalErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(TYPE_ERROR_NAME, TypeErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(REFERENCE_ERROR_NAME, ReferenceErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(RANGE_ERROR_NAME, RangeErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(SYNTAX_ERROR_NAME, SyntaxErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(URI_ERROR_NAME, URIErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(AGGREGATE_ERROR_NAME, AggregateErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(SUPPRESSED_ERROR_NAME, SuppressedErrorConstructorFunc, dtConst, True);
  AScope.DefineLexicalBinding(DOM_EXCEPTION_NAME, DOMExceptionConstructorFunc, dtConst, True);

  AScope.DefineLexicalBinding('encodeURI',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      EncodeURICallback, 'encodeURI', 1), dtConst, True);

  AScope.DefineLexicalBinding('decodeURI',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      DecodeURICallback, 'decodeURI', 1), dtConst, True);

  AScope.DefineLexicalBinding('encodeURIComponent',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      EncodeURIComponentCallback, 'encodeURIComponent', 1), dtConst, True);

  AScope.DefineLexicalBinding('decodeURIComponent',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      DecodeURIComponentCallback, 'decodeURIComponent', 1), dtConst, True);
end;

procedure TGocciaGlobals.RegisterUtilityRuntimeGlobals;
begin
  FScope.DefineLexicalBinding('queueMicrotask',
    TGocciaNativeFunctionValue.Create(QueueMicrotaskCallback, 'queueMicrotask', 1), dtConst, True);

  FScope.DefineLexicalBinding('structuredClone',
    TGocciaNativeFunctionValue.Create(StructuredCloneCallback, 'structuredClone', 1), dtConst, True);
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

procedure InstallErrorCause(const AObj: TGocciaObjectValue; const AOptions: TGocciaValue);
var
  CauseValue: TGocciaValue;
begin
  if AOptions is TGocciaObjectValue then
  begin
    if TGocciaObjectValue(AOptions).HasProperty(PROP_CAUSE) then
    begin
      CauseValue := AOptions.GetProperty(PROP_CAUSE);
      if CauseValue = nil then
        CauseValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      AObj.DefineProperty(PROP_CAUSE,
        TGocciaPropertyDescriptorData.Create(CauseValue, [pfConfigurable, pfWritable]));
    end;
  end;
end;

function TGocciaGlobals.BuildErrorObject(const AName: string; const AProto: TGocciaObjectValue; const AArgs: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  MessageText: string;
  MessageValue: TGocciaValue;
begin
  Result := TGocciaObjectValue.Create(AProto);
  Result.HasErrorData := True;
  MessageText := '';

  if (AArgs.Length > 0) and
     not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
  begin
    MessageText := AArgs.GetElement(0).ToStringLiteral.Value;
    MessageValue := TGocciaStringLiteralValue.Create(
      MessageText);
    Result.DefineProperty(PROP_MESSAGE,
      TGocciaPropertyDescriptorData.Create(MessageValue, [pfConfigurable, pfWritable]));
  end;

  if Assigned(TGocciaCallStack.Instance) then
    Result.DefineProperty(PROP_STACK,
      TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(
          TGocciaCallStack.Instance.CaptureStackTrace(AName, MessageText, 1)),
        [pfConfigurable, pfWritable]));

  if AArgs.Length > 1 then
    InstallErrorCause(Result, AArgs.GetElement(1));
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

// ES2026 §20.5.3.4 Error.prototype.toString()
function TGocciaGlobals.ErrorPrototypeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  NameValue, MsgValue: TGocciaValue;
  Name, Msg: string;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorErrorProtoToStringRequiresObject);

  Obj := TGocciaObjectValue(AThisValue);

  NameValue := Obj.GetProperty(PROP_NAME);
  if (NameValue = nil) or (NameValue is TGocciaUndefinedLiteralValue) then
    Name := ERROR_NAME
  else
    Name := NameValue.ToStringLiteral.Value;

  MsgValue := Obj.GetProperty(PROP_MESSAGE);
  if (MsgValue = nil) or (MsgValue is TGocciaUndefinedLiteralValue) then
    Msg := ''
  else
    Msg := MsgValue.ToStringLiteral.Value;

  if Name = '' then
    Result := TGocciaStringLiteralValue.Create(Msg)
  else if Msg = '' then
    Result := TGocciaStringLiteralValue.Create(Name)
  else
    Result := TGocciaStringLiteralValue.Create(Name + ': ' + Msg);
end;

{ EvalError ( message [ , options ] ) — §20.5.6.1.1 (NativeError) }
function TGocciaGlobals.EvalErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(EVAL_ERROR_NAME, FEvalErrorProto, AArgs);
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
function TGocciaGlobals.BuildAggregateError(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;
var
  Errors: TGocciaValue;
  ErrorsArray: TGocciaArrayValue;
  Message: string;
  I: Integer;
begin
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

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Message := AArgs.GetElement(1).ToStringLiteral.Value
  else
    Message := '';

  Result := CreateErrorObject(AGGREGATE_ERROR_NAME, Message);
  Result.Prototype := AProto;
  Result.AssignProperty(PROP_ERRORS, ErrorsArray);

  if AArgs.Length > 2 then
    InstallErrorCause(Result, AArgs.GetElement(2));
end;

function TGocciaGlobals.AggregateErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildAggregateError(AArgs, FAggregateErrorProto);
end;

{ TC39 Explicit Resource Management §6.1 SuppressedError(error, suppressed [, message [, options]])
  1. If NewTarget is undefined, let newTarget be the active function object.
  2. Let O = OrdinaryCreateFromConstructor(newTarget, "%SuppressedError.prototype%").
  3. If message is not undefined, CreateNonEnumerableDataPropertyOrThrow(O, "message", ToString(message)).
  4. CreateNonEnumerableDataPropertyOrThrow(O, "error", error).
  5. CreateNonEnumerableDataPropertyOrThrow(O, "suppressed", suppressed).
  6. Return O. }
function TGocciaGlobals.BuildSuppressedError(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;
var
  ErrorArg, SuppressedArg: TGocciaValue;
  Message: string;
begin
  if AArgs.Length > 0 then
    ErrorArg := AArgs.GetElement(0)
  else
    ErrorArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    SuppressedArg := AArgs.GetElement(1)
  else
    SuppressedArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    Message := AArgs.GetElement(2).ToStringLiteral.Value
  else
    Message := '';

  Result := CreateErrorObject(SUPPRESSED_ERROR_NAME, Message, 1);
  Result.Prototype := AProto;

  Result.DefineProperty(PROP_ERROR,
    TGocciaPropertyDescriptorData.Create(ErrorArg, [pfConfigurable, pfWritable]));
  Result.DefineProperty(PROP_SUPPRESSED,
    TGocciaPropertyDescriptorData.Create(SuppressedArg, [pfConfigurable, pfWritable]));

  if AArgs.Length > 3 then
    InstallErrorCause(Result, AArgs.GetElement(3));
end;

function TGocciaGlobals.SuppressedErrorConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildSuppressedError(AArgs, FSuppressedErrorProto);
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

function TGocciaGlobals.BuildDOMException(const AArgs: TGocciaArgumentsCollection; const AProto: TGocciaObjectValue): TGocciaObjectValue;
var
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

  Result := CreateErrorObject(Name, Message, 1);
  Result.HasErrorData := False;
  Result.Prototype := AProto;
  Result.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(DOMExceptionLegacyCode(Name)));
end;

function TGocciaGlobals.DOMExceptionConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BuildDOMException(AArgs, FDOMExceptionProto);
end;

{ [[Construct]] callbacks — ES2026 §20.5.1.1 / §20.5.6.1.1
  OrdinaryCreateFromConstructor(newTarget, "%...prototype%") so the instance
  [[Prototype]] honours Reflect.construct / Proxy / class-extends newTarget. }

function TGocciaGlobals.ErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FErrorProto), AArgs);
end;

function TGocciaGlobals.EvalErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(EVAL_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FEvalErrorProto), AArgs);
end;

function TGocciaGlobals.TypeErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(TYPE_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FTypeErrorProto), AArgs);
end;

function TGocciaGlobals.ReferenceErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(REFERENCE_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FReferenceErrorProto), AArgs);
end;

function TGocciaGlobals.RangeErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(RANGE_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FRangeErrorProto), AArgs);
end;

function TGocciaGlobals.SyntaxErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(SYNTAX_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FSyntaxErrorProto), AArgs);
end;

function TGocciaGlobals.URIErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildErrorObject(URI_ERROR_NAME,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FURIErrorProto), AArgs);
end;

function TGocciaGlobals.AggregateErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildAggregateError(AArgs,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FAggregateErrorProto));
end;

function TGocciaGlobals.SuppressedErrorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildSuppressedError(AArgs,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FSuppressedErrorProto));
end;

function TGocciaGlobals.DOMExceptionConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := BuildDOMException(AArgs,
    GetProtoFromConstructorWithIntrinsic(ANewTarget, FDOMExceptionProto));
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
  Cursor: Integer;
  Key, Value: TGocciaValue;
begin
  Result := TGocciaMapValue.Create;
  AMemory.Add(AMap, Result);

  Cursor := 0;
  while AMap.NextEntry(Cursor, Key, Value) do
    Result.SetEntry(
      StructuredCloneValue(Key, AMemory),
      StructuredCloneValue(Value, AMemory));
end;

function CloneSet(const ASet: TGocciaSetValue;
  const AMemory: THashMap<TGocciaValue, TGocciaValue>): TGocciaSetValue;
var
  Cursor: Integer;
  Item: TGocciaValue;
begin
  Result := TGocciaSetValue.Create;
  AMemory.Add(ASet, Result);

  Cursor := 0;
  while ASet.NextItem(Cursor, Item) do
    Result.AddItem(StructuredCloneValue(Item, AMemory));
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

  if AValue is TGocciaSymbolValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [TGocciaSymbolValue(AValue).ToDisplayString.Value]), SSuggestStructuredClone);

  if AValue.IsPrimitive then
    Exit(AValue);

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
  else if AValue is TGocciaWeakMapValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [CONSTRUCTOR_WEAK_MAP]), SSuggestStructuredClone)
  else if AValue is TGocciaWeakSetValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [CONSTRUCTOR_WEAK_SET]), SSuggestStructuredClone)
  else if AValue is TGocciaWeakRefValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [CONSTRUCTOR_WEAK_REF]), SSuggestStructuredClone)
  else if AValue is TGocciaFinalizationRegistryValue then
    ThrowDataCloneError(Format(SErrorStructuredCloneNotCloneable, [CONSTRUCTOR_FINALIZATION_REGISTRY]), SSuggestStructuredClone)
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

initialization
  GErrorProtoSlot := RegisterRealmSlot('Error.prototype');
  GEvalErrorProtoSlot := RegisterRealmSlot('EvalError.prototype');
  GTypeErrorProtoSlot := RegisterRealmSlot('TypeError.prototype');
  GReferenceErrorProtoSlot := RegisterRealmSlot('ReferenceError.prototype');
  GRangeErrorProtoSlot := RegisterRealmSlot('RangeError.prototype');
  GSyntaxErrorProtoSlot := RegisterRealmSlot('SyntaxError.prototype');
  GURIErrorProtoSlot := RegisterRealmSlot('URIError.prototype');
  GAggregateErrorProtoSlot := RegisterRealmSlot('AggregateError.prototype');
  GSuppressedErrorProtoSlot := RegisterRealmSlot('SuppressedError.prototype');
  GDOMExceptionProtoSlot := RegisterRealmSlot('DOMException.prototype');

end.
