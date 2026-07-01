unit Goccia.Values.IteratorValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIteratorValue = class(TGocciaObjectValue)
  protected
    class function EnsureConcreteIteratorPrototype(
      const ASlotId: TGocciaRealmSlotId;
      const AToStringTag: string): TGocciaObjectValue; static;
  public
    function IteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorDispose(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorHelperReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function IteratorMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorTake(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorDrop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorToArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorSome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorZip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorZipKeyed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorConstructorCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function IteratorGetConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorSetConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorGetToStringTag(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorSetToStringTag(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    class procedure EnsurePrototypeInitialized;
    class function SharedPrototype: TGocciaObjectValue; static;
    procedure InitializePrototype;
  protected
    FDone: Boolean;
  public
    constructor Create;
    function AdvanceNext: TGocciaObjectValue; virtual;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; virtual;
    function AdvanceNextResultValue(const AValue: TGocciaValue): TGocciaObjectValue; virtual;
    function DirectNext(out ADone: Boolean): TGocciaValue; virtual;
    function DirectNextValue(const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue; virtual;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; virtual;
    function ThrowValue(const AValue: TGocciaValue): TGocciaObjectValue; virtual;
    procedure Close; virtual;
    function ToStringTag: string; override;

    class function CreateGlobalObject: TGocciaObjectValue;
  end;

  TGocciaIteratorHelperValue = class(TGocciaIteratorValue)
  private
    FExecuting: Boolean;
    FDirectResultRoot: TGocciaValue;
    FDirectResultRooted: Boolean;
    procedure ClearDirectResultRoot;
    procedure RootDirectResult(const AValue: TGocciaValue);
  protected
    function DoAdvanceNext: TGocciaObjectValue; virtual; abstract;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    procedure Close; override;
  end;

function CreateIteratorResult(const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
function IteratorResultDone(const AIterResult: TGocciaObjectValue): Boolean;
function IteratorResultValue(const AIterResult: TGocciaObjectValue): TGocciaValue;
function InvokeIteratorCallback(const ACallback: TGocciaValue; const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;
procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concat,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.Iterator.Lazy,
  Goccia.Values.Iterator.Zip,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

// Iterator.prototype, its helper prototype, the Iterator constructor, and the
// method host all live in per-realm slots, so the realm pins them on SetSlot and
// unpins them on Destroy.  The method host (Self in InitializePrototype) backs
// the prototype, helper, constructor, and static methods; the member definitions
// are rebuilt per realm because they bind to this realm's host. #892
var
  GIteratorPrototypeSlot: TGocciaRealmSlotId;
  GIteratorHelperPrototypeSlot: TGocciaRealmSlotId;
  GIteratorConstructorSlot: TGocciaRealmSlotId;
  GIteratorMethodHostSlot: TGocciaRealmSlotId;

function GetSharedIteratorPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GIteratorPrototypeSlot))
  else
    Result := nil;
end;

function GetSharedIteratorConstructor: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GIteratorConstructorSlot))
  else
    Result := nil;
end;

function GetSharedIteratorHelperPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GIteratorHelperPrototypeSlot))
  else
    Result := nil;
end;

// The per-realm method host (Self from InitializePrototype) that the helper,
// constructor, and static methods bind to.  Populated by InitializePrototype,
// which every entry point calls via EnsurePrototypeInitialized first. #892
function GetIteratorMethodHost: TGocciaIteratorValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaIteratorValue(CurrentRealm.GetSlot(GIteratorMethodHostSlot))
  else
    Result := nil;
end;

function IteratorThisToDirectIterator(
  const AThisValue: TGocciaValue; const AMethodName: string): TGocciaIteratorValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(Format('Iterator.prototype.%s called on non-object', [AMethodName]));

  // ES2026 §7.4.2 GetIteratorDirect(obj): capture "next" once.  The
  // generic iterator reports a missing/non-callable next on first use.
  Result := CreateRootedGenericIterator(AThisValue);
end;

function IteratorThisHasCallableReturn(const AThisValue: TGocciaValue): Boolean;
var
  ReturnMethod: TGocciaValue;
begin
  Result := False;
  if not (AThisValue is TGocciaObjectValue) then
    Exit;
  ReturnMethod := TGocciaObjectValue(AThisValue).GetProperty(PROP_RETURN);
  Result := Assigned(ReturnMethod) and
    not (ReturnMethod is TGocciaUndefinedLiteralValue) and
    not (ReturnMethod is TGocciaNullLiteralValue) and
    ReturnMethod.IsCallable;
end;

procedure CloseIteratorThisPreservingError(const AThisValue: TGocciaValue);
var
  Iterator: TGocciaIteratorValue;
begin
  if AThisValue is TGocciaIteratorValue then
  begin
    CloseIteratorPreservingError(TGocciaIteratorValue(AThisValue));
    Exit;
  end;

  if not IteratorThisHasCallableReturn(AThisValue) then
    Exit;

  Iterator := CreateRootedGenericIterator(AThisValue,
    TGocciaUndefinedLiteralValue.UndefinedValue);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    CloseIteratorPreservingError(Iterator);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function IteratorCallbackArgOrClose(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  const AErrorMessage, ASuggestion: string): TGocciaValue;
begin
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
  begin
    CloseIteratorThisPreservingError(AThisValue);
    ThrowTypeError(AErrorMessage, ASuggestion);
  end;
  Result := AArgs.GetElement(0);
end;

function IteratorLimitArgOrClose(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  const AMissingMessage, ANegativeMessage: string): Integer;
var
  NumArg: TGocciaNumberLiteralValue;
begin
  if AArgs.Length < 1 then
  begin
    CloseIteratorThisPreservingError(AThisValue);
    ThrowRangeError(AMissingMessage, SSuggestIteratorNonNegative);
  end;

  try
    NumArg := AArgs.GetElement(0).ToNumberLiteral;
  except
    CloseIteratorThisPreservingError(AThisValue);
    raise;
  end;
  if NumArg.IsNaN then
  begin
    CloseIteratorThisPreservingError(AThisValue);
    ThrowRangeError(ANegativeMessage, SSuggestIteratorNonNegative);
  end;

  Result := ToIntegerValue(NumArg);
  if Result < 0 then
  begin
    CloseIteratorThisPreservingError(AThisValue);
    ThrowRangeError(ANegativeMessage, SSuggestIteratorNonNegative);
  end;
end;

procedure SetterThatIgnoresPrototypeStringProperty(
  const AThisValue: TGocciaValue; const AHome: TGocciaObjectValue;
  const APropertyKey: string; const AValue: TGocciaValue);
begin
  // ES2026 §7.3.37 SetterThatIgnoresPrototypeProperties(thisValue, home, propertyKey, value)
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('Iterator accessor setter requires an object receiver');
  if AThisValue = AHome then
    ThrowTypeError('Cannot assign Iterator intrinsic accessor on its home object');
  if not Assigned(TGocciaObjectValue(AThisValue).GetOwnPropertyDescriptor(APropertyKey)) then
    TGocciaObjectValue(AThisValue).CreateDataPropertyOrThrow(APropertyKey, AValue)
  else
    TGocciaObjectValue(AThisValue).AssignProperty(APropertyKey, AValue);
end;

procedure SetterThatIgnoresPrototypeSymbolProperty(
  const AThisValue: TGocciaValue; const AHome: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
begin
  // ES2026 §7.3.37 SetterThatIgnoresPrototypeProperties(thisValue, home, propertyKey, value)
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('Iterator accessor setter requires an object receiver');
  if AThisValue = AHome then
    ThrowTypeError('Cannot assign Iterator intrinsic accessor on its home object');
  if not Assigned(TGocciaObjectValue(AThisValue).GetOwnSymbolPropertyDescriptor(ASymbol)) then
    TGocciaObjectValue(AThisValue).CreateDataPropertyOrThrow(ASymbol, AValue)
  else
    TGocciaObjectValue(AThisValue).AssignSymbolProperty(ASymbol, AValue);
end;

procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);
begin
  if not Assigned(AIterator) then
    Exit;
  try
    AIterator.Close;
  except
    // Preserve the original abrupt-completion error when cleanup also throws.
  end;
end;

procedure EnsureIteratorHelperPrototypeInitialized;
var
  Members: TGocciaMemberCollection;
  HelperPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then
    Exit;
  if Assigned(GetSharedIteratorHelperPrototype) then
    Exit;

  TGocciaIteratorValue.EnsurePrototypeInitialized;
  HelperPrototype := TGocciaObjectValue.Create(GetSharedIteratorPrototype);
  CurrentRealm.SetSlot(GIteratorHelperPrototypeSlot, HelperPrototype);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('next', GetIteratorMethodHost.IteratorNext, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_RETURN, GetIteratorMethodHost.IteratorHelperReturn, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Iterator Helper'), [pfConfigurable]);
    RegisterMemberDefinitions(HelperPrototype, Members.ToDefinitions);
  finally
    Members.Free;
  end;
end;

function CreateIteratorResult(const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
var
  GC: TGarbageCollector;
  ValueWasRooted, ResultWasRooted: Boolean;
begin
  GC := TGarbageCollector.Instance;
  ValueWasRooted := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
  if ValueWasRooted then
    GC.AddTempRoot(AValue);
  try
    Result := TGocciaObjectValue.Create;
    ResultWasRooted := Assigned(GC) and not GC.IsTempRoot(Result);
    if ResultWasRooted then
      GC.AddTempRoot(Result);
    try
      Result.DefineProperty(PROP_VALUE, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
      if ADone then
        Result.DefineProperty(PROP_DONE, TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]))
      else
        Result.DefineProperty(PROP_DONE, TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.FalseValue, [pfEnumerable, pfConfigurable, pfWritable]));
    finally
      if ResultWasRooted then
        GC.RemoveTempRoot(Result);
    end;
  finally
    if ValueWasRooted then
      GC.RemoveTempRoot(AValue);
  end;
end;

function IteratorResultDone(const AIterResult: TGocciaObjectValue): Boolean;
var
  GC: TGarbageCollector;
  IterResultWasRooted: Boolean;
  DoneValue: TGocciaValue;
begin
  GC := TGarbageCollector.Instance;
  IterResultWasRooted := Assigned(GC) and Assigned(AIterResult) and
    not GC.IsTempRoot(AIterResult);
  if IterResultWasRooted then
    GC.AddTempRoot(AIterResult);
  try
    DoneValue := AIterResult.GetProperty(PROP_DONE);
    Result := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
  finally
    if IterResultWasRooted then
      GC.RemoveTempRoot(AIterResult);
  end;
end;

function IteratorResultValue(const AIterResult: TGocciaObjectValue): TGocciaValue;
var
  GC: TGarbageCollector;
  IterResultWasRooted: Boolean;
begin
  GC := TGarbageCollector.Instance;
  IterResultWasRooted := Assigned(GC) and Assigned(AIterResult) and
    not GC.IsTempRoot(AIterResult);
  if IterResultWasRooted then
    GC.AddTempRoot(AIterResult);
  try
    Result := AIterResult.GetProperty(PROP_VALUE);
  finally
    if IterResultWasRooted then
      GC.RemoveTempRoot(AIterResult);
  end;
end;

function InvokeIteratorCallback(const ACallback: TGocciaValue; const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  IndexValue: TGocciaValue;
  GC: TGarbageCollector;
  CallbackWasRooted, ValueWasRooted, IndexWasRooted: Boolean;
begin
  GC := TGarbageCollector.Instance;
  CallbackWasRooted := Assigned(GC) and Assigned(ACallback) and not GC.IsTempRoot(ACallback);
  if CallbackWasRooted then
    GC.AddTempRoot(ACallback);
  ValueWasRooted := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
  if ValueWasRooted then
    GC.AddTempRoot(AValue);
  IndexValue := TGocciaNumberLiteralValue.Create(AIndex);
  IndexWasRooted := Assigned(GC) and not GC.IsTempRoot(IndexValue);
  if IndexWasRooted then
    GC.AddTempRoot(IndexValue);
  try
    CallArgs := TGocciaArgumentsCollection.Create([AValue, IndexValue]);
    try
      Result := InvokeCallable(ACallback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  finally
    if IndexWasRooted then
      GC.RemoveTempRoot(IndexValue);
    if ValueWasRooted then
      GC.RemoveTempRoot(AValue);
    if CallbackWasRooted then
      GC.RemoveTempRoot(ACallback);
  end;
end;

function AddTempRootIfNeeded(const AValue: TGocciaValue): Boolean;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Result := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
  if Result then
    GC.AddTempRoot(AValue);
end;

procedure RemoveTempRootIfNeeded(
  const AValue: TGocciaValue; const AWasRooted: Boolean);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if AWasRooted and Assigned(GC) then
    GC.RemoveTempRoot(AValue);
end;

{ TGocciaIteratorValue }

constructor TGocciaIteratorValue.Create;
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(nil);
  FDone := False;
  InitializePrototype;
  SharedPrototype := GetSharedIteratorPrototype;
  if Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
end;

function TGocciaIteratorValue.AdvanceNext: TGocciaObjectValue;
begin
  FDone := True;
  Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
end;

function TGocciaIteratorValue.AdvanceNextValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := AdvanceNext;
end;

function TGocciaIteratorValue.AdvanceNextResultValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := AdvanceNextValue(AValue);
end;

function TGocciaIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  IterResult: TGocciaObjectValue;
begin
  IterResult := AdvanceNext;
  ADone := IteratorResultDone(IterResult);
  if ADone then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := IteratorResultValue(IterResult);
end;

function TGocciaIteratorValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
var
  IterResult: TGocciaObjectValue;
begin
  IterResult := AdvanceNextValue(AValue);
  ADone := IteratorResultDone(IterResult);
  if ADone then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := IteratorResultValue(IterResult);
end;

function TGocciaIteratorValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Close;
  Result := CreateIteratorResult(AValue, True);
end;

function TGocciaIteratorValue.ThrowValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Close;
  ThrowTypeError('Delegated iterator has no throw method');
  Result := nil;
end;

procedure TGocciaIteratorValue.Close;
begin
end;

function TGocciaIteratorValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_ITERATOR;
end;

class procedure TGocciaIteratorValue.EnsurePrototypeInitialized;
begin
  if Assigned(GetSharedIteratorPrototype) then Exit;
  TGocciaIteratorValue.Create;
end;

class function TGocciaIteratorValue.SharedPrototype: TGocciaObjectValue;
begin
  EnsurePrototypeInitialized;
  Result := GetSharedIteratorPrototype;
end;

class function TGocciaIteratorValue.EnsureConcreteIteratorPrototype(
  const ASlotId: TGocciaRealmSlotId;
  const AToStringTag: string): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then
    Exit(nil);

  Result := TGocciaObjectValue(CurrentRealm.GetSlot(ASlotId));
  if Assigned(Result) then
    Exit;

  EnsurePrototypeInitialized;
  IteratorPrototype := GetSharedIteratorPrototype;
  Result := TGocciaObjectValue.Create(IteratorPrototype);
  Result.DefineProperty('next',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        GetIteratorMethodHost.IteratorNext, 'next', 0),
      [pfConfigurable, pfWritable]));
  Result.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(AToStringTag), [pfConfigurable]));
  CurrentRealm.SetSlot(ASlotId, Result);
end;

procedure TGocciaIteratorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedIteratorPrototype) then Exit;

  SharedPrototype := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  CurrentRealm.SetSlot(GIteratorPrototypeSlot, SharedPrototype);
  // The native methods below bind to this instance (Self); keep it alive for the
  // realm's lifetime in its own slot so the realm unpins it on Destroy.  The
  // helper, constructor, and static setup read it back via GetIteratorMethodHost.
  // #892
  CurrentRealm.SetSlot(GIteratorMethodHostSlot, Self);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('next', IteratorNext, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddAccessor(PROP_CONSTRUCTOR, IteratorGetConstructor,
      IteratorSetConstructor, [pfConfigurable], gmkPrototypeGetter);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownIterator, '[Symbol.iterator]',
      IteratorSelf, 0, [pfConfigurable, pfWritable]);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownDispose, '[Symbol.dispose]',
      IteratorDispose, 0, [pfConfigurable, pfWritable],
      [gmfNoFunctionPrototype]);
    Members.AddSymbolAccessor(
      TGocciaSymbolValue.WellKnownToStringTag, '[Symbol.toStringTag]',
      IteratorGetToStringTag, IteratorSetToStringTag,
      [pfConfigurable]);
    Members.AddNamedMethod('map', IteratorMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('filter', IteratorFilter, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('take', IteratorTake, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('drop', IteratorDrop, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('forEach', IteratorForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('reduce', IteratorReduce, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('toArray', IteratorToArray, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('some', IteratorSome, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('every', IteratorEvery, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('find', IteratorFind, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('flatMap', IteratorFlatMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(SharedPrototype, PrototypeMembers);
end;

class function TGocciaIteratorValue.CreateGlobalObject: TGocciaObjectValue;
var
  Members: TGocciaMemberCollection;
  IteratorConstructor: TGocciaNativeFunctionValue;
  SharedPrototype: TGocciaObjectValue;
  StaticMembers: TArray<TGocciaMemberDefinition>;
begin
  EnsurePrototypeInitialized;

  if Assigned(GetSharedIteratorConstructor) then
    Exit(GetSharedIteratorConstructor);

  IteratorConstructor := TGocciaNativeFunctionValue.Create(
    GetIteratorMethodHost.IteratorConstructorCall, CONSTRUCTOR_ITERATOR, 0);
  IteratorConstructor.ConstructCallback := GetIteratorMethodHost.IteratorConstruct;
  if Assigned(CurrentRealm) then
    CurrentRealm.SetSlot(GIteratorConstructorSlot, IteratorConstructor);

  Result := IteratorConstructor;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('from', GetIteratorMethodHost.IteratorFrom, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('concat', GetIteratorMethodHost.IteratorConcat, 0, gmkStaticMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('zip', GetIteratorMethodHost.IteratorZip, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('zipKeyed', GetIteratorMethodHost.IteratorZipKeyed, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Result, StaticMembers);
  SharedPrototype := GetSharedIteratorPrototype;
  if Assigned(SharedPrototype) then
    Result.DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(SharedPrototype, []));
end;

// ES2026 §27.1.3.1.1 Iterator()
function TGocciaIteratorValue.IteratorConstructorCall(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(SErrorIllegalConstructor);
  Result := nil;
end;

// ES2026 §27.1.3.1.1 Iterator()
function TGocciaIteratorValue.IteratorConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  IteratorConstructor: TGocciaObjectValue;
  SharedPrototype: TGocciaObjectValue;
begin
  IteratorConstructor := GetSharedIteratorConstructor;
  if (not Assigned(ANewTarget)) or (ANewTarget = IteratorConstructor) then
    ThrowTypeError(SErrorIllegalConstructor);

  SharedPrototype := GetSharedIteratorPrototype;
  Result := TGocciaObjectValue.Create(
    GetProtoFromConstructorWithIntrinsic(ANewTarget, SharedPrototype));
end;

// ES2026 §27.1.3.3.1.1 get Iterator.prototype.constructor
function TGocciaIteratorValue.IteratorGetConstructor(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := GetSharedIteratorConstructor;
  if not Assigned(Result) then
    Result := TGocciaIteratorValue.CreateGlobalObject;
end;

// ES2026 §27.1.3.3.1.2 set Iterator.prototype.constructor
function TGocciaIteratorValue.IteratorSetConstructor(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  SetterThatIgnoresPrototypeStringProperty(AThisValue,
    GetSharedIteratorPrototype, PROP_CONSTRUCTOR, Value);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §27.1.3.3.14.1 get Iterator.prototype [ %Symbol.toStringTag% ]
function TGocciaIteratorValue.IteratorGetToStringTag(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(CONSTRUCTOR_ITERATOR);
end;

// ES2026 §27.1.3.3.14.2 set Iterator.prototype [ %Symbol.toStringTag% ]
function TGocciaIteratorValue.IteratorSetToStringTag(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  SetterThatIgnoresPrototypeSymbolProperty(AThisValue,
    GetSharedIteratorPrototype, TGocciaSymbolValue.WellKnownToStringTag,
    Value);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ Protocol methods }

function TGocciaIteratorValue.IteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorNextNonIterator, SSuggestIteratorThisType);
  Result := TGocciaIteratorValue(AThisValue).AdvanceNext;
end;

function TGocciaIteratorValue.IteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

// ES2026 §27.1.3.3.2 Iterator.prototype [ %Symbol.dispose% ] ()
function TGocciaIteratorValue.IteratorDispose(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ReturnMethod: TGocciaValue;
  ReturnResult: TGocciaValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('Iterator.prototype[Symbol.dispose] called on non-object');

  ReturnMethod := TGocciaObjectValue(AThisValue).GetProperty(PROP_RETURN);
  if Assigned(ReturnMethod) and
     not (ReturnMethod is TGocciaUndefinedLiteralValue) and
     not (ReturnMethod is TGocciaNullLiteralValue) then
  begin
    if not ReturnMethod.IsCallable then
      ThrowTypeError(SErrorIteratorReturnMustBeCallable,
        SSuggestIteratorProtocol);
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      ReturnResult := InvokeCallable(ReturnMethod, CallArgs, AThisValue);
      if not (ReturnResult is TGocciaObjectValue) then
        ThrowTypeError(SErrorIteratorReturnObject, SSuggestIteratorResultObject);
    finally
      CallArgs.Free;
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §27.1.5.2.2 %IteratorHelperPrototype%.return()
function TGocciaIteratorValue.IteratorHelperReturn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorHelperValue) then
    ThrowTypeError('Iterator helper return called on non-helper');
  Result := TGocciaIteratorHelperValue(AThisValue).ReturnValue(
    TGocciaUndefinedLiteralValue.UndefinedValue);
end;

{ Lazy helper methods — return new lazy iterators }

function TGocciaIteratorValue.IteratorMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GC: TGarbageCollector;
  CallbackWasRooted, IteratorWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorMapNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorMapCallable, SSuggestIteratorCallable);

  GC := TGarbageCollector.Instance;
  CallbackWasRooted := Assigned(GC) and Assigned(Callback) and not GC.IsTempRoot(Callback);
  if CallbackWasRooted then
    GC.AddTempRoot(Callback);
  try
    Iterator := IteratorThisToDirectIterator(AThisValue, 'map');
    IteratorWasRooted := Assigned(GC) and Assigned(Iterator) and not GC.IsTempRoot(Iterator);
    if IteratorWasRooted then
      GC.AddTempRoot(Iterator);
    try
      Result := TGocciaLazyMapIteratorValue.Create(
        Iterator, Callback
      );
    finally
      if IteratorWasRooted then
        GC.RemoveTempRoot(Iterator);
    end;
  finally
    if CallbackWasRooted then
      GC.RemoveTempRoot(Callback);
  end;
end;

function TGocciaIteratorValue.IteratorFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GC: TGarbageCollector;
  CallbackWasRooted, IteratorWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorFilterNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorFilterCallable, SSuggestIteratorCallable);

  GC := TGarbageCollector.Instance;
  CallbackWasRooted := Assigned(GC) and Assigned(Callback) and not GC.IsTempRoot(Callback);
  if CallbackWasRooted then
    GC.AddTempRoot(Callback);
  try
    Iterator := IteratorThisToDirectIterator(AThisValue, 'filter');
    IteratorWasRooted := Assigned(GC) and Assigned(Iterator) and not GC.IsTempRoot(Iterator);
    if IteratorWasRooted then
      GC.AddTempRoot(Iterator);
    try
      Result := TGocciaLazyFilterIteratorValue.Create(
        Iterator, Callback
      );
    finally
      if IteratorWasRooted then
        GC.RemoveTempRoot(Iterator);
    end;
  finally
    if CallbackWasRooted then
      GC.RemoveTempRoot(Callback);
  end;
end;

function TGocciaIteratorValue.IteratorTake(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Limit: Integer;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorTakeNonIterator, SSuggestIteratorThisType);

  // ES2026 §27.1.3.3.11 steps 4-7: NaN and negative (incl. -∞) limits throw
  // RangeError; +∞ takes everything (saturated to MaxInt here).
  Limit := IteratorLimitArgOrClose(AArgs, AThisValue,
    SErrorIteratorTakeRequiresArg, SErrorIteratorTakeNonNegative);
  Iterator := IteratorThisToDirectIterator(AThisValue, 'take');

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    Result := TGocciaLazyTakeIteratorValue.Create(
      Iterator, Limit
    );
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorDrop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  DropCount: Integer;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorDropNonIterator, SSuggestIteratorThisType);

  // ES2026 §27.1.3.3.3 steps 4-7: NaN and negative (incl. -∞) counts throw
  // RangeError; +∞ drops everything (saturated to MaxInt here).
  DropCount := IteratorLimitArgOrClose(AArgs, AThisValue,
    SErrorIteratorDropRequiresArg, SErrorIteratorDropNonNegative);
  Iterator := IteratorThisToDirectIterator(AThisValue, 'drop');

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    Result := TGocciaLazyDropIteratorValue.Create(
      Iterator, DropCount
    );
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GC: TGarbageCollector;
  CallbackWasRooted, IteratorWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorFlatMapNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorFlatMapCallable, SSuggestIteratorFlatMapCallable);

  GC := TGarbageCollector.Instance;
  CallbackWasRooted := Assigned(GC) and Assigned(Callback) and not GC.IsTempRoot(Callback);
  if CallbackWasRooted then
    GC.AddTempRoot(Callback);
  try
    Iterator := IteratorThisToDirectIterator(AThisValue, 'flatMap');
    IteratorWasRooted := Assigned(GC) and Assigned(Iterator) and not GC.IsTempRoot(Iterator);
    if IteratorWasRooted then
      GC.AddTempRoot(Iterator);
    try
      Result := TGocciaLazyFlatMapIteratorValue.Create(
        Iterator, Callback
      );
    finally
      if IteratorWasRooted then
        GC.RemoveTempRoot(Iterator);
    end;
  finally
    if CallbackWasRooted then
      GC.RemoveTempRoot(Callback);
  end;
end;

{ Consuming helper methods — eagerly drain the iterator }

function TGocciaIteratorValue.IteratorForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorForEachNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorForEachCallable, SSuggestIteratorCallable);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'forEach');
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        InvokeIteratorCallback(Callback, Value, Index);
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaIteratorValue.IteratorReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  Accumulator, NewAccumulator, Value: TGocciaValue;
  HasInitial, Done: Boolean;
  CallArgs: TGocciaArgumentsCollection;
  Index: Integer;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorReduceNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorReduceCallable, SSuggestIteratorCallable);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'reduce');
  HasInitial := AArgs.Length >= 2;
  Index := 0;
  Done := False;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      if HasInitial then
        Accumulator := AArgs.GetElement(1)
      else
      begin
        Accumulator := Iterator.DirectNext(Done);
        if Done then
          ThrowTypeError(SErrorReduceEmptyIterator,
            SSuggestReduceInitialValue);
        Index := 1;
      end;

      TGarbageCollector.Instance.AddTempRoot(Accumulator);
      try
        Value := Iterator.DirectNext(Done);
        while not Done do
        begin
          CallArgs := TGocciaArgumentsCollection.Create([Accumulator, Value, TGocciaNumberLiteralValue.Create(Index)]);
          try
            NewAccumulator := InvokeCallable(Callback, CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue);
          finally
            CallArgs.Free;
          end;
          TGarbageCollector.Instance.RemoveTempRoot(Accumulator);
          Accumulator := NewAccumulator;
          TGarbageCollector.Instance.AddTempRoot(Accumulator);
          Value := Iterator.DirectNext(Done);
          Inc(Index);
        end;

        Result := Accumulator;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Accumulator);
      end;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorToArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  ResultArray: TGocciaArrayValue;
  Value, RootedValue: TGocciaValue;
  Done: Boolean;
  ValueWasRooted: Boolean;
  GC: TGarbageCollector;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorToArrayNonIterator, SSuggestIteratorThisType);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'toArray');
  ResultArray := TGocciaArrayValue.Create;
  GC := TGarbageCollector.Instance;

  GC.AddTempRoot(Iterator);
  GC.AddTempRoot(ResultArray);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        RootedValue := Value;
        ValueWasRooted := Assigned(RootedValue) and not GC.IsTempRoot(RootedValue);
        if ValueWasRooted then
          GC.AddTempRoot(RootedValue);
        try
          ResultArray.Elements.Add(RootedValue);
          Value := Iterator.DirectNext(Done);
        finally
          if ValueWasRooted then
            GC.RemoveTempRoot(RootedValue);
        end;
      end;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;

    Result := ResultArray;
  finally
    GC.RemoveTempRoot(ResultArray);
    GC.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorSome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value, PredicateValue: TGocciaValue;
  Done: Boolean;
  Index: Integer;
  ValueWasRooted, PredicateWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorSomeNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorSomeCallable, SSuggestIteratorCallable);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'some');
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        ValueWasRooted := AddTempRootIfNeeded(Value);
        try
          PredicateValue := InvokeIteratorCallback(Callback, Value, Index);
          PredicateWasRooted := AddTempRootIfNeeded(PredicateValue);
          try
            if PredicateValue.ToBooleanLiteral.Value then
            begin
              Iterator.Close;
              Result := TGocciaBooleanLiteralValue.TrueValue;
              Exit;
            end;
          finally
            RemoveTempRootIfNeeded(PredicateValue, PredicateWasRooted);
          end;
        finally
          RemoveTempRootIfNeeded(Value, ValueWasRooted);
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaBooleanLiteralValue.FalseValue;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value, PredicateValue: TGocciaValue;
  Done: Boolean;
  Index: Integer;
  ValueWasRooted, PredicateWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorEveryNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorEveryCallable, SSuggestIteratorCallable);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'every');
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        ValueWasRooted := AddTempRootIfNeeded(Value);
        try
          PredicateValue := InvokeIteratorCallback(Callback, Value, Index);
          PredicateWasRooted := AddTempRootIfNeeded(PredicateValue);
          try
            if not PredicateValue.ToBooleanLiteral.Value then
            begin
              Iterator.Close;
              Result := TGocciaBooleanLiteralValue.FalseValue;
              Exit;
            end;
          finally
            RemoveTempRootIfNeeded(PredicateValue, PredicateWasRooted);
          end;
        finally
          RemoveTempRootIfNeeded(Value, ValueWasRooted);
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaBooleanLiteralValue.TrueValue;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value, PredicateValue: TGocciaValue;
  Done: Boolean;
  Index: Integer;
  ValueWasRooted, PredicateWasRooted: Boolean;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorFindNonIterator, SSuggestIteratorThisType);
  Callback := IteratorCallbackArgOrClose(AArgs, AThisValue,
    SErrorIteratorFindCallable, SSuggestIteratorCallable);

  Iterator := IteratorThisToDirectIterator(AThisValue, 'find');
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        ValueWasRooted := AddTempRootIfNeeded(Value);
        try
          PredicateValue := InvokeIteratorCallback(Callback, Value, Index);
          PredicateWasRooted := AddTempRootIfNeeded(PredicateValue);
          try
            if PredicateValue.ToBooleanLiteral.Value then
            begin
              Iterator.Close;
              Result := Value;
              Exit;
            end;
          finally
            RemoveTempRootIfNeeded(PredicateValue, PredicateWasRooted);
          end;
        finally
          RemoveTempRootIfNeeded(Value, ValueWasRooted);
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

{ Iterator.from() }

function TGocciaIteratorValue.IteratorFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
  ValueWasRooted, MethodWasRooted, IteratorWasRooted: Boolean;

  function AddRootIfNeeded(const ARootValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(GC) and Assigned(ARootValue) and
      not GC.IsTempRoot(ARootValue);
    if Result then
      GC.AddTempRoot(ARootValue);
  end;

  procedure RemoveRootIfNeeded(const ARootValue: TGocciaValue;
    const AWasRooted: Boolean);
  begin
    if AWasRooted then
      GC.RemoveTempRoot(ARootValue);
  end;

begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorIteratorFromRequiresArg, SSuggestIteratorFromArg);

  GC := TGarbageCollector.Instance;
  Value := AArgs.GetElement(0);

  if Value is TGocciaIteratorValue then
  begin
    Result := Value;
    Exit;
  end;

  if Value is TGocciaObjectValue then
  begin
    ValueWasRooted := AddRootIfNeeded(Value);
    try
      IteratorMethod := TGocciaObjectValue(Value).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
      // Per ES2024 GetMethod: a present-but-non-callable @@iterator is
      // a TypeError.  Falling through to the iterator-like (`next` on
      // Value) branch when @@iterator was malformed would mask the
      // protocol violation.
      if Assigned(IteratorMethod) and
         not (IteratorMethod is TGocciaUndefinedLiteralValue) and
         not (IteratorMethod is TGocciaNullLiteralValue) and
         not IteratorMethod.IsCallable then
        ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
          SSuggestIteratorProtocol);
      if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
      begin
        MethodWasRooted := AddRootIfNeeded(IteratorMethod);
        try
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := InvokeCallable(IteratorMethod, CallArgs, Value);
          finally
            CallArgs.Free;
          end;
        finally
          RemoveRootIfNeeded(IteratorMethod, MethodWasRooted);
        end;

        if IteratorObj is TGocciaIteratorValue then
        begin
          Result := IteratorObj;
          Exit;
        end;
        // §7.4.3 GetIterator step 4 / TC39 Iterator Helpers
        // GetIteratorFlattenable step 5: the value returned from
        // [@@iterator]() must be an Object — anything else is a
        // protocol violation, NOT a cue to fall back to iterator-like
        // handling on the outer Value.
        if not (IteratorObj is TGocciaObjectValue) then
          ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
        IteratorWasRooted := AddRootIfNeeded(IteratorObj);
        try
          NextMethod := IteratorObj.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
            // §7.4.2 GetIteratorDirect step 2: missing/non-callable next.
            ThrowTypeError(SErrorIteratorNextMustBeCallable,
              SSuggestIteratorProtocol);
          // Capture-once per ES2024 §7.4.2 GetIteratorDirect.
          Result := CreateRootedGenericIterator(IteratorObj, NextMethod);
        finally
          RemoveRootIfNeeded(IteratorObj, IteratorWasRooted);
        end;
        Exit;
      end;

      // No @@iterator (undefined/null): TC39 Iterator Helpers
      // GetIteratorFlattenable iterator-like fallback — try the value
      // itself as a duck-typed iterator (must have a callable next).
      NextMethod := Value.GetProperty(PROP_NEXT);
      if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
      begin
        // Capture-once per ES2024 §7.4.2 GetIteratorDirect.
        Result := CreateRootedGenericIterator(Value, NextMethod);
        Exit;
      end;
    finally
      RemoveRootIfNeeded(Value, ValueWasRooted);
    end;
  end;

  if Value is TGocciaStringLiteralValue then
  begin
    Result := TGocciaStringIteratorValue.Create(Value);
    Exit;
  end;

  ThrowTypeError(SErrorIteratorFromRequiresIterable,
    SSuggestIteratorFromArg);
  Result := nil;
end;

{ Iterator.concat() }

// TC39 Iterator Sequencing §1 Iterator.concat ( ...items )
function TGocciaIteratorValue.IteratorConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Item, IteratorMethod: TGocciaValue;
  Iterables: array of TGocciaConcatIterableRecord;
begin
  // TC39 Iterator Sequencing §1 step 1: Let iterables be a new empty List.
  SetLength(Iterables, AArgs.Length);

  // TC39 Iterator Sequencing §1 step 2: For each element item of items
  for I := 0 to AArgs.Length - 1 do
  begin
    Item := AArgs.GetElement(I);

    if Item is TGocciaStringLiteralValue then
    begin
      // Strings are iterable primitives — handle specially
      Iterables[I].Iterable := Item;
      Iterables[I].IteratorMethod := nil;
    end
    else if Item is TGocciaObjectValue then
    begin
      // TC39 Iterator Sequencing §1 step 2b: Let method be GetMethod(item, @@iterator)
      IteratorMethod := TGocciaObjectValue(Item).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
      // TC39 Iterator Sequencing §1 step 2c: If method is undefined, throw TypeError
      if not Assigned(IteratorMethod) or (IteratorMethod is TGocciaUndefinedLiteralValue) or not IteratorMethod.IsCallable then
        ThrowTypeError(SErrorIteratorConcatNotIterable, SSuggestNotIterable);
      Iterables[I].Iterable := Item;
      Iterables[I].IteratorMethod := IteratorMethod;
    end
    else
      // TC39 Iterator Sequencing §1 step 2a: If item is not an Object, throw TypeError
      ThrowTypeError(SErrorIteratorConcatNotIterable, SSuggestNotIterable);
  end;

  // TC39 Iterator Sequencing §1 steps 3-6: Create iterator from closure
  Result := TGocciaConcatIteratorValue.Create(Iterables);
end;

{ Iterator.zip() }

// TC39 Joint Iteration §1.1 Iterator.zip(iterables [, options])
function TGocciaIteratorValue.IteratorZip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
const
  ZIP_MODE_SHORTEST = 'shortest';
  ZIP_MODE_LONGEST = 'longest';
  ZIP_MODE_STRICT = 'strict';
var
  IterablesArg, OptionsArg, Item, ModeVal, PaddingVal, PaddingItem: TGocciaValue;
  OuterIterator, InnerIterator, PaddingIterator: TGocciaIteratorValue;
  OuterDone, PaddingDone: Boolean;
  Iterators: array of TGocciaIteratorValue;
  Padding: array of TGocciaValue;
  Mode: TGocciaZipMode;
  Items: TGocciaValueList;
  PaddingItems: TGocciaValueList;
  I, J, Count, Acquired: Integer;
  ModeStr: string;
  GC: TGarbageCollector;
  Success: Boolean;
begin
  // TC39 Joint Iteration §1.1 step 1: If iterables is not an Object, throw TypeError
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorIteratorZipRequiresArg, SSuggestNotIterable);

  IterablesArg := AArgs.GetElement(0);
  GC := TGarbageCollector.Instance;

  // TC39 Joint Iteration §1.1 step 2: Get iterator from iterables
  OuterIterator := GetIteratorFromIterable(IterablesArg);
  if OuterIterator = nil then
    ThrowTypeError(SErrorIteratorZipFirstIterable, SSuggestNotIterable);

  GC.AddTempRoot(OuterIterator);
  try
    // TC39 Joint Iteration §1.1 step 3: Collect all inner iterables
    Items := TGocciaValueList.Create(False);
    try
      Item := OuterIterator.DirectNext(OuterDone);
      while not OuterDone do
      begin
        Items.Add(Item);
        Item := OuterIterator.DirectNext(OuterDone);
      end;

      Count := Items.Count;
      SetLength(Iterators, Count);
      Acquired := 0;

      // TC39 Joint Iteration §1.1 step 4: Get iterators from each iterable
      try
        for I := 0 to Count - 1 do
        begin
          InnerIterator := GetIteratorFromIterable(Items[I]);
          if InnerIterator = nil then
            ThrowTypeError(SErrorIteratorZipItemIterable, SSuggestNotIterable);
          Iterators[I] := InnerIterator;
          GC.AddTempRoot(InnerIterator);
          Acquired := I + 1;
        end;
      except
        // Close and unroot already-acquired iterators before re-raising
        AcquireExceptionObject;
        for J := 0 to Acquired - 1 do
        begin
          CloseIteratorPreservingError(Iterators[J]);
          GC.RemoveTempRoot(Iterators[J]);
        end;
        raise;
      end;
    finally
      Items.Free;
    end;
  finally
    GC.RemoveTempRoot(OuterIterator);
  end;

  // TC39 Joint Iteration §1.1 step 5: Parse options
  Mode := zmShortest;
  SetLength(Padding, Count);
  for I := 0 to Count - 1 do
    Padding[I] := TGocciaUndefinedLiteralValue.UndefinedValue;

  Success := False;
  try
    if AArgs.Length >= 2 then
    begin
      OptionsArg := AArgs.GetElement(1);
      if Assigned(OptionsArg) and not (OptionsArg is TGocciaUndefinedLiteralValue) then
      begin
        if not (OptionsArg is TGocciaObjectValue) then
          ThrowTypeError(SErrorIteratorZipOptionsObject, SSuggestIteratorZipOptions);

        // TC39 Joint Iteration §1.1 step 5a: Get mode
        ModeVal := OptionsArg.GetProperty(PROP_MODE);
        if Assigned(ModeVal) and not (ModeVal is TGocciaUndefinedLiteralValue) then
        begin
          if not (ModeVal is TGocciaStringLiteralValue) then
            ThrowTypeError(SErrorIteratorZipModeString, SSuggestIteratorZipMode);
          ModeStr := TGocciaStringLiteralValue(ModeVal).Value;
          if ModeStr = ZIP_MODE_SHORTEST then
            Mode := zmShortest
          else if ModeStr = ZIP_MODE_LONGEST then
            Mode := zmLongest
          else if ModeStr = ZIP_MODE_STRICT then
            Mode := zmStrict
          else
            ThrowRangeError(Format(SErrorIteratorZipInvalidMode, [ModeStr]), SSuggestIteratorZipMode);
        end;

        // TC39 Joint Iteration §1.1 step 5b: Get padding (only for longest mode)
        if Mode = zmLongest then
        begin
          PaddingVal := OptionsArg.GetProperty(PROP_PADDING);
          if Assigned(PaddingVal) and not (PaddingVal is TGocciaUndefinedLiteralValue) then
          begin
            PaddingIterator := GetIteratorFromIterable(PaddingVal);
            if PaddingIterator = nil then
              ThrowTypeError(SErrorIteratorZipPaddingIterable, SSuggestNotIterable);
            GC.AddTempRoot(PaddingIterator);
            PaddingItems := TGocciaValueList.Create(False);
            try
              PaddingItem := PaddingIterator.DirectNext(PaddingDone);
              while not PaddingDone do
              begin
                PaddingItems.Add(PaddingItem);
                PaddingItem := PaddingIterator.DirectNext(PaddingDone);
              end;
              for I := 0 to Count - 1 do
              begin
                if I < PaddingItems.Count then
                  Padding[I] := PaddingItems[I];
              end;
            finally
              PaddingItems.Free;
              GC.RemoveTempRoot(PaddingIterator);
            end;
          end;
        end;
      end;
    end;

    Result := TGocciaZipIteratorValue.Create(Iterators, Padding, Mode);
    Success := True;
  finally
    // On error, close all iterators so generator finally blocks run
    if not Success then
    begin
      for I := 0 to Count - 1 do
        CloseIteratorPreservingError(Iterators[I]);
    end;
    // Unroot iterators — the zip iterator now owns them via MarkReferences
    for I := 0 to Count - 1 do
      GC.RemoveTempRoot(Iterators[I]);
  end;
end;

{ Iterator.zipKeyed() }

// TC39 Joint Iteration §1.2 Iterator.zipKeyed(iterables [, options])
function TGocciaIteratorValue.IteratorZipKeyed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
const
  ZIP_MODE_SHORTEST = 'shortest';
  ZIP_MODE_LONGEST = 'longest';
  ZIP_MODE_STRICT = 'strict';
var
  IterablesArg, OptionsArg, ModeVal, PaddingVal, PropValue: TGocciaValue;
  InnerIterator: TGocciaIteratorValue;
  Keys: TArray<string>;
  Iterators: array of TGocciaIteratorValue;
  Padding: array of TGocciaValue;
  Mode: TGocciaZipMode;
  I, J, Count, Acquired: Integer;
  ModeStr: string;
  GC: TGarbageCollector;
  Success: Boolean;
begin
  // TC39 Joint Iteration §1.2 step 1: If iterables is not an Object, throw TypeError
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorIteratorZipKeyedRequiresArg, SSuggestNotIterable);

  IterablesArg := AArgs.GetElement(0);
  if not (IterablesArg is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorZipKeyedFirstObject, SSuggestIteratorZipKeyedArg);

  GC := TGarbageCollector.Instance;

  // TC39 Joint Iteration §1.2 step 2: Get own enumerable property keys
  Keys := TGocciaObjectValue(IterablesArg).GetEnumerablePropertyNames;
  Count := Length(Keys);

  // TC39 Joint Iteration §1.2 step 3: Get iterators from each property value
  SetLength(Iterators, Count);
  Acquired := 0;
  try
    for I := 0 to Count - 1 do
    begin
      PropValue := IterablesArg.GetProperty(Keys[I]);
      InnerIterator := GetIteratorFromIterable(PropValue);
      if InnerIterator = nil then
        ThrowTypeError(Format(SErrorIteratorZipKeyedPropertyNotIterable, [Keys[I]]), SSuggestNotIterable);
      Iterators[I] := InnerIterator;
      GC.AddTempRoot(InnerIterator);
      Acquired := I + 1;
    end;
  except
    // Close and unroot already-acquired iterators before re-raising
    AcquireExceptionObject;
    for J := 0 to Acquired - 1 do
    begin
      CloseIteratorPreservingError(Iterators[J]);
      GC.RemoveTempRoot(Iterators[J]);
    end;
    raise;
  end;

  // TC39 Joint Iteration §1.2 step 4: Parse options
  Mode := zmShortest;
  SetLength(Padding, Count);
  for I := 0 to Count - 1 do
    Padding[I] := TGocciaUndefinedLiteralValue.UndefinedValue;

  Success := False;
  try
    if AArgs.Length >= 2 then
    begin
      OptionsArg := AArgs.GetElement(1);
      if Assigned(OptionsArg) and not (OptionsArg is TGocciaUndefinedLiteralValue) then
      begin
        if not (OptionsArg is TGocciaObjectValue) then
          ThrowTypeError(SErrorIteratorZipKeyedOptionsObject, SSuggestIteratorZipOptions);

        // TC39 Joint Iteration §1.2 step 4a: Get mode
        ModeVal := OptionsArg.GetProperty(PROP_MODE);
        if Assigned(ModeVal) and not (ModeVal is TGocciaUndefinedLiteralValue) then
        begin
          if not (ModeVal is TGocciaStringLiteralValue) then
            ThrowTypeError(SErrorIteratorZipKeyedModeString, SSuggestIteratorZipMode);
          ModeStr := TGocciaStringLiteralValue(ModeVal).Value;
          if ModeStr = ZIP_MODE_SHORTEST then
            Mode := zmShortest
          else if ModeStr = ZIP_MODE_LONGEST then
            Mode := zmLongest
          else if ModeStr = ZIP_MODE_STRICT then
            Mode := zmStrict
          else
            ThrowRangeError(Format(SErrorIteratorZipKeyedInvalidMode, [ModeStr]), SSuggestIteratorZipMode);
        end;

        // TC39 Joint Iteration §1.2 step 4b: Get padding (only for longest mode)
        if Mode = zmLongest then
        begin
          PaddingVal := OptionsArg.GetProperty(PROP_PADDING);
          if Assigned(PaddingVal) and not (PaddingVal is TGocciaUndefinedLiteralValue) then
          begin
            if not (PaddingVal is TGocciaObjectValue) then
              ThrowTypeError(SErrorIteratorZipKeyedPaddingObject, SSuggestIteratorZipKeyedPadding);
            for I := 0 to Count - 1 do
            begin
              PropValue := PaddingVal.GetProperty(Keys[I]);
              if Assigned(PropValue) and not (PropValue is TGocciaUndefinedLiteralValue) then
                Padding[I] := PropValue;
            end;
          end;
        end;
      end;
    end;

    Result := TGocciaZipKeyedIteratorValue.Create(Keys, Iterators, Padding, Mode);
    Success := True;
  finally
    // On error, close all iterators so generator finally blocks run
    if not Success then
    begin
      for I := 0 to Count - 1 do
        CloseIteratorPreservingError(Iterators[I]);
    end;
    // Unroot iterators — the zipKeyed iterator now owns them via MarkReferences
    for I := 0 to Count - 1 do
      GC.RemoveTempRoot(Iterators[I]);
  end;
end;

{ TGocciaIteratorHelperValue }

constructor TGocciaIteratorHelperValue.Create;
var
  HelperPrototype: TGocciaObjectValue;
  GC: TGarbageCollector;
  SelfWasRooted: Boolean;
begin
  inherited Create;
  FExecuting := False;
  FDirectResultRoot := nil;
  FDirectResultRooted := False;

  GC := TGarbageCollector.Instance;
  SelfWasRooted := Assigned(GC) and not GC.IsTempRoot(Self);
  if SelfWasRooted then
    GC.AddTempRoot(Self);
  try
    EnsureIteratorHelperPrototypeInitialized;
    HelperPrototype := GetSharedIteratorHelperPrototype;
    if Assigned(HelperPrototype) then
      FPrototype := HelperPrototype;
  finally
    if SelfWasRooted then
      GC.RemoveTempRoot(Self);
  end;
end;

destructor TGocciaIteratorHelperValue.Destroy;
begin
  ClearDirectResultRoot;
  inherited;
end;

procedure TGocciaIteratorHelperValue.ClearDirectResultRoot;
var
  GC: TGarbageCollector;
begin
  if FDirectResultRooted then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.RemoveQueuedRoot(FDirectResultRoot);
  end;
  FDirectResultRoot := nil;
  FDirectResultRooted := False;
end;

procedure TGocciaIteratorHelperValue.RootDirectResult(const AValue: TGocciaValue);
var
  GC: TGarbageCollector;
begin
  ClearDirectResultRoot;
  if not Assigned(AValue) then
    Exit;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    GC.AddQueuedRoot(AValue);
    FDirectResultRoot := AValue;
    FDirectResultRooted := True;
  end;
end;

function TGocciaIteratorHelperValue.AdvanceNext: TGocciaObjectValue;
begin
  ClearDirectResultRoot;
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  if FExecuting then
    ThrowTypeError(SErrorIteratorHelperExecuting, SSuggestIteratorProtocol);
  FExecuting := True;
  try
    Result := DoAdvanceNext;
  finally
    FExecuting := False;
  end;
end;

function TGocciaIteratorHelperValue.DirectNext(out ADone: Boolean): TGocciaValue;
begin
  ClearDirectResultRoot;
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  if FExecuting then
    ThrowTypeError(SErrorIteratorHelperExecuting, SSuggestIteratorProtocol);
  FExecuting := True;
  try
    Result := DoDirectNext(ADone);
    if not ADone then
      RootDirectResult(Result);
  finally
    FExecuting := False;
  end;
end;

function TGocciaIteratorHelperValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  if FExecuting then
    ThrowTypeError(SErrorIteratorHelperExecuting, SSuggestIteratorProtocol);
  if FDone then
    Exit(CreateIteratorResult(AValue, True));
  FExecuting := True;
  try
    Close;
  finally
    FExecuting := False;
  end;
  Result := CreateIteratorResult(AValue, True);
end;

procedure TGocciaIteratorHelperValue.Close;
begin
  ClearDirectResultRoot;
  FDone := True;
end;

initialization
  GIteratorPrototypeSlot := RegisterRealmSlot('Iterator.prototype');
  GIteratorHelperPrototypeSlot := RegisterRealmSlot('IteratorHelper.prototype');
  GIteratorConstructorSlot := RegisterRealmSlot('Iterator');
  GIteratorMethodHostSlot := RegisterRealmSlot('Iterator.prototype.methodHost');

end.
