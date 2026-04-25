unit Goccia.Values.IteratorValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIteratorValue = class(TGocciaObjectValue)
  public
    function IteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

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

    class procedure EnsurePrototypeInitialized;
    procedure InitializePrototype;
  protected
    FDone: Boolean;
  public
    constructor Create;
    function AdvanceNext: TGocciaObjectValue; virtual;
    function DirectNext(out ADone: Boolean): TGocciaValue; virtual;
    procedure Close; virtual;
    function ToStringTag: string; override;

    class function CreateGlobalObject: TGocciaObjectValue;
  end;

function CreateIteratorResult(const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
function InvokeIteratorCallback(const ACallback: TGocciaValue; const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
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

// Iterator.prototype lives in a per-realm slot so engine teardown unpins
// the prototype.  Method host and member definitions stay process-wide
// (immutable across realms) — same pattern as Number.prototype.
var
  GIteratorPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaIteratorValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FStaticMembers: TArray<TGocciaMemberDefinition>;

function GetSharedIteratorPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GIteratorPrototypeSlot))
  else
    Result := nil;
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

function CreateIteratorResult(const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.DefineProperty(PROP_VALUE, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
  if ADone then
    Result.DefineProperty(PROP_DONE, TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]))
  else
    Result.DefineProperty(PROP_DONE, TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.FalseValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

function InvokeIteratorCallback(const ACallback: TGocciaValue; const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
begin
  CallArgs := TGocciaArgumentsCollection.Create([AValue, TGocciaNumberLiteralValue.Create(AIndex)]);
  try
    Result := TGocciaFunctionBase(ACallback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;
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

function TGocciaIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  IterResult: TGocciaObjectValue;
begin
  IterResult := AdvanceNext;
  ADone := TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value;
  if ADone then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := IterResult.GetProperty(PROP_VALUE);
end;

procedure TGocciaIteratorValue.Close;
begin
  FDone := True;
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

procedure TGocciaIteratorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedIteratorPrototype) then Exit;

  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GIteratorPrototypeSlot, SharedPrototype);
  FPrototypeMethodHost := Self;
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('next', IteratorNext, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator, '[Symbol.iterator]',
        IteratorSelf, 0, [pfConfigurable, pfWritable]);
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
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(SharedPrototype, FPrototypeMembers);

  // SharedPrototype pinned via realm slot; method host pinned directly
  // because it's a process-wide singleton.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
end;

class function TGocciaIteratorValue.CreateGlobalObject: TGocciaObjectValue;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  EnsurePrototypeInitialized;

  Result := TGocciaObjectValue.Create;
  if Length(FStaticMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('from', FPrototypeMethodHost.IteratorFrom, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('concat', FPrototypeMethodHost.IteratorConcat, 0, gmkStaticMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('zip', FPrototypeMethodHost.IteratorZip, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('zipKeyed', FPrototypeMethodHost.IteratorZipKeyed, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
      FStaticMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Result, FStaticMembers);
  SharedPrototype := GetSharedIteratorPrototype;
  if Assigned(SharedPrototype) then
    Result.AssignProperty(PROP_PROTOTYPE, SharedPrototype);
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

{ Lazy helper methods — return new lazy iterators }

function TGocciaIteratorValue.IteratorMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorMapNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorMapCallable,
      SSuggestIteratorCallable);

  Result := TGocciaLazyMapIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

function TGocciaIteratorValue.IteratorFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorFilterNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorFilterCallable,
      SSuggestIteratorCallable);

  Result := TGocciaLazyFilterIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

function TGocciaIteratorValue.IteratorTake(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Limit: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorTakeNonIterator, SSuggestIteratorThisType);
  if AArgs.Length < 1 then
    ThrowRangeError(SErrorIteratorTakeRequiresArg, SSuggestIteratorNonNegative);

  Limit := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if Limit < 0 then
    ThrowRangeError(SErrorIteratorTakeNonNegative,
      SSuggestIteratorNonNegative);

  Result := TGocciaLazyTakeIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), Limit
  );
end;

function TGocciaIteratorValue.IteratorDrop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DropCount: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorDropNonIterator, SSuggestIteratorThisType);
  if AArgs.Length < 1 then
    ThrowRangeError(SErrorIteratorDropRequiresArg, SSuggestIteratorNonNegative);

  DropCount := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if DropCount < 0 then
    ThrowRangeError(SErrorIteratorDropNonNegative,
      SSuggestIteratorNonNegative);

  Result := TGocciaLazyDropIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), DropCount
  );
end;

function TGocciaIteratorValue.IteratorFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorFlatMapNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorFlatMapCallable,
      SSuggestIteratorFlatMapCallable);

  Result := TGocciaLazyFlatMapIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

{ Consuming helper methods — eagerly drain the iterator }

function TGocciaIteratorValue.IteratorForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorForEachNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorForEachCallable,
      SSuggestIteratorCallable);

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
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
      Iterator.Close;
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
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorReduceNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorReduceCallable,
      SSuggestIteratorCallable);

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
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
            NewAccumulator := TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
      Iterator.Close;
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
  Value: TGocciaValue;
  Done: Boolean;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorToArrayNonIterator, SSuggestIteratorThisType);

  Iterator := TGocciaIteratorValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    Value := Iterator.DirectNext(Done);
    while not Done do
    begin
      ResultArray.Elements.Add(Value);
      Value := Iterator.DirectNext(Done);
    end;

    Result := ResultArray;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorSome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorSomeNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorSomeCallable,
      SSuggestIteratorCallable);

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        if InvokeIteratorCallback(Callback, Value, Index).ToBooleanLiteral.Value then
        begin
          Iterator.Close;
          Result := TGocciaBooleanLiteralValue.TrueValue;
          Exit;
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaBooleanLiteralValue.FalseValue;
    except
      Iterator.Close;
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorEveryNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorEveryCallable,
      SSuggestIteratorCallable);

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        if not InvokeIteratorCallback(Callback, Value, Index).ToBooleanLiteral.Value then
        begin
          Iterator.Close;
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaBooleanLiteralValue.TrueValue;
    except
      Iterator.Close;
      raise;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError(SErrorIteratorFindNonIterator, SSuggestIteratorThisType);
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorIteratorFindCallable,
      SSuggestIteratorCallable);

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      Value := Iterator.DirectNext(Done);
      while not Done do
      begin
        if InvokeIteratorCallback(Callback, Value, Index).ToBooleanLiteral.Value then
        begin
          Iterator.Close;
          Result := Value;
          Exit;
        end;
        Inc(Index);
        Value := Iterator.DirectNext(Done);
      end;

      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    except
      Iterator.Close;
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
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorIteratorFromRequiresArg, SSuggestIteratorFromArg);

  Value := AArgs.GetElement(0);

  if Value is TGocciaIteratorValue then
  begin
    Result := Value;
    Exit;
  end;

  if Value is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(Value).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
    begin
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, Value);
      finally
        CallArgs.Free;
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := IteratorObj;
        Exit;
      end;
      if (IteratorObj is TGocciaObjectValue) then
      begin
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
        begin
          Result := TGocciaGenericIteratorValue.Create(IteratorObj);
          Exit;
        end;
      end;
    end;

    NextMethod := Value.GetProperty(PROP_NEXT);
    if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
    begin
      Result := TGocciaGenericIteratorValue.Create(Value);
      Exit;
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

initialization
  GIteratorPrototypeSlot := RegisterRealmSlot('Iterator.prototype');

end.
