unit Goccia.Values.IteratorValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIteratorValue = class(TGocciaObjectValue)
  private
    class var FSharedIteratorPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaIteratorValue;
  private
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

    class procedure EnsurePrototypeInitialized;
    procedure InitializePrototype;
  protected
    FDone: Boolean;
  public
    constructor Create;
    function AdvanceNext: TGocciaObjectValue; virtual;
    function ToStringTag: string; override;

    class function CreateGlobalObject: TGocciaObjectValue;
  end;

function CreateIteratorResult(const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
function InvokeIteratorCallback(const ACallback: TGocciaValue; const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ConstructorNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.Iterator.Lazy,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PropertyNames,
  Goccia.Values.SymbolValue;

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
  CallArgs := TGocciaArgumentsCollection.Create([AValue, TGocciaNumberLiteralValue.SmallInt(AIndex)]);
  try
    Result := TGocciaFunctionBase(ACallback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;
end;

{ TGocciaIteratorValue }

constructor TGocciaIteratorValue.Create;
begin
  inherited Create(nil);
  FDone := False;
  InitializePrototype;
  if Assigned(FSharedIteratorPrototype) then
    FPrototype := FSharedIteratorPrototype;
end;

function TGocciaIteratorValue.AdvanceNext: TGocciaObjectValue;
begin
  FDone := True;
  Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
end;

function TGocciaIteratorValue.ToStringTag: string;
begin
  Result := CTOR_ITERATOR;
end;

class procedure TGocciaIteratorValue.EnsurePrototypeInitialized;
begin
  if Assigned(FSharedIteratorPrototype) then Exit;
  TGocciaIteratorValue.Create;
end;

procedure TGocciaIteratorValue.InitializePrototype;
begin
  if Assigned(FSharedIteratorPrototype) then Exit;

  FSharedIteratorPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorNext, 'next', 0));

  FSharedIteratorPrototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorSelf, '[Symbol.iterator]', 0),
      [pfConfigurable, pfWritable]
    )
  );

  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorMap, 'map', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorFilter, 'filter', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorTake, 'take', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorDrop, 'drop', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorForEach, 'forEach', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorReduce, 'reduce', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorToArray, 'toArray', 0));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorSome, 'some', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorEvery, 'every', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorFind, 'find', 1));
  FSharedIteratorPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(IteratorFlatMap, 'flatMap', 1));

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedIteratorPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

class function TGocciaIteratorValue.CreateGlobalObject: TGocciaObjectValue;
begin
  EnsurePrototypeInitialized;

  Result := TGocciaObjectValue.Create;
  Result.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(FPrototypeMethodHost.IteratorFrom, 'from', 1)
  );
  Result.AssignProperty(PROP_PROTOTYPE, FSharedIteratorPrototype);
end;

{ Protocol methods }

function TGocciaIteratorValue.IteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.next called on non-iterator');
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
    ThrowTypeError('Iterator.prototype.map called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.map requires a callable argument');

  Result := TGocciaLazyMapIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

function TGocciaIteratorValue.IteratorFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.filter called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.filter requires a callable argument');

  Result := TGocciaLazyFilterIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

function TGocciaIteratorValue.IteratorTake(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Limit: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.take called on non-iterator');
  if AArgs.Length < 1 then
    ThrowRangeError('Iterator.prototype.take requires an argument');

  Limit := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if Limit < 0 then
    ThrowRangeError('Iterator.prototype.take argument must be non-negative');

  Result := TGocciaLazyTakeIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), Limit
  );
end;

function TGocciaIteratorValue.IteratorDrop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DropCount: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.drop called on non-iterator');
  if AArgs.Length < 1 then
    ThrowRangeError('Iterator.prototype.drop requires an argument');

  DropCount := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if DropCount < 0 then
    ThrowRangeError('Iterator.prototype.drop argument must be non-negative');

  Result := TGocciaLazyDropIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), DropCount
  );
end;

function TGocciaIteratorValue.IteratorFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.flatMap called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.flatMap requires a callable argument');

  Result := TGocciaLazyFlatMapIteratorValue.Create(
    TGocciaIteratorValue(AThisValue), AArgs.GetElement(0)
  );
end;

{ Consuming helper methods — eagerly drain the iterator }

function TGocciaIteratorValue.IteratorForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  IterResult: TGocciaObjectValue;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.forEach called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.forEach requires a callable argument');

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    IterResult := Iterator.AdvanceNext;
    while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
    begin
      InvokeIteratorCallback(Callback, IterResult.GetProperty(PROP_VALUE), Index);
      Inc(Index);
      IterResult := Iterator.AdvanceNext;
    end;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaIteratorValue.IteratorReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  Accumulator, NewAccumulator: TGocciaValue;
  IterResult: TGocciaObjectValue;
  HasInitial: Boolean;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.reduce called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.reduce requires a callable argument');

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  HasInitial := AArgs.Length >= 2;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    if HasInitial then
      Accumulator := AArgs.GetElement(1)
    else
    begin
      IterResult := Iterator.AdvanceNext;
      if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
        ThrowTypeError('Reduce of empty iterator with no initial value');
      Accumulator := IterResult.GetProperty(PROP_VALUE);
    end;

    TGocciaGarbageCollector.Instance.AddTempRoot(Accumulator);
    try
      IterResult := Iterator.AdvanceNext;
      while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
      begin
        CallArgs := TGocciaArgumentsCollection.Create([Accumulator, IterResult.GetProperty(PROP_VALUE)]);
        try
          NewAccumulator := TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          CallArgs.Free;
        end;
        TGocciaGarbageCollector.Instance.RemoveTempRoot(Accumulator);
        Accumulator := NewAccumulator;
        TGocciaGarbageCollector.Instance.AddTempRoot(Accumulator);
        IterResult := Iterator.AdvanceNext;
      end;

      Result := Accumulator;
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Accumulator);
    end;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorToArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  ResultArray: TGocciaArrayValue;
  IterResult: TGocciaObjectValue;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.toArray called on non-iterator');

  Iterator := TGocciaIteratorValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  TGocciaGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    IterResult := Iterator.AdvanceNext;
    while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
    begin
      ResultArray.Elements.Add(IterResult.GetProperty(PROP_VALUE));
      IterResult := Iterator.AdvanceNext;
    end;

    Result := ResultArray;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultArray);
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorSome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  IterResult: TGocciaObjectValue;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.some called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.some requires a callable argument');

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    IterResult := Iterator.AdvanceNext;
    while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
    begin
      if InvokeIteratorCallback(Callback, IterResult.GetProperty(PROP_VALUE), Index).ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
      Inc(Index);
      IterResult := Iterator.AdvanceNext;
    end;

    Result := TGocciaBooleanLiteralValue.FalseValue;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  IterResult: TGocciaObjectValue;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.every called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.every requires a callable argument');

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    IterResult := Iterator.AdvanceNext;
    while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
    begin
      if not InvokeIteratorCallback(Callback, IterResult.GetProperty(PROP_VALUE), Index).ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      Inc(Index);
      IterResult := Iterator.AdvanceNext;
    end;

    Result := TGocciaBooleanLiteralValue.TrueValue;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function TGocciaIteratorValue.IteratorFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Iterator: TGocciaIteratorValue;
  Callback: TGocciaValue;
  IterResult: TGocciaObjectValue;
  Value: TGocciaValue;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.find called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.find requires a callable argument');

  Iterator := TGocciaIteratorValue(AThisValue);
  Callback := AArgs.GetElement(0);
  Index := 0;

  TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    IterResult := Iterator.AdvanceNext;
    while not TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value do
    begin
      Value := IterResult.GetProperty(PROP_VALUE);
      if InvokeIteratorCallback(Callback, Value, Index).ToBooleanLiteral.Value then
      begin
        Result := Value;
        Exit;
      end;
      Inc(Index);
      IterResult := Iterator.AdvanceNext;
    end;

    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

{ Iterator.from() }

function TGocciaIteratorValue.IteratorFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('Iterator.from requires an argument');

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

  ThrowTypeError('Iterator.from requires an iterable or iterator-like object');
  Result := nil;
end;

end.
