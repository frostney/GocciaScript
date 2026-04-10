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
  private
    class var FSharedIteratorPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaIteratorValue;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
    class var FStaticMembers: array of TGocciaMemberDefinition;
  private
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
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.Iterator.Lazy,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
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
  CallArgs := TGocciaArgumentsCollection.Create([AValue, TGocciaNumberLiteralValue.Create(AIndex)]);
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
  if Assigned(FSharedIteratorPrototype) then Exit;
  TGocciaIteratorValue.Create;
end;

procedure TGocciaIteratorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FSharedIteratorPrototype) then Exit;

  FSharedIteratorPrototype := TGocciaObjectValue.Create;
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
  RegisterMemberDefinitions(FSharedIteratorPrototype, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedIteratorPrototype);
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
  end;
end;

class function TGocciaIteratorValue.CreateGlobalObject: TGocciaObjectValue;
var
  Members: TGocciaMemberCollection;
begin
  EnsurePrototypeInitialized;

  Result := TGocciaObjectValue.Create;
  if Length(FStaticMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('from', FPrototypeMethodHost.IteratorFrom, 1, gmkStaticMethod, [gmfNoFunctionPrototype]);
      FStaticMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Result, FStaticMembers);
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
  Callback, Value: TGocciaValue;
  Done: Boolean;
  Index: Integer;
begin
  if not (AThisValue is TGocciaIteratorValue) then
    ThrowTypeError('Iterator.prototype.forEach called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.forEach requires a callable argument');

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
    ThrowTypeError('Iterator.prototype.reduce called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.reduce requires a callable argument');

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
          ThrowTypeError('Reduce of empty iterator with no initial value');
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
    ThrowTypeError('Iterator.prototype.toArray called on non-iterator');

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
    ThrowTypeError('Iterator.prototype.some called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.some requires a callable argument');

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
    ThrowTypeError('Iterator.prototype.every called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.every requires a callable argument');

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
    ThrowTypeError('Iterator.prototype.find called on non-iterator');
  if (AArgs.Length < 1) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('Iterator.prototype.find requires a callable argument');

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
