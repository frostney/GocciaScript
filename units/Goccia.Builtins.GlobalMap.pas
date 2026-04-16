unit Goccia.Builtins.GlobalMap;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalMap = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    function MapGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

constructor TGocciaGlobalMap.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MapGroupBy, 'groupBy', 2));
end;

// ES2026 §24.1.2.1 Map.groupBy(items, callbackfn)
function TGocciaGlobalMap.MapGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Source: TGocciaValue;
  SourceArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultMap: TGocciaMapValue;
  GroupKey: TGocciaValue;
  GroupArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  IteratorMethod, IteratorObj, NextMethod, CurrentValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  Done: Boolean;
  I: Integer;

  procedure AddToGroup(const AValue: TGocciaValue; const AIndex: Integer);
  var
    EntryIndex: Integer;
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(AValue);
      CallArgs.Add(TGocciaNumberLiteralValue.Create(AIndex));
      GroupKey := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;

    // Use Map's SameValueZero lookup to preserve key identity (1 vs "1", etc.)
    EntryIndex := ResultMap.FindEntry(GroupKey);
    if EntryIndex >= 0 then
      GroupArray := TGocciaArrayValue(ResultMap.Entries[EntryIndex].Value)
    else
    begin
      GroupArray := TGocciaArrayValue.Create;
      ResultMap.SetEntry(GroupKey, GroupArray);
    end;

    // Append value to the group's elements list
    GroupArray.Elements.Add(AValue);
  end;

begin
  // Step 1: Let groups be GroupBy(items, callbackfn, zero)
  // (Validate arguments first)
  if not AArgs.GetElement(1).IsCallable then
    ThrowTypeError(SErrorMapGroupByRequiresCallback, SSuggestCallbackRequired);

  Source := AArgs.GetElement(0);
  Callback := AArgs.GetElement(1);

  // Step 2: Let map be a new empty Map
  ResultMap := TGocciaMapValue.Create;

  // Fast path: source is an array (avoids iterator overhead)
  if Source is TGocciaArrayValue then
  begin
    SourceArray := TGocciaArrayValue(Source);
    TGarbageCollector.Instance.AddTempRoot(ResultMap);
    try
      I := 0;
      while I < SourceArray.Elements.Count do
      begin
        AddToGroup(SourceArray.Elements[I], I);
        Inc(I);
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ResultMap);
    end;
    Result := ResultMap;
    Exit;
  end;

  // Iterator path: strings via string iterator
  if Source is TGocciaStringLiteralValue then
  begin
    Iterator := TGocciaStringIteratorValue.Create(Source);
    TGarbageCollector.Instance.AddTempRoot(Iterator);
    TGarbageCollector.Instance.AddTempRoot(ResultMap);
    try
      I := 0;
      CurrentValue := Iterator.DirectNext(Done);
      while not Done do
      begin
        AddToGroup(CurrentValue, I);
        Inc(I);
        CurrentValue := Iterator.DirectNext(Done);
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ResultMap);
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
    Result := ResultMap;
    Exit;
  end;

  // Iterator path: objects with [Symbol.iterator]
  if Source is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
    begin
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, Source);
      finally
        CallArgs.Free;
      end;

      if IteratorObj is TGocciaIteratorValue then
        Iterator := TGocciaIteratorValue(IteratorObj)
      else if IteratorObj is TGocciaObjectValue then
      begin
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
          Iterator := TGocciaGenericIteratorValue.Create(IteratorObj)
        else
          Iterator := nil;
      end
      else
        Iterator := nil;

      if not Assigned(Iterator) then
        ThrowTypeError(SErrorMapGroupByRequiresIterable, SSuggestNotIterable);

      TGarbageCollector.Instance.AddTempRoot(Iterator);
      TGarbageCollector.Instance.AddTempRoot(ResultMap);
      try
        I := 0;
        CurrentValue := Iterator.DirectNext(Done);
        while not Done do
        begin
          AddToGroup(CurrentValue, I);
          Inc(I);
          CurrentValue := Iterator.DirectNext(Done);
        end;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(ResultMap);
        TGarbageCollector.Instance.RemoveTempRoot(Iterator);
      end;
      Result := ResultMap;
      Exit;
    end;
  end;

  // Non-iterable: throw TypeError
  ThrowTypeError(SErrorMapGroupByRequiresIterable, SSuggestNotIterable);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
