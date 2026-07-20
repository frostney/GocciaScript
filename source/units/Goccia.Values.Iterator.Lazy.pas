unit Goccia.Values.Iterator.Lazy;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaLazyMapIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyFilterIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyTakeIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FIndex: Integer;
    FLimit: Integer;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyDropIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FIndex: Integer;
    FLimit: Integer;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyFlatMapIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
    FInnerIterator: TGocciaIteratorValue;
    function ResolveIterator(const AValue: TGocciaValue): TGocciaIteratorValue;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.SymbolValue;

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

{ TGocciaLazyMapIteratorValue }

constructor TGocciaLazyMapIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FCallback := ACallback;
  FIndex := 0;
end;

function TGocciaLazyMapIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value, MappedValue: TGocciaValue;
  ValueWasRooted, MappedValueWasRooted: Boolean;
begin
  IterResult := FSourceIterator.AdvanceNext;
  if IteratorResultDone(IterResult) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Value := IteratorResultValue(IterResult);
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    try
      MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
    except
      PreserveCurrentExceptionAcrossNestedHandler;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    MappedValueWasRooted := AddTempRootIfNeeded(MappedValue);
    try
      Inc(FIndex);
      Result := CreateIteratorResult(MappedValue, False);
    finally
      RemoveTempRootIfNeeded(MappedValue, MappedValueWasRooted);
    end;
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

function TGocciaLazyMapIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value: TGocciaValue;
  ValueWasRooted: Boolean;
begin
  Value := FSourceIterator.DirectNext(ADone);
  if ADone then
  begin
    FDone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    try
      Result := InvokeIteratorCallback(FCallback, Value, FIndex);
    except
      PreserveCurrentExceptionAcrossNestedHandler;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    Inc(FIndex);
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

procedure TGocciaLazyMapIteratorValue.Close;
begin
  if FDone then
    Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.Close;
end;

procedure TGocciaLazyMapIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.MarkReferences;
  if Assigned(FCallback) then
    FCallback.MarkReferences;
end;

{ TGocciaLazyFilterIteratorValue }

constructor TGocciaLazyFilterIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FCallback := ACallback;
  FIndex := 0;
end;

function TGocciaLazyFilterIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value, PredicateValue: TGocciaValue;
  ValueWasRooted, PredicateWasRooted: Boolean;
begin
  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if IteratorResultDone(IterResult) then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Value := IteratorResultValue(IterResult);
    ValueWasRooted := AddTempRootIfNeeded(Value);
    try
      try
        PredicateValue := InvokeIteratorCallback(FCallback, Value, FIndex);
      except
        PreserveCurrentExceptionAcrossNestedHandler;
        CloseIteratorPreservingError(FSourceIterator);
        raise;
      end;
      PredicateWasRooted := AddTempRootIfNeeded(PredicateValue);
      try
        if PredicateValue.ToBooleanLiteral.Value then
        begin
          Inc(FIndex);
          Result := CreateIteratorResult(Value, False);
          Exit;
        end;
      finally
        RemoveTempRootIfNeeded(PredicateValue, PredicateWasRooted);
      end;
    finally
      RemoveTempRootIfNeeded(Value, ValueWasRooted);
    end;
    Inc(FIndex);
  until False;
end;

function TGocciaLazyFilterIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value, PredicateValue: TGocciaValue;
  ValueWasRooted, PredicateWasRooted: Boolean;
begin
  repeat
    Value := FSourceIterator.DirectNext(ADone);
    if ADone then
    begin
      FDone := True;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    ValueWasRooted := AddTempRootIfNeeded(Value);
    try
      try
        PredicateValue := InvokeIteratorCallback(FCallback, Value, FIndex);
      except
        PreserveCurrentExceptionAcrossNestedHandler;
        CloseIteratorPreservingError(FSourceIterator);
        raise;
      end;
      PredicateWasRooted := AddTempRootIfNeeded(PredicateValue);
      try
        if PredicateValue.ToBooleanLiteral.Value then
        begin
          Inc(FIndex);
          Result := Value;
          Exit;
        end;
      finally
        RemoveTempRootIfNeeded(PredicateValue, PredicateWasRooted);
      end;
    finally
      RemoveTempRootIfNeeded(Value, ValueWasRooted);
    end;
    Inc(FIndex);
  until False;
end;

procedure TGocciaLazyFilterIteratorValue.Close;
begin
  if FDone then
    Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.Close;
end;

procedure TGocciaLazyFilterIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.MarkReferences;
  if Assigned(FCallback) then
    FCallback.MarkReferences;
end;

{ TGocciaLazyTakeIteratorValue }

constructor TGocciaLazyTakeIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FIndex := 0;
  FLimit := ALimit;
end;

function TGocciaLazyTakeIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value: TGocciaValue;
  ValueWasRooted: Boolean;
begin
  if FIndex >= FLimit then
  begin
    FDone := True;
    CloseIterator(FSourceIterator);
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  IterResult := FSourceIterator.AdvanceNext;
  if IteratorResultDone(IterResult) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Inc(FIndex);
  Value := IteratorResultValue(IterResult);
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    Result := CreateIteratorResult(Value, False);
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

function TGocciaLazyTakeIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
begin
  if FIndex >= FLimit then
  begin
    FDone := True;
    CloseIterator(FSourceIterator);
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  Result := FSourceIterator.DirectNext(ADone);
  if ADone then
  begin
    FDone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  Inc(FIndex);
end;

procedure TGocciaLazyTakeIteratorValue.Close;
begin
  if FDone then
    Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.Close;
end;

procedure TGocciaLazyTakeIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.MarkReferences;
end;

{ TGocciaLazyDropIteratorValue }

constructor TGocciaLazyDropIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FIndex := 0;
  FLimit := ALimit;
end;

function TGocciaLazyDropIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value: TGocciaValue;
  ValueWasRooted: Boolean;
begin
  while FIndex < FLimit do
  begin
    IterResult := FSourceIterator.AdvanceNext;
    if IteratorResultDone(IterResult) then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Inc(FIndex);
  end;

  IterResult := FSourceIterator.AdvanceNext;
  if IteratorResultDone(IterResult) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Value := IteratorResultValue(IterResult);
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    Result := CreateIteratorResult(Value, False);
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

function TGocciaLazyDropIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
begin
  while FIndex < FLimit do
  begin
    Result := FSourceIterator.DirectNext(ADone);
    if ADone then
    begin
      FDone := True;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    Inc(FIndex);
  end;

  Result := FSourceIterator.DirectNext(ADone);
  if ADone then
  begin
    FDone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

procedure TGocciaLazyDropIteratorValue.Close;
begin
  if FDone then
    Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.Close;
end;

procedure TGocciaLazyDropIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.MarkReferences;
end;

{ TGocciaLazyFlatMapIteratorValue }

constructor TGocciaLazyFlatMapIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FCallback := ACallback;
  FIndex := 0;
  FInnerIterator := nil;
end;

function TGocciaLazyFlatMapIteratorValue.ResolveIterator(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
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
  // Iterator.prototype.flatMap uses GetIteratorFlattenable(...,
  // reject-primitives): primitives, including strings, are mapper errors.
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorIteratorFlatMapMustReturnIterable,
      SSuggestIteratorFlatMapCallable);

  GC := TGarbageCollector.Instance;
  ValueWasRooted := AddRootIfNeeded(AValue);
  try
    IteratorObj := AValue;
    IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       not (IteratorMethod is TGocciaNullLiteralValue) then
    begin
      if not IteratorMethod.IsCallable then
        ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
          SSuggestIteratorProtocol);

      MethodWasRooted := AddRootIfNeeded(IteratorMethod);
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        try
          IteratorObj := InvokeCallable(IteratorMethod, CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        RemoveRootIfNeeded(IteratorMethod, MethodWasRooted);
      end;

      if not (IteratorObj is TGocciaObjectValue) then
        ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
    end;

    if IteratorObj is TGocciaIteratorValue then
    begin
      Result := TGocciaIteratorValue(IteratorObj);
      Exit;
    end;

    if IteratorObj is TGocciaObjectValue then
    begin
      IteratorWasRooted := AddRootIfNeeded(IteratorObj);
      try
        NextMethod := TGocciaObjectValue(IteratorObj).GetProperty(PROP_NEXT);
        Result := CreateRootedGenericIterator(IteratorObj, NextMethod);
      finally
        RemoveRootIfNeeded(IteratorObj, IteratorWasRooted);
      end;
      Exit;
    end;
  finally
    RemoveRootIfNeeded(AValue, ValueWasRooted);
  end;

  ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
  Result := nil;
end;

function TGocciaLazyFlatMapIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult, InnerResult: TGocciaObjectValue;
  Value, MappedValue: TGocciaValue;
  ValueWasRooted, MappedValueWasRooted, InnerValueWasRooted: Boolean;
begin
  if Assigned(FInnerIterator) then
  begin
    try
      InnerResult := FInnerIterator.AdvanceNext;
    except
      PreserveCurrentExceptionAcrossNestedHandler;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    if not IteratorResultDone(InnerResult) then
    begin
      Value := IteratorResultValue(InnerResult);
      InnerValueWasRooted := AddTempRootIfNeeded(Value);
      try
        Result := CreateIteratorResult(Value, False);
      finally
        RemoveTempRootIfNeeded(Value, InnerValueWasRooted);
      end;
      Exit;
    end;
    FInnerIterator := nil;
  end;

  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if IteratorResultDone(IterResult) then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;

    Value := IteratorResultValue(IterResult);
    ValueWasRooted := AddTempRootIfNeeded(Value);
    try
      try
        MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
      except
        PreserveCurrentExceptionAcrossNestedHandler;
        CloseIteratorPreservingError(FSourceIterator);
        if Assigned(FInnerIterator) then
          CloseIteratorPreservingError(FInnerIterator);
        raise;
      end;
      MappedValueWasRooted := AddTempRootIfNeeded(MappedValue);
      try
        Inc(FIndex);

        try
          FInnerIterator := ResolveIterator(MappedValue);
          InnerResult := FInnerIterator.AdvanceNext;
        except
          PreserveCurrentExceptionAcrossNestedHandler;
          CloseIteratorPreservingError(FSourceIterator);
          raise;
        end;
      finally
        RemoveTempRootIfNeeded(MappedValue, MappedValueWasRooted);
      end;
    finally
      RemoveTempRootIfNeeded(Value, ValueWasRooted);
    end;
    if not IteratorResultDone(InnerResult) then
    begin
      Value := IteratorResultValue(InnerResult);
      InnerValueWasRooted := AddTempRootIfNeeded(Value);
      try
        Result := CreateIteratorResult(Value, False);
      finally
        RemoveTempRootIfNeeded(Value, InnerValueWasRooted);
      end;
      Exit;
    end;
    FInnerIterator := nil;
  until False;
end;

function TGocciaLazyFlatMapIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value, MappedValue, InnerValue: TGocciaValue;
  InnerDone: Boolean;
  ValueWasRooted, MappedValueWasRooted: Boolean;
begin
  if Assigned(FInnerIterator) then
  begin
    try
      InnerValue := FInnerIterator.DirectNext(InnerDone);
    except
      PreserveCurrentExceptionAcrossNestedHandler;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    if not InnerDone then
    begin
      ADone := False;
      Result := InnerValue;
      Exit;
    end;
    FInnerIterator := nil;
  end;

  repeat
    Value := FSourceIterator.DirectNext(ADone);
    if ADone then
    begin
      FDone := True;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    ValueWasRooted := AddTempRootIfNeeded(Value);
    try
      try
        MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
      except
        PreserveCurrentExceptionAcrossNestedHandler;
        CloseIteratorPreservingError(FSourceIterator);
        if Assigned(FInnerIterator) then
          CloseIteratorPreservingError(FInnerIterator);
        raise;
      end;
      MappedValueWasRooted := AddTempRootIfNeeded(MappedValue);
      try
        Inc(FIndex);

        try
          FInnerIterator := ResolveIterator(MappedValue);
          InnerValue := FInnerIterator.DirectNext(InnerDone);
        except
          PreserveCurrentExceptionAcrossNestedHandler;
          CloseIteratorPreservingError(FSourceIterator);
          raise;
        end;
      finally
        RemoveTempRootIfNeeded(MappedValue, MappedValueWasRooted);
      end;
    finally
      RemoveTempRootIfNeeded(Value, ValueWasRooted);
    end;
    if not InnerDone then
    begin
      ADone := False;
      Result := InnerValue;
      Exit;
    end;
    FInnerIterator := nil;
  until False;
end;

procedure TGocciaLazyFlatMapIteratorValue.Close;
begin
  if FDone then
    Exit;
  inherited;
  if Assigned(FInnerIterator) then
    FInnerIterator.Close;
  if Assigned(FSourceIterator) then
    FSourceIterator.Close;
end;

procedure TGocciaLazyFlatMapIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSourceIterator) then
    FSourceIterator.MarkReferences;
  if Assigned(FCallback) then
    FCallback.MarkReferences;
  if Assigned(FInnerIterator) then
    FInnerIterator.MarkReferences;
end;

end.
