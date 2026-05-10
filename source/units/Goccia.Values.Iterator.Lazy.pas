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
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.SymbolValue;

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
begin
  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Value := IterResult.GetProperty(PROP_VALUE);
  try
    MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
  except
    AcquireExceptionObject;
    CloseIteratorPreservingError(FSourceIterator);
    raise;
  end;
  Inc(FIndex);
  Result := CreateIteratorResult(MappedValue, False);
end;

function TGocciaLazyMapIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value: TGocciaValue;
begin
  Value := FSourceIterator.DirectNext(ADone);
  if ADone then
  begin
    FDone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  try
    Result := InvokeIteratorCallback(FCallback, Value, FIndex);
  except
    AcquireExceptionObject;
    CloseIteratorPreservingError(FSourceIterator);
    raise;
  end;
  Inc(FIndex);
end;

procedure TGocciaLazyMapIteratorValue.Close;
begin
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
  Value: TGocciaValue;
begin
  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Value := IterResult.GetProperty(PROP_VALUE);
    try
      if InvokeIteratorCallback(FCallback, Value, FIndex).ToBooleanLiteral.Value then
      begin
        Inc(FIndex);
        Result := CreateIteratorResult(Value, False);
        Exit;
      end;
    except
      AcquireExceptionObject;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    Inc(FIndex);
  until False;
end;

function TGocciaLazyFilterIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value: TGocciaValue;
begin
  repeat
    Value := FSourceIterator.DirectNext(ADone);
    if ADone then
    begin
      FDone := True;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    try
      if InvokeIteratorCallback(FCallback, Value, FIndex).ToBooleanLiteral.Value then
      begin
        Inc(FIndex);
        Result := Value;
        Exit;
      end;
    except
      AcquireExceptionObject;
      CloseIteratorPreservingError(FSourceIterator);
      raise;
    end;
    Inc(FIndex);
  until False;
end;

procedure TGocciaLazyFilterIteratorValue.Close;
begin
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
begin
  if FIndex >= FLimit then
  begin
    FDone := True;
    CloseIterator(FSourceIterator);
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Inc(FIndex);
  Result := CreateIteratorResult(IterResult.GetProperty(PROP_VALUE), False);
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
begin
  while FIndex < FLimit do
  begin
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Inc(FIndex);
  end;

  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Result := CreateIteratorResult(IterResult.GetProperty(PROP_VALUE), False);
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
begin
  if AValue is TGocciaIteratorValue then
  begin
    Result := TGocciaIteratorValue(AValue);
    Exit;
  end;

  if AValue is TGocciaArrayValue then
  begin
    Result := TGocciaArrayIteratorValue.Create(AValue, akValues);
    Exit;
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    Result := TGocciaStringIteratorValue.Create(AValue);
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
    begin
      TGarbageCollector.Instance.AddTempRoot(AValue);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(AValue);
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;
      if IteratorObj is TGocciaObjectValue then
      begin
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
        begin
          // Capture-once per ES2024 §7.4.2 GetIteratorDirect.
          Result := TGocciaGenericIteratorValue.Create(IteratorObj, NextMethod);
          Exit;
        end;
      end;
    end;
  end;

  Result := nil;
end;

function TGocciaLazyFlatMapIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  IterResult, InnerResult: TGocciaObjectValue;
  Value, MappedValue: TGocciaValue;
begin
  if Assigned(FInnerIterator) then
  begin
    InnerResult := FInnerIterator.AdvanceNext;
    if not TGocciaBooleanLiteralValue(InnerResult.GetProperty(PROP_DONE)).Value then
    begin
      Result := CreateIteratorResult(InnerResult.GetProperty(PROP_VALUE), False);
      Exit;
    end;
    FInnerIterator := nil;
  end;

  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty(PROP_DONE)).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;

    Value := IterResult.GetProperty(PROP_VALUE);
    try
      MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
    except
      AcquireExceptionObject;
      CloseIteratorPreservingError(FSourceIterator);
      if Assigned(FInnerIterator) then
        CloseIteratorPreservingError(FInnerIterator);
      raise;
    end;
    Inc(FIndex);

    FInnerIterator := ResolveIterator(MappedValue);
    if not Assigned(FInnerIterator) then
    begin
      try
        ThrowTypeError(SErrorIteratorFlatMapMustReturnIterable, SSuggestIteratorFlatMapCallable);
      except
        AcquireExceptionObject;
        CloseIteratorPreservingError(FSourceIterator);
        raise;
      end;
    end;

    InnerResult := FInnerIterator.AdvanceNext;
    if not TGocciaBooleanLiteralValue(InnerResult.GetProperty(PROP_DONE)).Value then
    begin
      Result := CreateIteratorResult(InnerResult.GetProperty(PROP_VALUE), False);
      Exit;
    end;
    FInnerIterator := nil;
  until False;
end;

function TGocciaLazyFlatMapIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  Value, MappedValue, InnerValue: TGocciaValue;
  InnerDone: Boolean;
begin
  if Assigned(FInnerIterator) then
  begin
    InnerValue := FInnerIterator.DirectNext(InnerDone);
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

    try
      MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
    except
      AcquireExceptionObject;
      CloseIteratorPreservingError(FSourceIterator);
      if Assigned(FInnerIterator) then
        CloseIteratorPreservingError(FInnerIterator);
      raise;
    end;
    Inc(FIndex);

    FInnerIterator := ResolveIterator(MappedValue);
    if not Assigned(FInnerIterator) then
    begin
      try
        ThrowTypeError(SErrorIteratorFlatMapMustReturnIterable, SSuggestIteratorFlatMapCallable);
      except
        AcquireExceptionObject;
        CloseIteratorPreservingError(FSourceIterator);
        raise;
      end;
    end;

    InnerValue := FInnerIterator.DirectNext(InnerDone);
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
