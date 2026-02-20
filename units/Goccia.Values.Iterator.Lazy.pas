unit Goccia.Values.Iterator.Lazy;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaLazyMapIteratorValue = class(TGocciaIteratorValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyFilterIteratorValue = class(TGocciaIteratorValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyTakeIteratorValue = class(TGocciaIteratorValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FIndex: Integer;
    FLimit: Integer;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyDropIteratorValue = class(TGocciaIteratorValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FIndex: Integer;
    FLimit: Integer;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ALimit: Integer);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaLazyFlatMapIteratorValue = class(TGocciaIteratorValue)
  private
    FSourceIterator: TGocciaIteratorValue;
    FCallback: TGocciaValue;
    FIndex: Integer;
    FInnerIterator: TGocciaIteratorValue;
    function ResolveIterator(const AValue: TGocciaValue): TGocciaIteratorValue;
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.SymbolValue;

{ TGocciaLazyMapIteratorValue }

constructor TGocciaLazyMapIteratorValue.Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
begin
  inherited Create;
  FSourceIterator := ASourceIterator;
  FCallback := ACallback;
  FIndex := 0;
end;

function TGocciaLazyMapIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value, MappedValue: TGocciaValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Value := IterResult.GetProperty('value');
  MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
  Inc(FIndex);
  Result := CreateIteratorResult(MappedValue, False);
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

function TGocciaLazyFilterIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
  Value: TGocciaValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Value := IterResult.GetProperty('value');
    if InvokeIteratorCallback(FCallback, Value, FIndex).ToBooleanLiteral.Value then
    begin
      Inc(FIndex);
      Result := CreateIteratorResult(Value, False);
      Exit;
    end;
    Inc(FIndex);
  until False;
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

function TGocciaLazyTakeIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
begin
  if FDone or (FIndex >= FLimit) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Inc(FIndex);
  Result := CreateIteratorResult(IterResult.GetProperty('value'), False);
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

function TGocciaLazyDropIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  IterResult: TGocciaObjectValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  while FIndex < FLimit do
  begin
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;
    Inc(FIndex);
  end;

  IterResult := FSourceIterator.AdvanceNext;
  if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Result := CreateIteratorResult(IterResult.GetProperty('value'), False);
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
      TGocciaGarbageCollector.Instance.AddTempRoot(AValue);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        TGocciaGarbageCollector.Instance.RemoveTempRoot(AValue);
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;
      if IteratorObj is TGocciaObjectValue then
      begin
        NextMethod := IteratorObj.GetProperty('next');
        if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
        begin
          Result := TGocciaGenericIteratorValue.Create(IteratorObj);
          Exit;
        end;
      end;
    end;
  end;

  Result := nil;
end;

function TGocciaLazyFlatMapIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  IterResult, InnerResult: TGocciaObjectValue;
  Value, MappedValue: TGocciaValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  if Assigned(FInnerIterator) then
  begin
    InnerResult := FInnerIterator.AdvanceNext;
    if not TGocciaBooleanLiteralValue(InnerResult.GetProperty('done')).Value then
    begin
      Result := CreateIteratorResult(InnerResult.GetProperty('value'), False);
      Exit;
    end;
    FInnerIterator := nil;
  end;

  repeat
    IterResult := FSourceIterator.AdvanceNext;
    if TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value then
    begin
      FDone := True;
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
      Exit;
    end;

    Value := IterResult.GetProperty('value');
    MappedValue := InvokeIteratorCallback(FCallback, Value, FIndex);
    Inc(FIndex);

    FInnerIterator := ResolveIterator(MappedValue);
    if not Assigned(FInnerIterator) then
      ThrowTypeError('Iterator.prototype.flatMap callback must return an iterable');

    InnerResult := FInnerIterator.AdvanceNext;
    if not TGocciaBooleanLiteralValue(InnerResult.GetProperty('done')).Value then
    begin
      Result := CreateIteratorResult(InnerResult.GetProperty('value'), False);
      Exit;
    end;
    FInnerIterator := nil;
  until False;
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
