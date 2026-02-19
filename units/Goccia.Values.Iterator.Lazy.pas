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
  public
    constructor Create(const ASourceIterator: TGocciaIteratorValue; const ACallback: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Iterator.Concrete;

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

    if MappedValue is TGocciaIteratorValue then
      FInnerIterator := TGocciaIteratorValue(MappedValue)
    else if MappedValue is TGocciaArrayValue then
      FInnerIterator := TGocciaArrayIteratorValue.Create(MappedValue, akValues)
    else if MappedValue is TGocciaStringLiteralValue then
      FInnerIterator := TGocciaStringIteratorValue.Create(MappedValue)
    else
      ThrowTypeError('Iterator.prototype.flatMap called on non-object');

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
