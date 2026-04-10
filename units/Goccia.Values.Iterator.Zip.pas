unit Goccia.Values.Iterator.Zip;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaZipMode = (zmShortest, zmLongest, zmStrict);

  TGocciaZipIteratorValue = class(TGocciaIteratorValue)
  private
    FIterators: array of TGocciaIteratorValue;
    FPadding: array of TGocciaValue;
    FMode: TGocciaZipMode;
    FExhausted: array of Boolean;
    procedure CloseAllIterators;
  public
    constructor Create(const AIterators: array of TGocciaIteratorValue;
      const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaZipKeyedIteratorValue = class(TGocciaIteratorValue)
  private
    FKeys: array of string;
    FIterators: array of TGocciaIteratorValue;
    FPadding: array of TGocciaValue;
    FMode: TGocciaZipMode;
    FExhausted: array of Boolean;
    procedure CloseAllIterators;
  public
    constructor Create(const AKeys: array of string;
      const AIterators: array of TGocciaIteratorValue;
      const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    procedure Close; override;
    procedure MarkReferences; override;
  end;

function GetIteratorFromIterable(const AValue: TGocciaValue): TGocciaIteratorValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

procedure CloseIteratorPreservingOriginalError(const AIterator: TGocciaIteratorValue);
begin
  if not Assigned(AIterator) then
    Exit;
  try
    AIterator.Close;
  except
    // Preserve the original abrupt-completion error when cleanup also throws.
  end;
end;

// TC39 Joint Iteration §2.1.1 GetIteratorFlattenable(obj, primitiveHandling)
function GetIteratorFromIterable(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Result := nil;

  if AValue is TGocciaIteratorValue then
  begin
    Result := TGocciaIteratorValue(AValue);
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
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
      finally
        CallArgs.Free;
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;
      if IteratorObj is TGocciaObjectValue then
      begin
        Result := TGocciaGenericIteratorValue.Create(IteratorObj);
        Exit;
      end;
    end;
  end;
end;

{ TGocciaZipIteratorValue }

constructor TGocciaZipIteratorValue.Create(const AIterators: array of TGocciaIteratorValue;
  const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FIterators, Length(AIterators));
  for I := 0 to High(AIterators) do
    FIterators[I] := AIterators[I];
  SetLength(FPadding, Length(APadding));
  for I := 0 to High(APadding) do
    FPadding[I] := APadding[I];
  FMode := AMode;
  SetLength(FExhausted, Length(AIterators));
  for I := 0 to High(FExhausted) do
    FExhausted[I] := False;
end;

procedure TGocciaZipIteratorValue.CloseAllIterators;
var
  I: Integer;
begin
  for I := 0 to High(FIterators) do
  begin
    if not FExhausted[I] then
      CloseIteratorPreservingOriginalError(FIterators[I]);
  end;
end;

// TC39 Joint Iteration §1.1 Iterator.zip(iterables [, options])
function TGocciaZipIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  Value: TGocciaValue;
  Done: Boolean;
begin
  Value := DirectNext(Done);
  Result := CreateIteratorResult(Value, Done);
end;

// TC39 Joint Iteration §1.1 Iterator.zip(iterables [, options]) — yield step
function TGocciaZipIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I: Integer;
  InnerValue: TGocciaValue;
  InnerDone: Boolean;
  AnyDone, AllDone: Boolean;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if Length(FIterators) = 0 then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ResultArray := TGocciaArrayValue.Create;
  AnyDone := False;
  AllDone := True;

  // TC39 Joint Iteration §1.1 step 6.a: For each iteratorRecord of iteratorRecords
  for I := 0 to High(FIterators) do
  begin
    if FExhausted[I] then
    begin
      // Already exhausted — use padding (only relevant for longest mode)
      if I < Length(FPadding) then
        ResultArray.Elements.Add(FPadding[I])
      else
        ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
      AnyDone := True;
      Continue;
    end;

    try
      InnerValue := FIterators[I].DirectNext(InnerDone);
    except
      CloseAllIterators;
      FDone := True;
      raise;
    end;

    if InnerDone then
    begin
      FExhausted[I] := True;
      AnyDone := True;
      if I < Length(FPadding) then
        ResultArray.Elements.Add(FPadding[I])
      else
        ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
    end
    else
    begin
      AllDone := False;
      ResultArray.Elements.Add(InnerValue);
    end;
  end;

  if not AnyDone then
    AllDone := False;

  case FMode of
    zmShortest:
    begin
      // TC39 Joint Iteration §1.1 step 6.a.ii: If mode is "shortest", close remaining
      if AnyDone then
      begin
        CloseAllIterators;
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
    zmLongest:
    begin
      // TC39 Joint Iteration §1.1: If all are done, complete
      if AllDone then
      begin
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
      // Otherwise yield the array with padding values for exhausted iterators
    end;
    zmStrict:
    begin
      // TC39 Joint Iteration §1.1: In strict mode, all must finish together
      if AnyDone and not AllDone then
      begin
        CloseAllIterators;
        FDone := True;
        ThrowTypeError('Iterator.zip: iterables have different lengths in strict mode');
      end;
      if AllDone then
      begin
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
  end;

  ADone := False;
  Result := ResultArray;
end;

procedure TGocciaZipIteratorValue.Close;
begin
  if FDone then Exit;
  FDone := True;
  CloseAllIterators;
end;

procedure TGocciaZipIteratorValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  for I := 0 to High(FIterators) do
  begin
    if Assigned(FIterators[I]) then
      FIterators[I].MarkReferences;
  end;
  for I := 0 to High(FPadding) do
  begin
    if Assigned(FPadding[I]) then
      FPadding[I].MarkReferences;
  end;
end;

{ TGocciaZipKeyedIteratorValue }

constructor TGocciaZipKeyedIteratorValue.Create(const AKeys: array of string;
  const AIterators: array of TGocciaIteratorValue;
  const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FKeys, Length(AKeys));
  for I := 0 to High(AKeys) do
    FKeys[I] := AKeys[I];
  SetLength(FIterators, Length(AIterators));
  for I := 0 to High(AIterators) do
    FIterators[I] := AIterators[I];
  SetLength(FPadding, Length(APadding));
  for I := 0 to High(APadding) do
    FPadding[I] := APadding[I];
  FMode := AMode;
  SetLength(FExhausted, Length(AIterators));
  for I := 0 to High(FExhausted) do
    FExhausted[I] := False;
end;

procedure TGocciaZipKeyedIteratorValue.CloseAllIterators;
var
  I: Integer;
begin
  for I := 0 to High(FIterators) do
  begin
    if not FExhausted[I] then
      CloseIteratorPreservingOriginalError(FIterators[I]);
  end;
end;

// TC39 Joint Iteration §1.2 Iterator.zipKeyed(iterables [, options])
function TGocciaZipKeyedIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  Value: TGocciaValue;
  Done: Boolean;
begin
  Value := DirectNext(Done);
  Result := CreateIteratorResult(Value, Done);
end;

// TC39 Joint Iteration §1.2 Iterator.zipKeyed(iterables [, options]) — yield step
function TGocciaZipKeyedIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  ResultObj: TGocciaObjectValue;
  I: Integer;
  InnerValue: TGocciaValue;
  InnerDone: Boolean;
  AnyDone, AllDone: Boolean;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if Length(FIterators) = 0 then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ResultObj := TGocciaObjectValue.Create;
  AnyDone := False;
  AllDone := True;

  // TC39 Joint Iteration §1.2 step 6.a: For each key of keys
  for I := 0 to High(FIterators) do
  begin
    if FExhausted[I] then
    begin
      if I < Length(FPadding) then
        ResultObj.AssignProperty(FKeys[I], FPadding[I])
      else
        ResultObj.AssignProperty(FKeys[I], TGocciaUndefinedLiteralValue.UndefinedValue);
      AnyDone := True;
      Continue;
    end;

    try
      InnerValue := FIterators[I].DirectNext(InnerDone);
    except
      CloseAllIterators;
      FDone := True;
      raise;
    end;

    if InnerDone then
    begin
      FExhausted[I] := True;
      AnyDone := True;
      if I < Length(FPadding) then
        ResultObj.AssignProperty(FKeys[I], FPadding[I])
      else
        ResultObj.AssignProperty(FKeys[I], TGocciaUndefinedLiteralValue.UndefinedValue);
    end
    else
    begin
      AllDone := False;
      ResultObj.AssignProperty(FKeys[I], InnerValue);
    end;
  end;

  if not AnyDone then
    AllDone := False;

  case FMode of
    zmShortest:
    begin
      if AnyDone then
      begin
        CloseAllIterators;
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
    zmLongest:
    begin
      if AllDone then
      begin
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
    zmStrict:
    begin
      if AnyDone and not AllDone then
      begin
        CloseAllIterators;
        FDone := True;
        ThrowTypeError('Iterator.zipKeyed: iterables have different lengths in strict mode');
      end;
      if AllDone then
      begin
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
  end;

  ADone := False;
  Result := ResultObj;
end;

procedure TGocciaZipKeyedIteratorValue.Close;
begin
  if FDone then Exit;
  FDone := True;
  CloseAllIterators;
end;

procedure TGocciaZipKeyedIteratorValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  for I := 0 to High(FIterators) do
  begin
    if Assigned(FIterators[I]) then
      FIterators[I].MarkReferences;
  end;
  for I := 0 to High(FPadding) do
  begin
    if Assigned(FPadding[I]) then
      FPadding[I].MarkReferences;
  end;
end;

end.
