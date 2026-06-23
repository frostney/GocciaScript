unit Goccia.Values.Iterator.Zip;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaZipMode = (zmShortest, zmLongest, zmStrict);

  TGocciaZipIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FIterators: array of TGocciaIteratorValue;
    FPadding: array of TGocciaValue;
    FMode: TGocciaZipMode;
    FExhausted: array of Boolean;
    procedure CloseAllIterators;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const AIterators: array of TGocciaIteratorValue;
      const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

  TGocciaZipKeyedIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FKeys: array of string;
    FIterators: array of TGocciaIteratorValue;
    FPadding: array of TGocciaValue;
    FMode: TGocciaZipMode;
    FExhausted: array of Boolean;
    procedure CloseAllIterators;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const AKeys: array of string;
      const AIterators: array of TGocciaIteratorValue;
      const APadding: array of TGocciaValue; const AMode: TGocciaZipMode);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

function GetIteratorFromIterable(const AValue: TGocciaValue): TGocciaIteratorValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.ObjectPropertyDescriptor,
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

// TC39 Joint Iteration §2.1.1 GetIteratorFlattenable(obj, primitiveHandling)
function GetIteratorFromIterable(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj: TGocciaValue;
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
  Result := nil;
  GC := TGarbageCollector.Instance;

  if AValue is TGocciaIteratorValue then
  begin
    Result := TGocciaIteratorValue(AValue);
    Exit;
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    ValueWasRooted := AddRootIfNeeded(AValue);
    try
      Result := TGocciaStringIteratorValue.Create(AValue);
    finally
      RemoveRootIfNeeded(AValue, ValueWasRooted);
    end;
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    ValueWasRooted := AddRootIfNeeded(AValue);
    try
      IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
      if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
      begin
        MethodWasRooted := AddRootIfNeeded(IteratorMethod);
        try
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
          finally
            CallArgs.Free;
          end;
        finally
          RemoveRootIfNeeded(IteratorMethod, MethodWasRooted);
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
            Result := CreateRootedGenericIterator(IteratorObj);
          finally
            RemoveRootIfNeeded(IteratorObj, IteratorWasRooted);
          end;
          Exit;
        end;
      end;
    finally
      RemoveRootIfNeeded(AValue, ValueWasRooted);
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
      CloseIteratorPreservingError(FIterators[I]);
  end;
end;

// TC39 Joint Iteration §1.1 Iterator.zip(iterables [, options])
function TGocciaZipIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  Value: TGocciaValue;
  Done: Boolean;
  ValueWasRooted: Boolean;
begin
  Value := DoDirectNext(Done);
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    Result := CreateIteratorResult(Value, Done);
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

// TC39 Joint Iteration §1.1 Iterator.zip(iterables [, options]) — yield step
function TGocciaZipIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I: Integer;
  InnerValue: TGocciaValue;
  InnerDone: Boolean;
  AnyDone, AllDone: Boolean;
begin
  if Length(FIterators) = 0 then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ResultArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    AnyDone := False;
    AllDone := True;

    // TC39 Joint Iteration §1.1 step 6.a: For each iteratorRecord of iteratorRecords
    for I := 0 to High(FIterators) do
    begin
      if FExhausted[I] then
      begin
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
        AcquireExceptionObject;
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
          ThrowTypeError(SErrorIteratorZipStrictLengthMismatch, SSuggestIteratorZipMode);
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
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
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
      CloseIteratorPreservingError(FIterators[I]);
  end;
end;

// TC39 Joint Iteration §1.2 Iterator.zipKeyed(iterables [, options])
function TGocciaZipKeyedIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  Value: TGocciaValue;
  Done: Boolean;
  ValueWasRooted: Boolean;
begin
  Value := DoDirectNext(Done);
  ValueWasRooted := AddTempRootIfNeeded(Value);
  try
    Result := CreateIteratorResult(Value, Done);
  finally
    RemoveTempRootIfNeeded(Value, ValueWasRooted);
  end;
end;

// TC39 Joint Iteration §1.2 Iterator.zipKeyed(iterables [, options]) — yield step
function TGocciaZipKeyedIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  ResultObj: TGocciaObjectValue;
  I: Integer;
  InnerValue: TGocciaValue;
  InnerDone: Boolean;
  AnyDone, AllDone: Boolean;
begin
  if Length(FIterators) = 0 then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ResultObj := TGocciaObjectValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultObj);
  try
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
        AcquireExceptionObject;
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
          ThrowTypeError(SErrorIteratorZipKeyedStrictLengthMismatch, SSuggestIteratorZipMode);
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
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultObj);
  end;
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
