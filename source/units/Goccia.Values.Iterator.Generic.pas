unit Goccia.Values.Iterator.Generic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGenericIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    // Captured at construction per ES2024 §7.4.2 GetIteratorDirect:
    // the iterator record's [[NextMethod]] is the result of `GetV(obj,
    // "next")` evaluated ONCE at iterator acquisition time.  Reusing
    // FNextMethod across AdvanceNext calls means post-acquisition
    // mutations of `iterator.next` are correctly ignored, and we
    // avoid a redundant GetProperty hash lookup per iteration.
    FNextMethod: TGocciaValue;
    function AdvanceNextInternal(const AValue: TGocciaValue;
      const AHasValue: Boolean): TGocciaObjectValue;
    function ReturnInternal(const AValue: TGocciaValue;
      const AHasValue: Boolean): TGocciaObjectValue;
  public
    constructor Create(const AIteratorObject: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function DirectNextValue(const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue; override;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function ThrowValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
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
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase;

{ TGocciaGenericIteratorValue }

constructor TGocciaGenericIteratorValue.Create(const AIteratorObject: TGocciaValue);
begin
  inherited Create;
  FSource := AIteratorObject;
  // Capture once.  Callers (GetIteratorFromValue and the seven
  // built-in factories above) already validate next is callable
  // before reaching here, so we trust the resolved value; the
  // defensive nil/non-callable guard in AdvanceNextInternal upgrades
  // the silent-truncation bug that previously fired when user code
  // mutated `iterator.next` mid-iteration into a spec-compliant
  // TypeError per ES2024 §7.4.5 IteratorStep.
  if Assigned(FSource) then
    FNextMethod := FSource.GetProperty(PROP_NEXT)
  else
    FNextMethod := nil;
end;

function TGocciaGenericIteratorValue.AdvanceNextInternal(
  const AValue: TGocciaValue; const AHasValue: Boolean): TGocciaObjectValue;
var
  NextResult, DoneVal, ValueVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  // ES2024 §7.4.2 GetIteratorDirect step 2 / §7.4.5 IteratorStep:
  // a missing/non-callable [[NextMethod]] is a TypeError, not silent
  // termination.  Closing the iterator before raising matches the
  // §7.4.10 IteratorClose semantics for abrupt completion.
  if not Assigned(FNextMethod) or
     (FNextMethod is TGocciaUndefinedLiteralValue) or
     not FNextMethod.IsCallable then
  begin
    FDone := True;
    ThrowTypeError(SErrorIteratorNextMustBeCallable, SSuggestIteratorProtocol);
  end;

  if AHasValue then
    CallArgs := TGocciaArgumentsCollection.Create([AValue])
  else
    CallArgs := TGocciaArgumentsCollection.Create;
  try
    NextResult := TGocciaFunctionBase(FNextMethod).Call(CallArgs, FSource);
  finally
    CallArgs.Free;
  end;

  if not (NextResult is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.TypeName]), SSuggestIteratorResultObject);

  DoneVal := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
  ValueVal := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
  if not Assigned(ValueVal) then
    ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Assigned(DoneVal) and DoneVal.ToBooleanLiteral.Value then
  begin
    FDone := True;
    Result := CreateIteratorResult(ValueVal, True);
  end
  else
    Result := CreateIteratorResult(ValueVal, False);
end;

function TGocciaGenericIteratorValue.AdvanceNext: TGocciaObjectValue;
begin
  Result := AdvanceNextInternal(nil, False);
end;

function TGocciaGenericIteratorValue.AdvanceNextValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := AdvanceNextInternal(AValue, True);
end;

function TGocciaGenericIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNext;
  ADone := IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value;
  Result := IteratorResult.GetProperty(PROP_VALUE);
end;

function TGocciaGenericIteratorValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNextValue(AValue);
  ADone := IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value;
  Result := IteratorResult.GetProperty(PROP_VALUE);
end;

function TGocciaGenericIteratorValue.ReturnInternal(
  const AValue: TGocciaValue; const AHasValue: Boolean): TGocciaObjectValue;
var
  ReturnMethod: TGocciaValue;
  DoneVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  ReturnResult: TGocciaValue;
begin
  if FDone then
  begin
    if AHasValue then
      Result := CreateIteratorResult(AValue, True)
    else
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  ReturnMethod := FSource.GetProperty(PROP_RETURN);
  if not Assigned(ReturnMethod) or
     (ReturnMethod is TGocciaUndefinedLiteralValue) or
     (ReturnMethod is TGocciaNullLiteralValue) then
  begin
    FDone := True;
    if AHasValue then
      Result := CreateIteratorResult(AValue, True)
    else
      Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  if not ReturnMethod.IsCallable then
    ThrowTypeError(SErrorIteratorReturnMustBeCallable, SSuggestIteratorProtocol);

  if AHasValue then
    CallArgs := TGocciaArgumentsCollection.Create([AValue])
  else
    CallArgs := TGocciaArgumentsCollection.Create;
  try
    ReturnResult := TGocciaFunctionBase(ReturnMethod).Call(CallArgs, FSource);
    if not (ReturnResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorIteratorReturnObject, SSuggestIteratorResultObject);
    DoneVal := TGocciaObjectValue(ReturnResult).GetProperty(PROP_DONE);
    if Assigned(DoneVal) and DoneVal.ToBooleanLiteral.Value then
      FDone := True;
    Result := TGocciaObjectValue(ReturnResult);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaGenericIteratorValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := ReturnInternal(AValue, True);
end;

function TGocciaGenericIteratorValue.ThrowValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  CallArgs: TGocciaArgumentsCollection;
  DoneValue: TGocciaValue;
  ThrowMethod: TGocciaValue;
  ThrowResult: TGocciaValue;
begin
  if FDone then
    ThrowTypeError('Delegated iterator has no throw method');

  ThrowMethod := FSource.GetProperty(PROP_THROW);
  if not Assigned(ThrowMethod) or
     (ThrowMethod is TGocciaUndefinedLiteralValue) or
     (ThrowMethod is TGocciaNullLiteralValue) then
  begin
    Close;
    ThrowTypeError('Delegated iterator has no throw method');
  end;
  if not ThrowMethod.IsCallable then
  begin
    Close;
    ThrowTypeError('Iterator throw is not callable');
  end;

  CallArgs := TGocciaArgumentsCollection.Create([AValue]);
  try
    ThrowResult := TGocciaFunctionBase(ThrowMethod).Call(CallArgs, FSource);
  finally
    CallArgs.Free;
  end;

  if not (ThrowResult is TGocciaObjectValue) then
    ThrowTypeError('Iterator throw result is not an object');
  DoneValue := TGocciaObjectValue(ThrowResult).GetProperty(PROP_DONE);
  if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
    FDone := True;
  Result := TGocciaObjectValue(ThrowResult);
end;

procedure TGocciaGenericIteratorValue.Close;
begin
  try
    ReturnInternal(nil, False);
  finally
    FDone := True;
  end;
end;

procedure TGocciaGenericIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
  if Assigned(FNextMethod) then
    FNextMethod.MarkReferences;
end;


end.
