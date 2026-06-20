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
    function AdvanceNextResultInternal(const AValue: TGocciaValue;
      const AHasValue: Boolean; out ADone: Boolean): TGocciaObjectValue;
    function ReturnInternal(const AValue: TGocciaValue;
      const AHasValue: Boolean): TGocciaObjectValue;
  public
    constructor Create(const AIteratorObject: TGocciaValue); overload;
    // Overload for callers that have already resolved `next` (e.g.
    // GetIteratorValue / GetIteratorFromValue).  ES2026 captures this
    // value without validating callability; AdvanceNextInternal reports
    // a non-callable `next` at the first step.
    constructor Create(const AIteratorObject, ANextMethod: TGocciaValue); overload;
    function AdvanceNext: TGocciaObjectValue; override;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function AdvanceNextResultValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
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

constructor TGocciaGenericIteratorValue.Create(
  const AIteratorObject, ANextMethod: TGocciaValue);
begin
  inherited Create;
  FSource := AIteratorObject;
  // ANextMethod was resolved by the caller (typically GetIteratorValue at
  // the iteratorRecord-creation site).  Trusting it directly avoids a
  // redundant GetProperty(PROP_NEXT) and keeps the capture-once contract
  // from the caller through to AdvanceNextInternal.
  FNextMethod := ANextMethod;
end;

function TGocciaGenericIteratorValue.AdvanceNextInternal(
  const AValue: TGocciaValue; const AHasValue: Boolean): TGocciaObjectValue;
var
  Done: Boolean;
  NextResult: TGocciaObjectValue;
  ValueVal: TGocciaValue;
begin
  if FDone then
    Exit(CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue,
      True));

  NextResult := AdvanceNextResultInternal(AValue, AHasValue, Done);
  if Done then
    ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue
  else
  begin
    ValueVal := NextResult.GetProperty(PROP_VALUE);
    if not Assigned(ValueVal) then
      ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
  Result := CreateIteratorResult(ValueVal, Done);
end;

function TGocciaGenericIteratorValue.AdvanceNextResultInternal(
  const AValue: TGocciaValue; const AHasValue: Boolean;
  out ADone: Boolean): TGocciaObjectValue;
var
  NextResult, DoneVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if FDone then
  begin
    ADone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  // ES2024 §7.4.2 GetIteratorDirect step 2 / §7.4.5 IteratorStep:
  // a missing/non-callable [[NextMethod]] is a TypeError, not silent
  // termination.  We deliberately do NOT mark the iterator FDone
  // before raising — per §7.4.10 IteratorClose, an abrupt completion
  // must run iter.return() best-effort, and ReturnInternal short-
  // circuits when FDone is already true.  Setting FDone here would
  // suppress the cleanup callback the consumer expects to fire.
  if not Assigned(FNextMethod) or
     (FNextMethod is TGocciaUndefinedLiteralValue) or
     not FNextMethod.IsCallable then
    ThrowTypeError(SErrorIteratorNextMustBeCallable, SSuggestIteratorProtocol);

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
  ADone := Assigned(DoneVal) and DoneVal.ToBooleanLiteral.Value;
  if ADone then
    FDone := True;
  Result := TGocciaObjectValue(NextResult);
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

function TGocciaGenericIteratorValue.AdvanceNextResultValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
begin
  Result := AdvanceNextResultInternal(AValue, True, Done);
end;

function TGocciaGenericIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNextResultInternal(nil, False, ADone);
  if ADone then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := IteratorResult.GetProperty(PROP_VALUE);
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGenericIteratorValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNextResultInternal(AValue, True, ADone);
  Result := IteratorResult.GetProperty(PROP_VALUE);
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGenericIteratorValue.ReturnInternal(
  const AValue: TGocciaValue; const AHasValue: Boolean): TGocciaObjectValue;
var
  ReturnMethod: TGocciaValue;
  DoneValue: TGocciaValue;
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
    // ES2026 §15.5.5 YieldExpression : yield * AssignmentExpression:
    // return() results with done:false are yielded and may resume delegation.
    DoneValue := TGocciaObjectValue(ReturnResult).GetProperty(PROP_DONE);
    if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
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
  if FDone then
    Exit;
  ReturnInternal(nil, False);
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
