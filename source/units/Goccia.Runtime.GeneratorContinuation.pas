unit Goccia.Runtime.GeneratorContinuation;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.ControlFlow,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.Values.IteratorValue,
  Goccia.Values.Primitives;

type
  TGocciaGeneratorResumeKind = (grkNext, grkReturn, grkThrow);

  EGocciaGeneratorYield = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  EGocciaGeneratorReturn = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaGeneratorContinuation = class
  private
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FCallScope: TGocciaScope;
    FContext: TGocciaEvaluationContext;
    FStatementIndex: Integer;
    FStarted: Boolean;
    FCompleted: Boolean;
    FSuspendedYield: TGocciaYieldExpression;
    FPendingKind: TGocciaGeneratorResumeKind;
    FPendingValue: TGocciaValue;
    FHasPendingValue: Boolean;
    FDelegateExpression: TGocciaYieldExpression;
    FDelegateIterator: TGocciaIteratorValue;
    FDelegateAsyncIterator: TGocciaValue;
    FDelegateAsyncNext: TGocciaValue;
  public
    constructor Create(const ABodyStatements: TObjectList<TGocciaASTNode>;
      const ACallScope: TGocciaScope; const AContext: TGocciaEvaluationContext);
    function Resume(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
    function YieldValue(const AYieldExpression: TGocciaYieldExpression;
      const AContext: TGocciaEvaluationContext): TGocciaValue;
    procedure MarkReferences;
    property Completed: Boolean read FCompleted;
  end;

function CurrentGeneratorContinuation: TGocciaGeneratorContinuation;
function EvaluateGeneratorYield(const AYieldExpression: TGocciaYieldExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

threadvar
  GCurrentContinuation: TGocciaGeneratorContinuation;

constructor EGocciaGeneratorYield.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

constructor EGocciaGeneratorReturn.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

constructor TGocciaGeneratorContinuation.Create(
  const ABodyStatements: TObjectList<TGocciaASTNode>;
  const ACallScope: TGocciaScope; const AContext: TGocciaEvaluationContext);
begin
  inherited Create;
  FBodyStatements := ABodyStatements;
  FCallScope := ACallScope;
  FContext := AContext;
  FContext.Scope := ACallScope;
  FStatementIndex := 0;
  FStarted := False;
  FCompleted := False;
  FSuspendedYield := nil;
  FPendingValue := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGeneratorContinuation.Resume(
  const AKind: TGocciaGeneratorResumeKind; const AValue: TGocciaValue;
  out ADone: Boolean): TGocciaValue;
var
  PreviousContinuation: TGocciaGeneratorContinuation;
  ControlFlow: TGocciaControlFlow;
begin
  if FCompleted then
  begin
    ADone := True;
    if AKind = grkThrow then
      raise TGocciaThrowValue.Create(AValue);
    Result := AValue;
    Exit;
  end;

  if AKind = grkReturn then
  begin
    if not FStarted then
    begin
      FCompleted := True;
      ADone := True;
      Result := AValue;
      Exit;
    end;
  end;

  if (not FStarted) and (AKind = grkThrow) then
  begin
    FCompleted := True;
    raise TGocciaThrowValue.Create(AValue);
  end;

  FHasPendingValue := FStarted and Assigned(FSuspendedYield);
  FPendingKind := AKind;
  FPendingValue := AValue;
  FStarted := True;

  PreviousContinuation := GCurrentContinuation;
  GCurrentContinuation := Self;
  try
    while FStatementIndex < FBodyStatements.Count do
    begin
      try
        if FBodyStatements[FStatementIndex] is TGocciaExpression then
          ControlFlow := TGocciaControlFlow.Normal(
            EvaluateExpression(TGocciaExpression(FBodyStatements[FStatementIndex]), FContext))
        else
          ControlFlow := EvaluateStatement(
            TGocciaStatement(FBodyStatements[FStatementIndex]), FContext);

        if ControlFlow.Kind = cfkReturn then
        begin
          FCompleted := True;
          ADone := True;
          if Assigned(ControlFlow.Value) then
            Result := ControlFlow.Value
          else
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
          Exit;
        end;

        Inc(FStatementIndex);
      except
        on E: EGocciaGeneratorYield do
        begin
          ADone := False;
          Result := E.Value;
          Exit;
        end;
        on E: EGocciaGeneratorReturn do
        begin
          FCompleted := True;
          ADone := True;
          Result := E.Value;
          Exit;
        end;
      end;
    end;
  finally
    GCurrentContinuation := PreviousContinuation;
  end;

  FCompleted := True;
  ADone := True;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGeneratorContinuation.YieldValue(
  const AYieldExpression: TGocciaYieldExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  IteratorResult: TGocciaObjectValue;
  IteratorMethod: TGocciaValue;
  YieldedValue: TGocciaValue;
begin
  if FHasPendingValue and (FSuspendedYield = AYieldExpression) then
  begin
    FHasPendingValue := False;
    FSuspendedYield := nil;
    if FPendingKind = grkThrow then
      raise TGocciaThrowValue.Create(FPendingValue);
    if FPendingKind = grkReturn then
      raise EGocciaGeneratorReturn.Create(FPendingValue);
    if not AYieldExpression.IsDelegate then
    begin
      Result := FPendingValue;
      Exit;
    end;
  end;

  if AYieldExpression.IsDelegate then
  begin
    if FDelegateExpression <> AYieldExpression then
    begin
      if Assigned(AYieldExpression.Operand) then
        YieldedValue := EvaluateExpression(AYieldExpression.Operand, AContext)
      else
        YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;

      FDelegateAsyncIterator := nil;
      FDelegateAsyncNext := nil;
      if YieldedValue is TGocciaObjectValue then
      begin
        IteratorMethod := TGocciaObjectValue(YieldedValue).GetSymbolProperty(
          TGocciaSymbolValue.WellKnownAsyncIterator);
        if Assigned(IteratorMethod) and IteratorMethod.IsCallable then
        begin
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            FDelegateAsyncIterator := TGocciaFunctionBase(IteratorMethod).Call(
              CallArgs, YieldedValue);
          finally
            CallArgs.Free;
          end;
          if Assigned(FDelegateAsyncIterator) then
            FDelegateAsyncNext := FDelegateAsyncIterator.GetProperty(PROP_NEXT);
        end;
      end;

      if not Assigned(FDelegateAsyncIterator) then
        FDelegateIterator := GetIteratorFromValue(YieldedValue);
      FDelegateExpression := AYieldExpression;
    end;

    if Assigned(FDelegateAsyncIterator) then
    begin
      if not Assigned(FDelegateAsyncNext) or not FDelegateAsyncNext.IsCallable then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue
      else
      begin
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorResult := AwaitValue(TGocciaFunctionBase(FDelegateAsyncNext).Call(
            CallArgs, FDelegateAsyncIterator)) as TGocciaObjectValue;
        finally
          CallArgs.Free;
        end;

        if IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
        begin
          Result := IteratorResult.GetProperty(PROP_VALUE);
          if not Assigned(Result) then
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
          FDelegateAsyncIterator := nil;
          FDelegateAsyncNext := nil;
          FDelegateExpression := nil;
          Exit;
        end;

        YieldedValue := IteratorResult.GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        FSuspendedYield := AYieldExpression;
        raise EGocciaGeneratorYield.Create(YieldedValue);
      end;
    end;

    if Assigned(FDelegateIterator) then
    begin
      IteratorResult := FDelegateIterator.AdvanceNext;
      if IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
      begin
        Result := IteratorResult.GetProperty(PROP_VALUE);
        if not Assigned(Result) then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        FDelegateIterator := nil;
        FDelegateExpression := nil;
        Exit;
      end;

      YieldedValue := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(YieldedValue) then
        YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      FSuspendedYield := AYieldExpression;
      raise EGocciaGeneratorYield.Create(YieldedValue);
    end;

    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if Assigned(AYieldExpression.Operand) then
    YieldedValue := EvaluateExpression(AYieldExpression.Operand, AContext)
  else
    YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  FSuspendedYield := AYieldExpression;
  raise EGocciaGeneratorYield.Create(YieldedValue);
end;

procedure TGocciaGeneratorContinuation.MarkReferences;
begin
  if Assigned(FCallScope) then
    FCallScope.MarkReferences;
  if Assigned(FPendingValue) then
    FPendingValue.MarkReferences;
  if Assigned(FDelegateIterator) then
    FDelegateIterator.MarkReferences;
  if Assigned(FDelegateAsyncIterator) then
    FDelegateAsyncIterator.MarkReferences;
  if Assigned(FDelegateAsyncNext) then
    FDelegateAsyncNext.MarkReferences;
end;

function CurrentGeneratorContinuation: TGocciaGeneratorContinuation;
begin
  Result := GCurrentContinuation;
end;

function EvaluateGeneratorYield(const AYieldExpression: TGocciaYieldExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  if not Assigned(GCurrentContinuation) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := GCurrentContinuation.YieldValue(AYieldExpression, AContext);
end;

end.
