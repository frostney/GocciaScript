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
  TGocciaGeneratorTryPhase = (gtpTry, gtpCatch, gtpFinally);

  TGocciaGeneratorTryState = class
  public
    Phase: TGocciaGeneratorTryPhase;
    ResultFlow: TGocciaControlFlow;
    CatchValue: TGocciaValue;
    ThrownValue: TGocciaValue;
    GeneratorReturnValue: TGocciaValue;
    HasUnhandledThrow: Boolean;
    HasGeneratorReturn: Boolean;
    constructor Create;
    procedure MarkReferences;
  end;

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
    FCompletedExpressionValues: TDictionary<TObject, TGocciaValue>;
    FExpressionValues: TDictionary<TObject, TGocciaValue>;
    FStatementIndexes: TDictionary<TObject, Integer>;
    FTryStates: TDictionary<TObject, TGocciaGeneratorTryState>;
    FIsAsyncGenerator: Boolean;
  public
    constructor Create(const ABodyStatements: TObjectList<TGocciaASTNode>;
      const ACallScope: TGocciaScope; const AContext: TGocciaEvaluationContext;
      const AIsAsyncGenerator: Boolean = False);
    destructor Destroy; override;
    function Resume(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
    function YieldValue(const AYieldExpression: TGocciaYieldExpression;
      const AContext: TGocciaEvaluationContext): TGocciaValue;
    procedure SaveCompletedExpressionValue(const AExpression: TObject; const AValue: TGocciaValue);
    function TakeCompletedExpressionValue(const AExpression: TObject; out AValue: TGocciaValue): Boolean;
    procedure SaveExpressionValue(const AExpression: TObject; const AValue: TGocciaValue);
    function TakeExpressionValue(const AExpression: TObject; out AValue: TGocciaValue): Boolean;
    procedure ClearExpressionValue(const AExpression: TObject);
    procedure ClearExpressionValues;
    function GetStatementIndex(const AStatements: TObject): Integer;
    procedure SaveStatementIndex(const AStatements: TObject; const AIndex: Integer);
    procedure ClearStatementIndex(const AStatements: TObject);
    procedure ClearStatementIndexes;
    function EnsureTryState(const ATryStatement: TObject): TGocciaGeneratorTryState;
    function GetTryState(const ATryStatement: TObject): TGocciaGeneratorTryState;
    procedure ClearTryState(const ATryStatement: TObject);
    procedure ClearTryStates;
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
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.IteratorSupport,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue;

threadvar
  GCurrentContinuation: TGocciaGeneratorContinuation;

type
  TGocciaAsyncFromSyncIteratorValue = class(TGocciaObjectValue)
  private
    FIterator: TGocciaIteratorValue;
    function ResolveIteratorResult(const AResult: TGocciaObjectValue): TGocciaValue;
    function Next(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReturnValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ThrowValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AIterator: TGocciaIteratorValue);
    procedure MarkReferences; override;
  end;

constructor TGocciaGeneratorTryState.Create;
begin
  inherited Create;
  Phase := gtpTry;
  ResultFlow := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TGocciaGeneratorTryState.MarkReferences;
begin
  if Assigned(ResultFlow.Value) then
    ResultFlow.Value.MarkReferences;
  if Assigned(CatchValue) then
    CatchValue.MarkReferences;
  if Assigned(ThrownValue) then
    ThrownValue.MarkReferences;
  if Assigned(GeneratorReturnValue) then
    GeneratorReturnValue.MarkReferences;
end;

constructor TGocciaAsyncFromSyncIteratorValue.Create(const AIterator: TGocciaIteratorValue);
begin
  inherited Create;
  FIterator := AIterator;
  AssignProperty(PROP_NEXT, TGocciaNativeFunctionValue.Create(Next, PROP_NEXT, 1));
  AssignProperty(PROP_RETURN, TGocciaNativeFunctionValue.Create(ReturnValue, PROP_RETURN, 1));
  AssignProperty(PROP_THROW, TGocciaNativeFunctionValue.Create(ThrowValue, PROP_THROW, 1));
end;

function TGocciaAsyncFromSyncIteratorValue.ResolveIteratorResult(
  const AResult: TGocciaObjectValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  try
    Promise.Resolve(AResult);
  except
    Promise.Free;
    raise;
  end;
  Result := Promise;
end;

function TGocciaAsyncFromSyncIteratorValue.Next(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if Assigned(FIterator) then
  begin
    if AArgs.Length > 0 then
    begin
      Value := FIterator.DirectNextValue(AArgs.GetElement(0), Done);
      Result := ResolveIteratorResult(CreateIteratorResult(Value, Done));
    end
    else
      Result := ResolveIteratorResult(FIterator.AdvanceNext);
  end
  else
    Result := ResolveIteratorResult(CreateIteratorResult(
      TGocciaUndefinedLiteralValue.UndefinedValue, True));
end;

function TGocciaAsyncFromSyncIteratorValue.ReturnValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaObjectValue;
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Assigned(FIterator) then
  begin
    IteratorResult := FIterator.ReturnValue(Value);
    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
      FIterator := nil;
    Result := ResolveIteratorResult(IteratorResult);
    Exit;
  end;
  Result := ResolveIteratorResult(CreateIteratorResult(Value, True));
end;

function TGocciaAsyncFromSyncIteratorValue.ThrowValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaObjectValue;
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Assigned(FIterator) then
  begin
    IteratorResult := FIterator.ThrowValue(Value);
    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
      FIterator := nil;
    Result := ResolveIteratorResult(IteratorResult);
    Exit;
  end;
  raise TGocciaThrowValue.Create(Value);
end;

procedure TGocciaAsyncFromSyncIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FIterator) then
    FIterator.MarkReferences;
end;

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
  const ACallScope: TGocciaScope; const AContext: TGocciaEvaluationContext;
  const AIsAsyncGenerator: Boolean = False);
begin
  inherited Create;
  FBodyStatements := ABodyStatements;
  FCallScope := ACallScope;
  FContext := AContext;
  FContext.Scope := ACallScope;
  FStatementIndex := 0;
  FStarted := False;
  FCompleted := False;
  FIsAsyncGenerator := AIsAsyncGenerator;
  FSuspendedYield := nil;
  FPendingValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  FCompletedExpressionValues := TDictionary<TObject, TGocciaValue>.Create;
  FExpressionValues := TDictionary<TObject, TGocciaValue>.Create;
  FStatementIndexes := TDictionary<TObject, Integer>.Create;
  FTryStates := TDictionary<TObject, TGocciaGeneratorTryState>.Create;
end;

destructor TGocciaGeneratorContinuation.Destroy;
begin
  ClearTryStates;
  FTryStates.Free;
  FStatementIndexes.Free;
  FExpressionValues.Free;
  FCompletedExpressionValues.Free;
  inherited;
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
    if AKind = grkReturn then
      Result := AValue
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
          ClearExpressionValues;
          ClearStatementIndexes;
          ClearTryStates;
          ADone := True;
          if Assigned(ControlFlow.Value) then
            Result := ControlFlow.Value
          else
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
          Exit;
        end;

        Inc(FStatementIndex);
        ClearExpressionValues;
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
          ClearExpressionValues;
          ClearStatementIndexes;
          ClearTryStates;
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
  ClearExpressionValues;
  ClearStatementIndexes;
  ClearTryStates;
  ADone := True;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGeneratorContinuation.YieldValue(
  const AYieldExpression: TGocciaYieldExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  HasDelegateResumeValue: Boolean;
  IteratorResult: TGocciaObjectValue;
  IteratorMethod: TGocciaValue;
  RawIteratorResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  YieldedValue: TGocciaValue;
begin
  HasDelegateResumeValue := False;
  if FHasPendingValue and (FSuspendedYield = AYieldExpression) then
  begin
    FHasPendingValue := False;
    FSuspendedYield := nil;
    if AYieldExpression.IsDelegate then
    begin
      if FPendingKind = grkThrow then
      begin
        if Assigned(FDelegateIterator) then
          RawIteratorResult := FDelegateIterator.ThrowValue(FPendingValue)
        else if Assigned(FDelegateAsyncIterator) then
        begin
          IteratorMethod := FDelegateAsyncIterator.GetProperty(PROP_THROW)
        end
        else
          IteratorMethod := nil;
        if not Assigned(FDelegateIterator) then
        begin
          if Assigned(IteratorMethod) and
             not (IteratorMethod is TGocciaUndefinedLiteralValue) and
             not (IteratorMethod is TGocciaNullLiteralValue) then
          begin
            if not IteratorMethod.IsCallable then
            begin
              ReturnMethod := FDelegateAsyncIterator.GetProperty(PROP_RETURN);
              if Assigned(ReturnMethod) and
                 not (ReturnMethod is TGocciaUndefinedLiteralValue) and
                 not (ReturnMethod is TGocciaNullLiteralValue) then
              begin
                if not ReturnMethod.IsCallable then
                  ThrowTypeError('Iterator return is not callable');
                CallArgs := TGocciaArgumentsCollection.Create;
                try
                  RawIteratorResult := AwaitValue(TGocciaFunctionBase(ReturnMethod).Call(
                    CallArgs, FDelegateAsyncIterator));
                finally
                  CallArgs.Free;
                end;
                if not (RawIteratorResult is TGocciaObjectValue) then
                  ThrowTypeError('Iterator return result is not an object');
              end;
              ThrowTypeError('Iterator throw is not callable');
            end;
            CallArgs := TGocciaArgumentsCollection.Create([FPendingValue]);
            try
              RawIteratorResult := AwaitValue(TGocciaFunctionBase(IteratorMethod).Call(
                CallArgs, FDelegateAsyncIterator));
            finally
              CallArgs.Free;
            end;
          end
          else
          begin
            ReturnMethod := nil;
            if Assigned(FDelegateAsyncIterator) then
              ReturnMethod := FDelegateAsyncIterator.GetProperty(PROP_RETURN);
            if Assigned(ReturnMethod) and
               not (ReturnMethod is TGocciaUndefinedLiteralValue) and
               not (ReturnMethod is TGocciaNullLiteralValue) then
            begin
              if not ReturnMethod.IsCallable then
                ThrowTypeError('Iterator return is not callable');
              CallArgs := TGocciaArgumentsCollection.Create;
              try
                RawIteratorResult := AwaitValue(TGocciaFunctionBase(ReturnMethod).Call(
                  CallArgs, FDelegateAsyncIterator));
              finally
                CallArgs.Free;
              end;
              if not (RawIteratorResult is TGocciaObjectValue) then
                ThrowTypeError('Iterator return result is not an object');
            end;
            ThrowTypeError('Delegated iterator has no throw method');
          end;
        end;
        if not (RawIteratorResult is TGocciaObjectValue) then
          ThrowTypeError('Iterator throw result is not an object');
        IteratorResult := TGocciaObjectValue(RawIteratorResult);
        if IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
        begin
          Result := IteratorResult.GetProperty(PROP_VALUE);
          if not Assigned(Result) then
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
          FDelegateIterator := nil;
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

      if FPendingKind = grkReturn then
      begin
        if Assigned(FDelegateIterator) then
          RawIteratorResult := FDelegateIterator.ReturnValue(FPendingValue)
        else if Assigned(FDelegateAsyncIterator) then
        begin
          IteratorMethod := FDelegateAsyncIterator.GetProperty(PROP_RETURN)
        end
        else
          IteratorMethod := nil;
        if not Assigned(FDelegateIterator) then
        begin
          if Assigned(IteratorMethod) and
             not (IteratorMethod is TGocciaUndefinedLiteralValue) and
             not (IteratorMethod is TGocciaNullLiteralValue) then
          begin
            if not IteratorMethod.IsCallable then
              ThrowTypeError('Iterator return is not callable');
            CallArgs := TGocciaArgumentsCollection.Create([FPendingValue]);
            try
              RawIteratorResult := AwaitValue(TGocciaFunctionBase(IteratorMethod).Call(
                CallArgs, FDelegateAsyncIterator));
            finally
              CallArgs.Free;
            end;
          end
          else
            raise EGocciaGeneratorReturn.Create(FPendingValue);
        end;
        if not (RawIteratorResult is TGocciaObjectValue) then
          ThrowTypeError('Iterator return result is not an object');
        IteratorResult := TGocciaObjectValue(RawIteratorResult);
        if not IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
        begin
          YieldedValue := IteratorResult.GetProperty(PROP_VALUE);
          if not Assigned(YieldedValue) then
            YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          FSuspendedYield := AYieldExpression;
          raise EGocciaGeneratorYield.Create(YieldedValue);
        end;
        YieldedValue := IteratorResult.GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        FDelegateIterator := nil;
        FDelegateAsyncIterator := nil;
        FDelegateAsyncNext := nil;
        FDelegateExpression := nil;
        raise EGocciaGeneratorReturn.Create(YieldedValue);
      end;
      HasDelegateResumeValue := FPendingKind = grkNext;
    end;
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
      if FIsAsyncGenerator and (YieldedValue is TGocciaObjectValue) then
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
          if Assigned(FDelegateAsyncIterator) and
             not (FDelegateAsyncIterator is TGocciaObjectValue) then
            ThrowTypeError('Async iterator method returned a non-object');
          if Assigned(FDelegateAsyncIterator) then
            FDelegateAsyncNext := FDelegateAsyncIterator.GetProperty(PROP_NEXT);
        end
        else if Assigned(IteratorMethod) and
                not (IteratorMethod is TGocciaUndefinedLiteralValue) and
                (IteratorMethod is TGocciaObjectValue) then
        begin
          FDelegateAsyncIterator := IteratorMethod;
          FDelegateAsyncNext := FDelegateAsyncIterator.GetProperty(PROP_NEXT);
        end
        else if Assigned(IteratorMethod) and
                not (IteratorMethod is TGocciaUndefinedLiteralValue) then
          ThrowTypeError('Async iterator method is not callable');
      end;

      if not Assigned(FDelegateAsyncIterator) then
      begin
        FDelegateIterator := GetIteratorFromValue(YieldedValue);
        if FIsAsyncGenerator and Assigned(FDelegateIterator) then
        begin
          FDelegateAsyncIterator :=
            TGocciaAsyncFromSyncIteratorValue.Create(FDelegateIterator);
          FDelegateAsyncNext := FDelegateAsyncIterator.GetProperty(PROP_NEXT);
          FDelegateIterator := nil;
        end;
      end;
      FDelegateExpression := AYieldExpression;
    end;

    if Assigned(FDelegateAsyncIterator) then
    begin
      if not Assigned(FDelegateAsyncNext) or not FDelegateAsyncNext.IsCallable then
        ThrowTypeError('Async iterator next is not callable')
      else
      begin
        if HasDelegateResumeValue then
          CallArgs := TGocciaArgumentsCollection.Create([FPendingValue])
        else
          CallArgs := TGocciaArgumentsCollection.Create;
        try
          RawIteratorResult := AwaitValue(TGocciaFunctionBase(FDelegateAsyncNext).Call(
            CallArgs, FDelegateAsyncIterator));
        finally
          CallArgs.Free;
        end;
        if not (RawIteratorResult is TGocciaObjectValue) then
          ThrowTypeError('Async iterator result is not an object');
        IteratorResult := TGocciaObjectValue(RawIteratorResult);

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
      if HasDelegateResumeValue then
        IteratorResult := FDelegateIterator.AdvanceNextValue(FPendingValue)
      else
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

procedure TGocciaGeneratorContinuation.SaveCompletedExpressionValue(
  const AExpression: TObject; const AValue: TGocciaValue);
begin
  FCompletedExpressionValues.AddOrSetValue(AExpression, AValue);
end;

function TGocciaGeneratorContinuation.TakeCompletedExpressionValue(
  const AExpression: TObject; out AValue: TGocciaValue): Boolean;
begin
  Result := FCompletedExpressionValues.TryGetValue(AExpression, AValue);
  if Result then
    FCompletedExpressionValues.Remove(AExpression);
end;

procedure TGocciaGeneratorContinuation.SaveExpressionValue(
  const AExpression: TObject; const AValue: TGocciaValue);
begin
  FExpressionValues.AddOrSetValue(AExpression, AValue);
end;

function TGocciaGeneratorContinuation.TakeExpressionValue(
  const AExpression: TObject; out AValue: TGocciaValue): Boolean;
begin
  Result := FExpressionValues.TryGetValue(AExpression, AValue);
  if Result then
    FExpressionValues.Remove(AExpression);
end;

procedure TGocciaGeneratorContinuation.ClearExpressionValue(const AExpression: TObject);
begin
  FExpressionValues.Remove(AExpression);
  FCompletedExpressionValues.Remove(AExpression);
end;

procedure TGocciaGeneratorContinuation.ClearExpressionValues;
begin
  FExpressionValues.Clear;
  FCompletedExpressionValues.Clear;
end;

function TGocciaGeneratorContinuation.GetStatementIndex(
  const AStatements: TObject): Integer;
begin
  if not FStatementIndexes.TryGetValue(AStatements, Result) then
    Result := 0;
end;

procedure TGocciaGeneratorContinuation.SaveStatementIndex(
  const AStatements: TObject; const AIndex: Integer);
begin
  FStatementIndexes.AddOrSetValue(AStatements, AIndex);
end;

procedure TGocciaGeneratorContinuation.ClearStatementIndex(
  const AStatements: TObject);
begin
  FStatementIndexes.Remove(AStatements);
end;

procedure TGocciaGeneratorContinuation.ClearStatementIndexes;
begin
  FStatementIndexes.Clear;
end;

function TGocciaGeneratorContinuation.EnsureTryState(
  const ATryStatement: TObject): TGocciaGeneratorTryState;
begin
  if not FTryStates.TryGetValue(ATryStatement, Result) then
  begin
    Result := TGocciaGeneratorTryState.Create;
    FTryStates.Add(ATryStatement, Result);
  end;
end;

function TGocciaGeneratorContinuation.GetTryState(
  const ATryStatement: TObject): TGocciaGeneratorTryState;
begin
  if not FTryStates.TryGetValue(ATryStatement, Result) then
    Result := nil;
end;

procedure TGocciaGeneratorContinuation.ClearTryState(
  const ATryStatement: TObject);
var
  TryState: TGocciaGeneratorTryState;
begin
  if FTryStates.TryGetValue(ATryStatement, TryState) then
    TryState.Free;
  FTryStates.Remove(ATryStatement);
end;

procedure TGocciaGeneratorContinuation.ClearTryStates;
var
  TryState: TGocciaGeneratorTryState;
begin
  for TryState in FTryStates.Values do
    TryState.Free;
  FTryStates.Clear;
end;

procedure TGocciaGeneratorContinuation.MarkReferences;
var
  TryState: TGocciaGeneratorTryState;
  Value: TGocciaValue;
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
  for Value in FCompletedExpressionValues.Values do
    if Assigned(Value) then
      Value.MarkReferences;
  for Value in FExpressionValues.Values do
    if Assigned(Value) then
      Value.MarkReferences;
  for TryState in FTryStates.Values do
    if Assigned(TryState) then
      TryState.MarkReferences;
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
