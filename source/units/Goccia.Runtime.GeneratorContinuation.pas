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

  TGocciaGeneratorLoopState = class
  public
    IteratorValue: TGocciaValue;
    CurrentValue: TGocciaValue;
    NextMethod: TGocciaValue;
    constructor Create(const AIteratorValue, ACurrentValue,
      ANextMethod: TGocciaValue);
    procedure Assign(const AIteratorValue, ACurrentValue,
      ANextMethod: TGocciaValue);
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
    FLoopStates: TDictionary<TObject, TGocciaGeneratorLoopState>;
    FIsAsyncGenerator: Boolean;
    procedure ClearDelegateState;
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
    procedure SaveLoopState(const ALoopStatement: TObject;
      const AIteratorValue, ACurrentValue: TGocciaValue;
      const ANextMethod: TGocciaValue = nil);
    function GetLoopState(const ALoopStatement: TObject;
      out AIteratorValue, ACurrentValue, ANextMethod: TGocciaValue): Boolean;
    procedure ClearLoopState(const ALoopStatement: TObject);
    procedure ClearLoopStates;
    procedure MarkReferences;
    property Completed: Boolean read FCompleted;
  end;

function CurrentGeneratorContinuation: TGocciaGeneratorContinuation;
function EvaluateGeneratorYield(const AYieldExpression: TGocciaYieldExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.GarbageCollector,
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
    function PromiseReject(const AValue: TGocciaValue): TGocciaValue;
    function ResolveIteratorResult(const AResult: TGocciaObjectValue;
      const ACloseOnRejection: Boolean): TGocciaValue;
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

constructor TGocciaGeneratorLoopState.Create(const AIteratorValue,
  ACurrentValue, ANextMethod: TGocciaValue);
begin
  inherited Create;
  Assign(AIteratorValue, ACurrentValue, ANextMethod);
end;

procedure TGocciaGeneratorLoopState.Assign(const AIteratorValue,
  ACurrentValue, ANextMethod: TGocciaValue);
begin
  IteratorValue := AIteratorValue;
  CurrentValue := ACurrentValue;
  NextMethod := ANextMethod;
end;

procedure TGocciaGeneratorLoopState.MarkReferences;
begin
  if Assigned(IteratorValue) then
    IteratorValue.MarkReferences;
  if Assigned(CurrentValue) then
    CurrentValue.MarkReferences;
  if Assigned(NextMethod) then
    NextMethod.MarkReferences;
end;

constructor TGocciaAsyncFromSyncIteratorValue.Create(const AIterator: TGocciaIteratorValue);
begin
  inherited Create;
  FIterator := AIterator;
  AssignProperty(PROP_NEXT, TGocciaNativeFunctionValue.Create(Next, PROP_NEXT, 1));
  AssignProperty(PROP_RETURN, TGocciaNativeFunctionValue.Create(ReturnValue, PROP_RETURN, 1));
  AssignProperty(PROP_THROW, TGocciaNativeFunctionValue.Create(ThrowValue, PROP_THROW, 1));
end;

function TGocciaAsyncFromSyncIteratorValue.PromiseReject(
  const AValue: TGocciaValue): TGocciaValue;
var
  IsRooted: Boolean;
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    Promise.Reject(AValue);
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
      IsRooted := False;
    end;
    Promise.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
  Result := Promise;
end;

function TGocciaAsyncFromSyncIteratorValue.ResolveIteratorResult(
  const AResult: TGocciaObjectValue; const ACloseOnRejection: Boolean): TGocciaValue;
var
  Done: Boolean;
  DoneValue: TGocciaValue;
  IsRooted: Boolean;
  Promise: TGocciaPromiseValue;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    try
      DoneValue := AResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := AResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      try
        UnwrappedValue := AwaitValue(Value);
      except
        if ACloseOnRejection and not Done then
        begin
          CloseIteratorPreservingError(FIterator);
          FIterator := nil;
        end;
        raise;
      end;
      Promise.Resolve(CreateIteratorResult(UnwrappedValue, Done));
    except
      on E: TGocciaThrowValue do
        Promise.Reject(E.Value);
      on E: TGocciaTypeError do
        Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
      on E: TGocciaReferenceError do
        Promise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
      on E: TGocciaSyntaxError do
        Promise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
      on E: Exception do
        Promise.Reject(CreateErrorObject(ERROR_NAME, E.Message));
    end;
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
      IsRooted := False;
    end;
    Promise.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
  Result := Promise;
end;

function TGocciaAsyncFromSyncIteratorValue.Next(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaObjectValue;
  Value: TGocciaValue;
begin
  try
    if Assigned(FIterator) then
    begin
      if AArgs.Length > 0 then
      begin
        Value := FIterator.DirectNextValue(AArgs.GetElement(0), Done);
        if Done then
          FIterator := nil;
        Result := ResolveIteratorResult(CreateIteratorResult(Value, Done), True);
      end
      else
      begin
        IteratorResult := FIterator.AdvanceNext;
        DoneValue := IteratorResult.GetProperty(PROP_DONE);
        if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
          FIterator := nil;
        Result := ResolveIteratorResult(IteratorResult, True);
      end;
    end
    else
      Result := ResolveIteratorResult(CreateIteratorResult(
        TGocciaUndefinedLiteralValue.UndefinedValue, True), True);
  except
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
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
  try
    if Assigned(FIterator) then
    begin
      IteratorResult := FIterator.ReturnValue(Value);
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
        FIterator := nil;
      Result := ResolveIteratorResult(IteratorResult, False);
      Exit;
    end;
    Result := ResolveIteratorResult(CreateIteratorResult(Value, True), False);
  except
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
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
  try
    if Assigned(FIterator) then
    begin
      IteratorResult := FIterator.ThrowValue(Value);
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
        FIterator := nil;
      Result := ResolveIteratorResult(IteratorResult, True);
      Exit;
    end;
    Result := PromiseReject(Value);
  except
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
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
  FLoopStates := TDictionary<TObject, TGocciaGeneratorLoopState>.Create;
end;

destructor TGocciaGeneratorContinuation.Destroy;
begin
  ClearLoopStates;
  FLoopStates.Free;
  ClearTryStates;
  FTryStates.Free;
  FStatementIndexes.Free;
  FExpressionValues.Free;
  FCompletedExpressionValues.Free;
  inherited;
end;

procedure TGocciaGeneratorContinuation.ClearDelegateState;
begin
  FDelegateIterator := nil;
  FDelegateAsyncIterator := nil;
  FDelegateAsyncNext := nil;
  FDelegateExpression := nil;
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
          ClearDelegateState;
          ClearExpressionValues;
          ClearStatementIndexes;
          ClearTryStates;
          ClearLoopStates;
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
          ClearDelegateState;
          ClearExpressionValues;
          ClearStatementIndexes;
          ClearTryStates;
          ClearLoopStates;
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
  ClearDelegateState;
  ClearExpressionValues;
  ClearStatementIndexes;
  ClearTryStates;
  ClearLoopStates;
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
  try
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
          ClearDelegateState;
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
        ClearDelegateState;
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

      ClearDelegateState;
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
          ClearDelegateState;
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
        ClearDelegateState;
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
  except
    on E: EGocciaGeneratorYield do
      raise;
    else
    begin
      if AYieldExpression.IsDelegate then
        ClearDelegateState;
      raise;
    end;
  end;
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
    // Consume replayed completed values; statement completion clears the rest.
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

procedure TGocciaGeneratorContinuation.SaveLoopState(
  const ALoopStatement: TObject; const AIteratorValue,
  ACurrentValue: TGocciaValue; const ANextMethod: TGocciaValue);
var
  LoopState: TGocciaGeneratorLoopState;
begin
  if FLoopStates.TryGetValue(ALoopStatement, LoopState) then
    LoopState.Assign(AIteratorValue, ACurrentValue, ANextMethod)
  else
    FLoopStates.Add(ALoopStatement,
      TGocciaGeneratorLoopState.Create(AIteratorValue, ACurrentValue,
        ANextMethod));
end;

function TGocciaGeneratorContinuation.GetLoopState(
  const ALoopStatement: TObject; out AIteratorValue, ACurrentValue,
  ANextMethod: TGocciaValue): Boolean;
var
  LoopState: TGocciaGeneratorLoopState;
begin
  Result := FLoopStates.TryGetValue(ALoopStatement, LoopState);
  if Result then
  begin
    AIteratorValue := LoopState.IteratorValue;
    ACurrentValue := LoopState.CurrentValue;
    ANextMethod := LoopState.NextMethod;
  end
  else
  begin
    AIteratorValue := nil;
    ACurrentValue := nil;
    ANextMethod := nil;
  end;
end;

procedure TGocciaGeneratorContinuation.ClearLoopState(
  const ALoopStatement: TObject);
var
  LoopState: TGocciaGeneratorLoopState;
begin
  if FLoopStates.TryGetValue(ALoopStatement, LoopState) then
    LoopState.Free;
  FLoopStates.Remove(ALoopStatement);
end;

procedure TGocciaGeneratorContinuation.ClearLoopStates;
var
  LoopState: TGocciaGeneratorLoopState;
begin
  for LoopState in FLoopStates.Values do
    LoopState.Free;
  FLoopStates.Clear;
end;

procedure TGocciaGeneratorContinuation.MarkReferences;
var
  LoopState: TGocciaGeneratorLoopState;
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
  for LoopState in FLoopStates.Values do
    if Assigned(LoopState) then
      LoopState.MarkReferences;
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
