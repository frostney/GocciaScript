// The tree-walk interpreter's generator objects and the call path that
// creates them.
//
// Goccia.Values.GeneratorValue holds what both executors share: the generator
// prototypes, their next/return/throw methods, and the generator function
// value classes. Calling one of those functions creates an object defined
// here, which resumes a TGocciaGeneratorContinuation over the function's AST.
// This unit registers that call path with RegisterGeneratorObjectFactory when
// it is linked; Goccia.Evaluator, the only unit that creates generator
// function values, uses it.
unit Goccia.Evaluator.Generators;

{$I Goccia.inc}

interface

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arithmetic,
  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Constants,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.EngineFault,
  Goccia.Evaluator,
  Goccia.Evaluator.Calls,
  Goccia.Evaluator.Context,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.InstructionLimit,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.MemoryLimit,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.Declarations,
  Goccia.Timeout,
  Goccia.Types.Enforcement,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.GeneratorValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

type
  TGocciaGeneratorState = (gsSuspendedStart, gsSuspendedYield, gsExecuting, gsCompleted);

  TGocciaAsyncGeneratorRequest = record
    Kind: TGocciaGeneratorResumeKind;
    Value: TGocciaValue;
    Promise: TGocciaPromiseValue;
  end;

  TGocciaGeneratorObjectValue = class(TGocciaGeneratorBaseValue)
  private
    FContinuation: TGocciaGeneratorContinuation;
    FState: TGocciaGeneratorState;
  public
    constructor Create(const AContinuation: TGocciaGeneratorContinuation);
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function DirectNextValue(const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue; override;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function ThrowValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    procedure Close; override;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorObjectValue = class(TGocciaAsyncGeneratorBaseValue)
  private
    FContinuation: TGocciaGeneratorContinuation;
    FState: TGocciaGeneratorState;
    { Async-generator request queue. DUPLICATED: TGocciaBytecodeAsyncGeneratorObjectValue
      in Goccia.VM
      carries the same queue with the same discipline for the other executor,
      and the two must stay in step — a change to one is a bug in the other
      until it is made there too.

      Async context is deliberately NOT recorded per request. A body observes
      the context of whichever call resumed it, and that falls out of the two
      execution paths without any per-request bookkeeping: a request that finds
      the queue idle is started synchronously on the resuming call's own stack,
      under that call's context, while a request that had to wait — a queued
      second next(), or a body suspended on await — reaches the body through a
      promise reaction, which carries the snapshot captured where it was
      registered.
      Probed against Node v24.0.1 across for-await, a generator created in one
      context and resumed in another, a queued second request overlapping a
      running one, and nested for-await under different stores; both executors
      match Node on all of them. See tests/built-ins/AsyncHooks/
      async-generators.js, which locks that in, and ADR 0111. }
    FQueue: array of TGocciaAsyncGeneratorRequest;
    FQueueHead: Integer;
    FQueueCount: Integer;
    FQueueRunning: Boolean;
    function ResumeAsPromise(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue): TGocciaValue;
    function DequeueRequest(out ARequest: TGocciaAsyncGeneratorRequest): Boolean;
    function PeekRequest(out ARequest: TGocciaAsyncGeneratorRequest): Boolean;
    procedure EnqueueRequest(const ARequest: TGocciaAsyncGeneratorRequest);
    procedure FinishRequest;
    procedure ProcessQueue;
    procedure StartRequest(const ARequest: TGocciaAsyncGeneratorRequest);
    procedure AttachBodyAwait(const ASuspension: EGocciaAsyncAwaitSuspend);
    procedure ContinueBodyAwait(const AValue: TGocciaValue;
      const AIsThrow: Boolean);
    procedure AwaitYieldValue(const AValue: TGocciaValue);
    procedure AwaitReturnValue(const AValue: TGocciaValue;
      const AThrowIntoGenerator: Boolean = False;
      const AResolvedPromise: TGocciaPromiseValue = nil);
    procedure CompleteCurrentRequest(const AValue: TGocciaValue;
      const AIsThrow, ADone: Boolean);
    procedure ResolveAwaitedYield(const AValue: TGocciaValue);
    procedure RejectAwaitedYield(const AReason: TGocciaValue);
    procedure ResolveAwaitedReturn(const AValue: TGocciaValue);
    procedure RejectAwaitedReturn(const AReason: TGocciaValue);
  public
    constructor Create(const AContinuation: TGocciaGeneratorContinuation);
    destructor Destroy; override;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAsyncGeneratorAwaitReturnHandler = class(TGocciaObjectValue)
  private
    FGenerator: TGocciaAsyncGeneratorObjectValue;
    FReject: Boolean;
    FReturn: Boolean;
  public
    constructor Create(const AGenerator: TGocciaAsyncGeneratorObjectValue;
      const AReject: Boolean; const AReturn: Boolean = False);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaAsyncGeneratorBodyAwaitHandler = class(TGocciaObjectValue)
  private
    FGenerator: TGocciaAsyncGeneratorObjectValue;
    FReject: Boolean;
  public
    constructor Create(const AGenerator: TGocciaAsyncGeneratorObjectValue;
      const AReject: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

function ArgumentOrUndefined(const AArguments: TGocciaArgumentsCollection): TGocciaValue;
begin
  if AArguments.Length > 0 then
    Result := AArguments.GetElement(0)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function GeneratorExecutingError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME, 'Generator is already executing');
end;

function PromiseConstructorIntrinsic: TGocciaValue;
var
  GlobalScope: TGocciaScope;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if (CurrentRealm = nil) or
     not (CurrentRealm.GlobalEnv is TGocciaScope) then
    Exit;

  GlobalScope := TGocciaScope(CurrentRealm.GlobalEnv);
  if GlobalScope.Contains(CONSTRUCTOR_PROMISE) then
    Result := GlobalScope.GetValue(CONSTRUCTOR_PROMISE);
end;

function AsyncGeneratorPromiseResolve(
  const AValue: TGocciaValue): TGocciaPromiseValue;
var
  ConstructorValue: TGocciaValue;
  IsRooted: Boolean;
  ValueConstructor: TGocciaValue;
begin
  ConstructorValue := PromiseConstructorIntrinsic;
  if AValue is TGocciaPromiseValue then
  begin
    ValueConstructor := TGocciaPromiseValue(AValue).GetProperty(
      PROP_CONSTRUCTOR);
    if IsSameValue(ValueConstructor, ConstructorValue) then
      Exit(TGocciaPromiseValue(AValue));
  end;

  Result := TGocciaPromiseValue.Create;
  IsRooted := (TGarbageCollector.Instance <> nil);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Result);
  try
    Result.Resolve(AValue);
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Result);
      IsRooted := False;
    end;
    Result.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Result);
end;

function ExceptionToErrorValue(const AException: Exception): TGocciaValue;
var
  MessageText: string;
begin
  MessageText := AException.Message;
  if Copy(MessageText, 1, 7) = 'Error: ' then
    Delete(MessageText, 1, 7);
  Result := CreateErrorObject(ERROR_NAME, MessageText);
end;

constructor TGocciaAsyncGeneratorAwaitReturnHandler.Create(
  const AGenerator: TGocciaAsyncGeneratorObjectValue; const AReject: Boolean;
  const AReturn: Boolean = False);
begin
  inherited Create(nil);
  FGenerator := AGenerator;
  FReject := AReject;
  FReturn := AReturn;
end;

function TGocciaAsyncGeneratorAwaitReturnHandler.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FGenerator) then
    Exit;

  Value := ArgumentOrUndefined(AArgs);
  if FReturn and FReject then
    FGenerator.RejectAwaitedReturn(Value)
  else if FReturn then
    FGenerator.ResolveAwaitedReturn(Value)
  else if FReject then
    FGenerator.RejectAwaitedYield(Value)
  else
    FGenerator.ResolveAwaitedYield(Value);
end;

procedure TGocciaAsyncGeneratorAwaitReturnHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FGenerator) then
    FGenerator.MarkReferences;
end;

constructor TGocciaAsyncGeneratorBodyAwaitHandler.Create(
  const AGenerator: TGocciaAsyncGeneratorObjectValue; const AReject: Boolean);
begin
  inherited Create(nil);
  FGenerator := AGenerator;
  FReject := AReject;
end;

function TGocciaAsyncGeneratorBodyAwaitHandler.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FGenerator) then
    Exit;

  FGenerator.ContinueBodyAwait(ArgumentOrUndefined(AArgs), FReject);
end;

procedure TGocciaAsyncGeneratorBodyAwaitHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FGenerator) then
    FGenerator.MarkReferences;
end;

{ TGocciaGeneratorObjectValue }

constructor TGocciaGeneratorObjectValue.Create(const AContinuation: TGocciaGeneratorContinuation);
begin
  inherited Create;
  FContinuation := AContinuation;
  FState := gsSuspendedStart;
  Prototype := GeneratorObjectIntrinsicParent(foikGenerator);
end;

destructor TGocciaGeneratorObjectValue.Destroy;
begin
  FContinuation.Free;
  inherited;
end;

function TGocciaGeneratorObjectValue.AdvanceNext: TGocciaObjectValue;
begin
  Result := AdvanceNextValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaGeneratorObjectValue.AdvanceNextValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  if FState = gsCompleted then
    Exit(CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True));
  if FState = gsExecuting then
    raise TGocciaThrowValue.Create(GeneratorExecutingError);
  FState := gsExecuting;
  try
    ResultValue := FContinuation.Resume(grkNext, AValue, Done);
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;
  except
    FState := gsCompleted;
    raise;
  end;
  Result := CreateIteratorResult(ResultValue, Done);
end;

function TGocciaGeneratorObjectValue.DirectNext(out ADone: Boolean): TGocciaValue;
begin
  Result := DirectNextValue(TGocciaUndefinedLiteralValue.UndefinedValue, ADone);
end;

function TGocciaGeneratorObjectValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNextValue(AValue);
  ADone := IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value;
  Result := IteratorResult.GetProperty(PROP_VALUE);
end;

function TGocciaGeneratorObjectValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  if FState = gsCompleted then
    Exit(CreateIteratorResult(AValue, True));
  if FState = gsExecuting then
    raise TGocciaThrowValue.Create(GeneratorExecutingError);
  FState := gsExecuting;
  try
    ResultValue := FContinuation.Resume(grkReturn, AValue, Done);
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;
  except
    FState := gsCompleted;
    raise;
  end;
  if (not Done) and FContinuation.LastYieldWasIteratorResult and
     (ResultValue is TGocciaObjectValue) then
    Exit(TGocciaObjectValue(ResultValue));
  Result := CreateIteratorResult(ResultValue, Done);
end;

function TGocciaGeneratorObjectValue.ThrowValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  if FState = gsExecuting then
    raise TGocciaThrowValue.Create(GeneratorExecutingError);
  FState := gsExecuting;
  try
    ResultValue := FContinuation.Resume(grkThrow, AValue, Done);
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;
  except
    FState := gsCompleted;
    raise;
  end;
  Result := CreateIteratorResult(ResultValue, Done);
end;

procedure TGocciaGeneratorObjectValue.Close;
begin
  if FState = gsCompleted then
    Exit;
  ReturnValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TGocciaGeneratorObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
end;

function TGocciaGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'Generator';
end;

function TGocciaGeneratorObjectValue.GeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  with TGocciaGeneratorObjectValue(AThisValue) do
  begin
    if FState = gsCompleted then
      Exit(CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True));
    if FState = gsExecuting then
      raise TGocciaThrowValue.Create(GeneratorExecutingError);
    FState := gsExecuting;
    try
      Value := FContinuation.Resume(grkNext, ArgumentOrUndefined(AArgs), Done);
      if Done then
        FState := gsCompleted
      else
        FState := gsSuspendedYield;
    except
      FState := gsCompleted;
      raise;
    end;
  end;
  Result := CreateIteratorResult(Value, Done);
end;

function TGocciaGeneratorObjectValue.GeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError)
  else
  begin
    with TGocciaGeneratorObjectValue(AThisValue) do
    begin
      if FState = gsCompleted then
        Exit(CreateIteratorResult(ArgumentOrUndefined(AArgs), True));
      if FState = gsExecuting then
        raise TGocciaThrowValue.Create(GeneratorExecutingError);
      FState := gsExecuting;
      try
        Value := FContinuation.Resume(grkReturn, ArgumentOrUndefined(AArgs), Done);
        if Done then
          FState := gsCompleted
        else
          FState := gsSuspendedYield;
      except
        FState := gsCompleted;
        raise;
      end;
    end;
    if (not Done) and FContinuation.LastYieldWasIteratorResult and
       (Value is TGocciaObjectValue) then
      Exit(Value);
    Result := CreateIteratorResult(Value, Done);
  end;
end;

function TGocciaGeneratorObjectValue.GeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  if TGocciaGeneratorObjectValue(AThisValue).FState = gsExecuting then
    raise TGocciaThrowValue.Create(GeneratorExecutingError);
  TGocciaGeneratorObjectValue(AThisValue).FState := gsExecuting;
  try
    Value := TGocciaGeneratorObjectValue(AThisValue).FContinuation.Resume(
      grkThrow, ArgumentOrUndefined(AArgs), Done);
    if Done then
      TGocciaGeneratorObjectValue(AThisValue).FState := gsCompleted
    else
      TGocciaGeneratorObjectValue(AThisValue).FState := gsSuspendedYield;
  except
    TGocciaGeneratorObjectValue(AThisValue).FState := gsCompleted;
    raise;
  end;
  Result := CreateIteratorResult(Value, Done);
end;

{ TGocciaAsyncGeneratorObjectValue }

constructor TGocciaAsyncGeneratorObjectValue.Create(
  const AContinuation: TGocciaGeneratorContinuation);
begin
  inherited Create(nil);
  FContinuation := AContinuation;
  FState := gsSuspendedStart;
  Prototype := GeneratorObjectIntrinsicParent(foikAsyncGenerator);
end;

destructor TGocciaAsyncGeneratorObjectValue.Destroy;
begin
  FContinuation.Free;
  inherited;
end;

function TGocciaAsyncGeneratorObjectValue.ResumeAsPromise(
  const AKind: TGocciaGeneratorResumeKind; const AValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Request: TGocciaAsyncGeneratorRequest;
begin
  Promise := TGocciaPromiseValue.Create;
  Request.Kind := AKind;
  Request.Value := AValue;
  Request.Promise := Promise;
  EnqueueRequest(Request);
  ProcessQueue;
  Result := Promise;
end;

function TGocciaAsyncGeneratorObjectValue.DequeueRequest(
  out ARequest: TGocciaAsyncGeneratorRequest): Boolean;
begin
  Result := FQueueCount > 0;
  if not Result then
    Exit;

  ARequest := FQueue[FQueueHead];
  FQueue[FQueueHead].Value := nil;
  FQueue[FQueueHead].Promise := nil;
  Inc(FQueueHead);
  Dec(FQueueCount);
  if FQueueCount = 0 then
    FQueueHead := 0;
end;

function TGocciaAsyncGeneratorObjectValue.PeekRequest(
  out ARequest: TGocciaAsyncGeneratorRequest): Boolean;
begin
  Result := FQueueCount > 0;
  if Result then
    ARequest := FQueue[FQueueHead];
end;

procedure TGocciaAsyncGeneratorObjectValue.EnqueueRequest(
  const ARequest: TGocciaAsyncGeneratorRequest);
var
  I: Integer;
begin
  if FQueueHead > 0 then
  begin
    for I := 0 to FQueueCount - 1 do
      FQueue[I] := FQueue[FQueueHead + I];
    for I := FQueueCount to High(FQueue) do
    begin
      FQueue[I].Value := nil;
      FQueue[I].Promise := nil;
    end;
    FQueueHead := 0;
  end;

  if FQueueCount >= Length(FQueue) then
    SetLength(FQueue, FQueueCount * 2 + 4);

  FQueue[FQueueCount] := ARequest;
  Inc(FQueueCount);
end;

procedure TGocciaAsyncGeneratorObjectValue.FinishRequest;
begin
  FQueueRunning := False;
  ProcessQueue;
end;

procedure TGocciaAsyncGeneratorObjectValue.ProcessQueue;
var
  Request: TGocciaAsyncGeneratorRequest;
begin
  if FQueueRunning then
    Exit;
  if not PeekRequest(Request) then
    Exit;

  FQueueRunning := True;
  StartRequest(Request);
end;

procedure TGocciaAsyncGeneratorObjectValue.AttachBodyAwait(
  const ASuspension: EGocciaAsyncAwaitSuspend);
var
  FulfillFunction: TGocciaNativeFunctionValue;
  FulfillHandler: TGocciaAsyncGeneratorBodyAwaitHandler;
  RejectFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaAsyncGeneratorBodyAwaitHandler;
begin
  FulfillHandler := TGocciaAsyncGeneratorBodyAwaitHandler.Create(Self, False);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-generator-body-await-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectHandler := TGocciaAsyncGeneratorBodyAwaitHandler.Create(Self, True);
  RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectHandler.Invoke, 'async-generator-body-await-reject', 1);
  RejectFunction.CapturedRoot := RejectHandler;

  ASuspension.Promise.InvokeThen(FulfillFunction, RejectFunction);
end;

procedure TGocciaAsyncGeneratorObjectValue.ContinueBodyAwait(
  const AValue: TGocciaValue; const AIsThrow: Boolean);
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  try
    FState := gsExecuting;
    PushAsyncAwaitSuspension;
    try
      if AIsThrow then
        ResultValue := FContinuation.Resume(grkThrow, AValue, Done)
      else
        ResultValue := FContinuation.Resume(grkNext, AValue, Done);
    finally
      PopAsyncAwaitSuspension;
    end;

    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;

    if Done then
    begin
      if FContinuation.ReturnRequiresAwait then
      begin
        AwaitReturnValue(ResultValue, True);
        Exit;
      end;
      CompleteCurrentRequest(ResultValue, False, True);
    end
    else if FContinuation.LastYieldWasDelegate then
      CompleteCurrentRequest(ResultValue, False, False)
    else
      AwaitYieldValue(ResultValue);
  except
    on E: EGocciaAsyncAwaitSuspend do
      AttachBodyAwait(E);
    on E: TGocciaThrowValue do
    begin
      FState := gsCompleted;
      CompleteCurrentRequest(E.Value, True, True);
    end;
  end;
end;

procedure TGocciaAsyncGeneratorObjectValue.AwaitYieldValue(
  const AValue: TGocciaValue);
var
  AwaitPromise: TGocciaPromiseValue;
  FulfillFunction: TGocciaNativeFunctionValue;
  FulfillHandler: TGocciaAsyncGeneratorAwaitReturnHandler;
  RejectFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaAsyncGeneratorAwaitReturnHandler;
begin
  try
    AwaitPromise := AsyncGeneratorPromiseResolve(AValue);
  except
    on E: TGocciaThrowValue do
    begin
      RejectAwaitedYield(E.Value);
      Exit;
    end;
  end;

  FulfillHandler := TGocciaAsyncGeneratorAwaitReturnHandler.Create(
    Self, False, False);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-generator-yield-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectHandler := TGocciaAsyncGeneratorAwaitReturnHandler.Create(
    Self, True, False);
  RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectHandler.Invoke, 'async-generator-yield-reject', 1);
  RejectFunction.CapturedRoot := RejectHandler;

  AwaitPromise.InvokeThen(FulfillFunction, RejectFunction);
end;

// ES2026 §27.6.3.9 AsyncGeneratorAwaitReturn(gen)
procedure TGocciaAsyncGeneratorObjectValue.AwaitReturnValue(
  const AValue: TGocciaValue; const AThrowIntoGenerator: Boolean = False;
  const AResolvedPromise: TGocciaPromiseValue = nil);
var
  AwaitPromise: TGocciaPromiseValue;
  FulfillFunction: TGocciaNativeFunctionValue;
  FulfillHandler: TGocciaAsyncGeneratorAwaitReturnHandler;
  RejectFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaAsyncGeneratorAwaitReturnHandler;
begin
  if Assigned(AResolvedPromise) then
    AwaitPromise := AResolvedPromise
  else
  begin
    try
      AwaitPromise := AsyncGeneratorPromiseResolve(AValue);
    except
      on E: TGocciaThrowValue do
      begin
        if AThrowIntoGenerator then
          RejectAwaitedYield(E.Value)
        else
          RejectAwaitedReturn(E.Value);
        Exit;
      end;
      { A resource ceiling reached while resolving the return/yield value —
        e.g. `.return(thenable)` whose `then` getter allocates past the budget,
        or an instruction/timeout deadline expiring mid-resolution — is opaque
        to the guest. Named before the generic arm (Pascal first-match), which
        would otherwise fold it into a promise rejection the guest can catch. }
      on E: TGocciaTimeoutError do
        raise;
      on E: TGocciaInstructionLimitError do
        raise;
      on E: TGocciaMemoryLimitError do
        raise;
      on E: Exception do
      begin
        if IsEngineIntegrityFault(E) then
          raise;
        if AThrowIntoGenerator then
          RejectAwaitedYield(ExceptionToErrorValue(E))
        else
          RejectAwaitedReturn(ExceptionToErrorValue(E));
        Exit;
      end;
    end;
  end;

  FulfillHandler := TGocciaAsyncGeneratorAwaitReturnHandler.Create(
    Self, False, True);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-generator-return-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectHandler := TGocciaAsyncGeneratorAwaitReturnHandler.Create(
    Self, True, True);
  RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectHandler.Invoke, 'async-generator-return-reject', 1);
  RejectFunction.CapturedRoot := RejectHandler;

  AwaitPromise.InvokeThen(FulfillFunction, RejectFunction);
end;

// ES2026 §27.6.3.5 AsyncGeneratorCompleteStep(gen, completion, done)
procedure TGocciaAsyncGeneratorObjectValue.CompleteCurrentRequest(
  const AValue: TGocciaValue; const AIsThrow, ADone: Boolean);
var
  Request: TGocciaAsyncGeneratorRequest;
begin
  if not DequeueRequest(Request) then
  begin
    FQueueRunning := False;
    Exit;
  end;

  if AIsThrow then
    Request.Promise.Reject(AValue)
  else
  Request.Promise.Resolve(CreateIteratorResult(AValue, ADone));
  FinishRequest;
end;

procedure TGocciaAsyncGeneratorObjectValue.ResolveAwaitedYield(
  const AValue: TGocciaValue);
begin
  CompleteCurrentRequest(AValue, False, False);
end;

procedure TGocciaAsyncGeneratorObjectValue.RejectAwaitedYield(
  const AReason: TGocciaValue);
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  try
    FState := gsExecuting;
    PushAsyncAwaitSuspension;
    try
      ResultValue := FContinuation.Resume(grkThrow, AReason, Done);
    finally
      PopAsyncAwaitSuspension;
    end;
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;

    if Done then
    begin
      if FContinuation.ReturnRequiresAwait then
      begin
        AwaitReturnValue(ResultValue, True);
        Exit;
      end;
      CompleteCurrentRequest(ResultValue, False, True);
    end
    else if FContinuation.LastYieldWasDelegate then
      CompleteCurrentRequest(ResultValue, False, False)
    else
      AwaitYieldValue(ResultValue);
  except
    on E: EGocciaAsyncAwaitSuspend do
      AttachBodyAwait(E);
    on E: TGocciaThrowValue do
    begin
      FState := gsCompleted;
      CompleteCurrentRequest(E.Value, True, True);
    end;
  end;
end;

procedure TGocciaAsyncGeneratorObjectValue.ResolveAwaitedReturn(
  const AValue: TGocciaValue);
begin
  CompleteCurrentRequest(AValue, False, True);
end;

procedure TGocciaAsyncGeneratorObjectValue.RejectAwaitedReturn(
  const AReason: TGocciaValue);
begin
  CompleteCurrentRequest(AReason, True, True);
end;

procedure TGocciaAsyncGeneratorObjectValue.StartRequest(
  const ARequest: TGocciaAsyncGeneratorRequest);
var
  CheckedReturnPromise: TGocciaPromiseValue;
  Done: Boolean;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  CheckedReturnPromise := nil;
  try
    if (ARequest.Kind = grkReturn) and
       (FState in [gsSuspendedStart, gsCompleted]) then
    begin
      FState := gsCompleted;
      AwaitReturnValue(ARequest.Value);
      Exit;
    end;

    if FState = gsCompleted then
    begin
      case ARequest.Kind of
        grkNext:
          begin
            CompleteCurrentRequest(TGocciaUndefinedLiteralValue.UndefinedValue,
              False, True);
            Exit;
          end;
        grkThrow:
          begin
            CompleteCurrentRequest(ARequest.Value, True, True);
            Exit;
          end;
      else
        begin
          AwaitReturnValue(ARequest.Value);
          Exit;
        end;
      end;
    end;

    if (ARequest.Kind = grkReturn) and
       (FState = gsSuspendedYield) and
       (ARequest.Value is TGocciaPromiseValue) then
    begin
      try
        CheckedReturnPromise := AsyncGeneratorPromiseResolve(ARequest.Value);
      except
        on E: TGocciaThrowValue do
        begin
          RejectAwaitedYield(E.Value);
          Exit;
        end;
        { The limit family stays opaque here too: resolving a promise-valued
          `.return()` runs guest code (a thenable's `then` getter) that can
          hit the budget, and the generic arm below would hand that ceiling
          back to the guest as a catchable rejection. First-match ordering. }
        on E: TGocciaTimeoutError do
          raise;
        on E: TGocciaInstructionLimitError do
          raise;
        on E: TGocciaMemoryLimitError do
          raise;
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          RejectAwaitedYield(ExceptionToErrorValue(E));
          Exit;
        end;
      end;
    end;

    FState := gsExecuting;
    PushAsyncAwaitSuspension;
    try
      Value := FContinuation.Resume(ARequest.Kind, ARequest.Value, Done);
    finally
      PopAsyncAwaitSuspension;
    end;
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;

    if Done then
    begin
      if FContinuation.ReturnRequiresAwait then
      begin
        if Assigned(CheckedReturnPromise) and
           IsSameValue(Value, ARequest.Value) then
          AwaitReturnValue(Value, True, CheckedReturnPromise)
        else
          AwaitReturnValue(Value, True);
        Exit;
      end;
      UnwrappedValue := Value;
    end
    else if FContinuation.LastYieldWasDelegate then
      UnwrappedValue := Value
    else
    begin
      AwaitYieldValue(Value);
      Exit;
    end;

    CompleteCurrentRequest(UnwrappedValue, False, Done);
  except
    on E: EGocciaAsyncAwaitSuspend do
    begin
      AttachBodyAwait(E);
    end;
    on E: TGocciaThrowValue do
    begin
      FState := gsCompleted;
      CompleteCurrentRequest(E.Value, True, True);
    end;
    { A `for await ... break` runs the body's `finally` as part of the
      generator's `.return()`; a resource ceiling raised there reaches this
      handler on the grkReturn path and, without these arms, was converted
      into a rejection the guest could catch (and, in bytecode, diverged to a
      fatal — a mode split). The ceiling must keep unwinding to the host in
      both executors. Named before the generic arm (Pascal first-match). }
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: TGocciaMemoryLimitError do
      raise;
    on E: Exception do
    begin
      if IsEngineIntegrityFault(E) then
        raise;
      if ARequest.Kind = grkReturn then
      begin
        FState := gsCompleted;
        CompleteCurrentRequest(ExceptionToErrorValue(E), True, True);
        Exit;
      end;
      raise;
    end;
  end;
end;

procedure TGocciaAsyncGeneratorObjectValue.MarkReferences;
var
  I: Integer;
  Index: Integer;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
  for I := 0 to FQueueCount - 1 do
  begin
    Index := FQueueHead + I;
    if (Index < 0) or (Index > High(FQueue)) then
      Continue;
    if Assigned(FQueue[Index].Value) then
      FQueue[Index].Value.MarkReferences;
    if Assigned(FQueue[Index].Promise) then
      FQueue[Index].Promise.MarkReferences;
  end;
end;

function TGocciaAsyncGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'AsyncGenerator';
end;

function TGocciaAsyncGeneratorObjectValue.AsyncGeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorObjectValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    grkNext, ArgumentOrUndefined(AArgs));
end;

function TGocciaAsyncGeneratorObjectValue.AsyncGeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorObjectValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    grkReturn, ArgumentOrUndefined(AArgs));
end;

function TGocciaAsyncGeneratorObjectValue.AsyncGeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorObjectValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    grkThrow, ArgumentOrUndefined(AArgs));
end;

function TGocciaAsyncGeneratorObjectValue.AsyncIteratorSelf(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

{ Binds AFunction's parameters in a fresh call scope and hoists its body's
  declarations, then returns a continuation suspended before the first
  statement: the FunctionDeclarationInstantiation half of ES2026 §10.2.1.4
  OrdinaryCallEvaluateBody for a generator body (§15.5.2
  EvaluateGeneratorBody, §15.6.2 EvaluateAsyncGeneratorBody). }
function CreateGeneratorContinuation(const AFunction: TGocciaFunctionValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue;
  const AIsAsyncGenerator: Boolean): TGocciaGeneratorContinuation;
var
  Parameters: TGocciaParameterArray;
  Closure: TGocciaScope;
  I, J: Integer;
  CallScope: TGocciaScope;
  Value: TGocciaValue;
  Context: TGocciaEvaluationContext;
  ParamTypeHint: TGocciaLocalType;
  CompatibilityNonStrictMode: Boolean;
  ArgumentsObjectEnabled: Boolean;
  EvalRejectNames, SavedEvalRejectNames: TGocciaEvalRejectNameArray;
  ParameterNames: array of string;
  BodyScope: TGocciaScope;
  HasParamExpressions: Boolean;
  function EvaluateParameterDefault(
    const AExpression: TGocciaExpression): TGocciaValue;
  var
    SavedRejectArgumentsVarDeclaration: Boolean;
  begin
    SavedRejectArgumentsVarDeclaration :=
      Context.RejectArgumentsVarDeclarationInEval;
    SavedEvalRejectNames := Context.RejectVarDeclarationNamesInEval;
    Context.RejectArgumentsVarDeclarationInEval :=
      ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject;
    Context.RejectVarDeclarationNamesInEval := EvalRejectNames;
    try
      Result := EvaluateExpression(AExpression, Context);
    finally
      Context.RejectArgumentsVarDeclarationInEval :=
        SavedRejectArgumentsVarDeclaration;
      Context.RejectVarDeclarationNamesInEval := SavedEvalRejectNames;
    end;
  end;
  function CreateArgumentsObjectForCall: TGocciaObjectValue;
  var
    ParameterIndex: Integer;
  begin
    if Context.NonStrictMode and AFunction.IsSimpleParams then
    begin
      SetLength(ParameterNames, Length(Parameters));
      for ParameterIndex := 0 to High(Parameters) do
        ParameterNames[ParameterIndex] := Parameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        CallScope, AFunction));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
  Parameters := AFunction.Parameters;
  Closure := AFunction.Closure;
  CallScope := AFunction.CreateCallScope;
  AFunction.BindThis(CallScope, AThisValue);

  FillChar(Context, SizeOf(Context), 0);
  Context.Realm := CurrentRealm;
  Context.Scope := CallScope;
  Context.OnError := Closure.OnError;
  Context.LoadModule := Closure.LoadModule;
  Context.LoadModuleSource := Closure.LoadModuleSource;
  Context.ResolveModuleURL := Closure.ResolveModuleURL;
  Context.CurrentFilePath := AFunction.SourceFilePath;
  Context.CoverageEnabled := AFunction.TrackCoverage and
    (TGocciaCoverageTracker.Instance <> nil) and
    TGocciaCoverageTracker.Instance.Enabled;
  // EffectiveStrictTypes walks to the root scope so generator bodies
  // observe TGocciaEngine.SetStrictTypes updates made after the
  // generator's closure scope was created.
  Context.StrictTypes := Closure.EffectiveStrictTypes;
  CompatibilityNonStrictMode := Closure.EffectiveNonStrictMode;
  ArgumentsObjectEnabled := Closure.EffectiveArgumentsObjectEnabled;
  Context.NonStrictMode := CompatibilityNonStrictMode and
    not AFunction.StrictCode;
  Context.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
  Context.HideFunctionSourceText := AFunction.HideNestedFunctionSourceText;
  Context.DisposalTracker := nil;
  AFunction.RecordCoverageCall;
  HasParamExpressions := FunctionHasParameterExpressions(AFunction);
  // EvalRejectNames is only read while evaluating a parameter default, so
  // only build it when a parameter actually has a default or pattern
  // expression (parity with the ordinary call path in Goccia.Evaluator.Calls).
  if HasParamExpressions then
    EvalRejectNames := BuildFunctionParameterEvalRejectNames(AFunction,
      ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject and
      not ParameterListBindsName(Parameters, IDENTIFIER_ARGUMENTS))
  else
    EvalRejectNames := nil;

  if ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject and
     not ParameterListBindsName(Parameters, IDENTIFIER_ARGUMENTS) and
     not CallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
    CallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
      CreateArgumentsObjectForCall, True);

  if HasParamExpressions then
    PredeclareFunctionParameterBindings(AFunction, CallScope);

  for I := 0 to Length(Parameters) - 1 do
  begin
    if Parameters[I].IsRest then
    begin
      Value := TGocciaArrayValue.Create;
      if I < AArguments.Length then
        for J := I to AArguments.Length - 1 do
          TGocciaArrayValue(Value).Elements.Add(AArguments.GetElement(J));
      // ES2026 §10.2.11 step 28: IteratorBindingInitialization applies the
      // rest binding pattern to the packed argument list.
      if Parameters[I].IsPattern then
      begin
        Context.Scope := CallScope;
        AssignPattern(Parameters[I].Pattern, Value, Context, True, dtParameter);
      end
      else
        CallScope.DefineLexicalBinding(Parameters[I].Name, Value, dtParameter);

      // Mirrors the ordinary call path's IsRest branch: record the type hint
      // on the binding so subsequent reassignments are guarded under
      // --strict-types; skip initial enforcement on the rest array itself.
      if Context.StrictTypes and (Parameters[I].TypeAnnotation <> '') and
         not Parameters[I].IsPattern then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          CallScope.SetOwnBindingTypeHint(Parameters[I].Name, ParamTypeHint);
      end;

      Break;
    end
    else if Parameters[I].IsPattern then
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(Parameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(Parameters[I].DefaultValue);

      // Mirrors the ordinary call path's IsPattern branch: enforce on the raw
      // pre-destructured value (defaults and optionals skip enforcement,
      // parity with the named-param loop below).
      if Context.StrictTypes
        and (Parameters[I].TypeAnnotation <> '')
        and not Parameters[I].IsOptional
        and not Assigned(Parameters[I].DefaultValue) then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          EnforceStrictType(Value, ParamTypeHint);
      end;

      AssignPattern(Parameters[I].Pattern, Value, Context, True, dtParameter);
    end
    else
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(Parameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(Parameters[I].DefaultValue);
      CallScope.DefineLexicalBinding(Parameters[I].Name, Value, dtParameter);
    end;
  end;

  // Mirrors the ordinary call path's named-param post-binding loop: for
  // non-rest, non-pattern, non-default, non-optional named params, enforce on
  // the bound value and record the type hint so subsequent reassignments are
  // guarded. Skips the categories already handled inline above.
  if Context.StrictTypes then
  begin
    for I := 0 to Length(Parameters) - 1 do
    begin
      if Parameters[I].TypeAnnotation = '' then
        Continue;
      if Parameters[I].IsRest then
        Continue;
      if Parameters[I].IsOptional then
        Continue;
      if Assigned(Parameters[I].DefaultValue) then
        Continue;
      if Parameters[I].IsPattern then
        Continue;
      ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
      if ParamTypeHint = sltUntyped then
        Continue;
      EnforceStrictType(CallScope.GetValue(Parameters[I].Name),
        ParamTypeHint);
      CallScope.SetOwnBindingTypeHint(Parameters[I].Name, ParamTypeHint);
    end;
  end;

  BodyScope := CallScope;
  if HasParamExpressions then
    BodyScope := CallScope.CreateChild(skFunction, AFunction.Name + ':body');

  Context.Scope := BodyScope;

  HoistVarDeclarations(AFunction.BodyStatements, BodyScope, Context);
  PredeclareFunctionBodyLexicalDeclarations(AFunction.BodyStatements, BodyScope);
  HoistFunctionDeclarations(AFunction.BodyStatements, Context);
  Result := TGocciaGeneratorContinuation.Create(AFunction.BodyStatements,
    BodyScope, Context, AIsAsyncGenerator);
end;

{ The object a generator function or method returns when called. The caller,
  TGocciaGeneratorFunctionValue.Call and its relatives, sets its prototype. }
function CreateGeneratorObject(const AFunction: TGocciaFunctionValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue;
  const AIsAsync: Boolean): TGocciaObjectValue;
begin
  if AIsAsync then
    Result := TGocciaAsyncGeneratorObjectValue.Create(
      CreateGeneratorContinuation(AFunction, AArguments, AThisValue, True))
  else
    Result := TGocciaGeneratorObjectValue.Create(
      CreateGeneratorContinuation(AFunction, AArguments, AThisValue, False));
end;

initialization
  RegisterGeneratorObjectFactory(CreateGeneratorObject);

end.
