unit Goccia.Values.GeneratorValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Runtime.GeneratorContinuation,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGeneratorState = (gsSuspendedStart, gsSuspendedYield, gsExecuting, gsCompleted);

  TGocciaGeneratorObjectValue = class(TGocciaIteratorValue)
  private
    FContinuation: TGocciaGeneratorContinuation;
    FState: TGocciaGeneratorState;
  public
    constructor Create(const AContinuation: TGocciaGeneratorContinuation);
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    procedure Close; override;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAsyncGeneratorObjectValue = class(TGocciaObjectValue)
  private
    FContinuation: TGocciaGeneratorContinuation;
    FState: TGocciaGeneratorState;
    function ResumeAsPromise(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AContinuation: TGocciaGeneratorContinuation);
    destructor Destroy; override;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaGeneratorFunctionValue = class(TGocciaFunctionValue)
  protected
    function CreateContinuation(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaGeneratorContinuation;
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorFunctionValue = class(TGocciaGeneratorFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaGeneratorMethodValue = class(TGocciaMethodValue)
  protected
    function CreateContinuation(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaGeneratorContinuation;
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorMethodValue = class(TGocciaGeneratorMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

implementation

uses
  SysUtils,

  Goccia.AST.Statements,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue;

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

function GeneratorIncompatibleReceiverError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME,
    'Generator method called on incompatible receiver');
end;

function RejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;
begin
  Result := TGocciaPromiseValue.Create;
  try
    Result.Reject(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
  except
    Result.Free;
    raise;
  end;
end;

{ TGocciaGeneratorObjectValue }

constructor TGocciaGeneratorObjectValue.Create(const AContinuation: TGocciaGeneratorContinuation);
begin
  inherited Create;
  FContinuation := AContinuation;
  FState := gsSuspendedStart;
  DefineProperty(PROP_NEXT,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(GeneratorNext, PROP_NEXT, 1),
      [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(GeneratorReturn, PROP_RETURN, 1),
      [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(GeneratorThrow, PROP_THROW, 1),
      [pfConfigurable, pfWritable]));
end;

destructor TGocciaGeneratorObjectValue.Destroy;
begin
  FContinuation.Free;
  inherited;
end;

function TGocciaGeneratorObjectValue.AdvanceNext: TGocciaObjectValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if FState = gsCompleted then
    Exit(CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True));
  FState := gsExecuting;
  Value := FContinuation.Resume(grkNext, TGocciaUndefinedLiteralValue.UndefinedValue, Done);
  if Done then
    FState := gsCompleted
  else
    FState := gsSuspendedYield;
  Result := CreateIteratorResult(Value, Done);
end;

function TGocciaGeneratorObjectValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  IteratorResult: TGocciaObjectValue;
begin
  IteratorResult := AdvanceNext;
  ADone := IteratorResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value;
  Result := IteratorResult.GetProperty(PROP_VALUE);
end;

procedure TGocciaGeneratorObjectValue.Close;
var
  Done: Boolean;
begin
  if FState = gsCompleted then
    Exit;
  if FState = gsExecuting then
    raise TGocciaThrowValue.Create(GeneratorExecutingError);
  FState := gsExecuting;
  try
    FContinuation.Resume(grkReturn, TGocciaUndefinedLiteralValue.UndefinedValue, Done);
    if Done then
      FState := gsCompleted
    else
      FState := gsSuspendedYield;
  except
    FState := gsCompleted;
    raise;
  end;
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
  DefineProperty(PROP_NEXT,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncGeneratorNext, PROP_NEXT, 1),
      [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncGeneratorReturn, PROP_RETURN, 1),
      [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncGeneratorThrow, PROP_THROW, 1),
      [pfConfigurable, pfWritable]));
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncIteratorSelf, '[Symbol.asyncIterator]', 0),
      [pfConfigurable, pfWritable]));
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
  Done: Boolean;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  try
    try
      if FState = gsCompleted then
      begin
        case AKind of
          grkNext:
            begin
              Promise.Resolve(CreateIteratorResult(
                TGocciaUndefinedLiteralValue.UndefinedValue, True));
              Exit(Promise);
            end;
          grkThrow:
            begin
              Promise.Reject(AValue);
              Exit(Promise);
            end;
        else
          begin
            UnwrappedValue := AwaitValue(AValue);
            Promise.Resolve(CreateIteratorResult(UnwrappedValue, True));
            Exit(Promise);
          end;
        end;
      end
      else
      begin
        FState := gsExecuting;
        Value := FContinuation.Resume(AKind, AValue, Done);
        if Done then
          FState := gsCompleted
        else
          FState := gsSuspendedYield;
      end;
      UnwrappedValue := AwaitValue(Value);
      Promise.Resolve(CreateIteratorResult(UnwrappedValue, Done));
    except
      on E: TGocciaThrowValue do
      begin
        FState := gsCompleted;
        Promise.Reject(E.Value);
      end;
    end;
  except
    Promise.Free;
    raise;
  end;
  Result := Promise;
end;

procedure TGocciaAsyncGeneratorObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
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

{ TGocciaGeneratorFunctionValue }

function TGocciaGeneratorFunctionValue.CreateContinuation(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaGeneratorContinuation;
var
  I, J: Integer;
  CallScope: TGocciaScope;
  Value: TGocciaValue;
  Context: TGocciaEvaluationContext;
begin
  CallScope := CreateCallScope;
  BindThis(CallScope, AThisValue);

  Context.Scope := CallScope;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := False;
  Context.DisposalTracker := nil;

  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then
    begin
      Value := TGocciaArrayValue.Create;
      if I < AArguments.Length then
        for J := I to AArguments.Length - 1 do
          TGocciaArrayValue(Value).Elements.Add(AArguments.GetElement(J));
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
      Break;
    end
    else if FParameters[I].IsPattern then
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else if Assigned(FParameters[I].DefaultValue) then
        Value := EvaluateExpression(FParameters[I].DefaultValue, Context)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      AssignPattern(FParameters[I].Pattern, Value, Context, True, dtParameter);
    end
    else
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else if Assigned(FParameters[I].DefaultValue) then
        Value := EvaluateExpression(FParameters[I].DefaultValue, Context)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
    end;
  end;

  HoistVarDeclarations(FBodyStatements, CallScope);
  HoistFunctionDeclarations(FBodyStatements, Context);
  Result := TGocciaGeneratorContinuation.Create(FBodyStatements, CallScope, Context);
end;

function TGocciaGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaGeneratorObjectValue.Create(CreateContinuation(AArguments, AThisValue));
end;

function TGocciaAsyncGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaAsyncGeneratorObjectValue.Create(CreateContinuation(AArguments, AThisValue));
end;

{ TGocciaGeneratorMethodValue }

function TGocciaGeneratorMethodValue.CreateContinuation(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaGeneratorContinuation;
var
  I, J: Integer;
  CallScope: TGocciaScope;
  Value: TGocciaValue;
  Context: TGocciaEvaluationContext;
begin
  CallScope := CreateCallScope;
  BindThis(CallScope, AThisValue);

  Context.Scope := CallScope;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := False;
  Context.DisposalTracker := nil;

  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then
    begin
      Value := TGocciaArrayValue.Create;
      if I < AArguments.Length then
        for J := I to AArguments.Length - 1 do
          TGocciaArrayValue(Value).Elements.Add(AArguments.GetElement(J));
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
      Break;
    end
    else if FParameters[I].IsPattern then
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else if Assigned(FParameters[I].DefaultValue) then
        Value := EvaluateExpression(FParameters[I].DefaultValue, Context)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      AssignPattern(FParameters[I].Pattern, Value, Context, True, dtParameter);
    end
    else
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else if Assigned(FParameters[I].DefaultValue) then
        Value := EvaluateExpression(FParameters[I].DefaultValue, Context)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
    end;
  end;

  HoistVarDeclarations(FBodyStatements, CallScope);
  HoistFunctionDeclarations(FBodyStatements, Context);
  Result := TGocciaGeneratorContinuation.Create(FBodyStatements, CallScope, Context);
end;

function TGocciaGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaGeneratorObjectValue.Create(CreateContinuation(AArguments, AThisValue));
end;

function TGocciaAsyncGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaAsyncGeneratorObjectValue.Create(CreateContinuation(AArguments, AThisValue));
end;

end.
