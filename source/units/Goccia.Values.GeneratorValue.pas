unit Goccia.Values.GeneratorValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Generator.Continuation,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.IteratorValue,
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

  TGocciaGeneratorBaseValue = class(TGocciaIteratorValue)
  public
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
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
    function BuiltinTagFallback: Boolean; override;
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorBaseValue = class(TGocciaObjectValue)
  public
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
  end;

  TGocciaAsyncGeneratorObjectValue = class(TGocciaAsyncGeneratorBaseValue)
  private
    FContinuation: TGocciaGeneratorContinuation;
    FState: TGocciaGeneratorState;
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
    function BuiltinTagFallback: Boolean; override;
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaGeneratorFunctionValue = class(TGocciaFunctionValue)
  protected
    function CreateContinuation(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue;
      const AIsAsyncGenerator: Boolean = False): TGocciaGeneratorContinuation;
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function IsConstructable: Boolean; override;
  end;

  TGocciaAsyncGeneratorFunctionValue = class(TGocciaGeneratorFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaGeneratorMethodValue = class(TGocciaMethodValue)
  protected
    function CreateContinuation(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue;
      const AIsAsyncGenerator: Boolean = False): TGocciaGeneratorContinuation;
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorMethodValue = class(TGocciaGeneratorMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

procedure EnsureGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
procedure EnsureAsyncGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);

implementation

uses
  SysUtils,

  Goccia.Arithmetic,
  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.Constants,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.GarbageCollector,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.Realm,
  Goccia.Types.Enforcement,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

type
  TGocciaGeneratorPrototypeMethodHost = class
  public
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAsyncGeneratorPrototypeMethodHost = class
  public
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
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

var
  GGeneratorPrototypeMethodHostSlot: TGocciaRealmOwnedSlotId;
  GAsyncGeneratorPrototypeMethodHostSlot: TGocciaRealmOwnedSlotId;

function GeneratorObjectIntrinsicParent(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const ARealm: TGocciaRealm = nil): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  ShouldSwitchRealm: Boolean;
begin
  PreviousRealm := CurrentRealm;
  ShouldSwitchRealm := Assigned(ARealm) and (ARealm <> PreviousRealm);
  if ShouldSwitchRealm then
    SetCurrentRealm(ARealm);
  try
    if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
      TGocciaObjectValue.InitializeSharedPrototype;
    IteratorPrototype := nil;
    if AKind = foikGenerator then
      IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
    Result := GeneratorObjectIntrinsicPrototype(AKind,
      TGocciaFunctionBase.GetSharedPrototype,
      TGocciaObjectValue.SharedObjectPrototype,
      IteratorPrototype);
    if AKind = foikGenerator then
      EnsureGeneratorPrototypeMethods(Result)
    else if AKind = foikAsyncGenerator then
      EnsureAsyncGeneratorPrototypeMethods(Result);
  finally
    if ShouldSwitchRealm then
      SetCurrentRealm(PreviousRealm);
  end;
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

function PromiseConstructorIntrinsic: TGocciaValue;
var
  GlobalScope: TGocciaScope;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(CurrentRealm) or
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
  IsRooted := Assigned(TGarbageCollector.Instance);
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

function GeneratorPrototypeMethodHost: TGocciaGeneratorPrototypeMethodHost;
begin
  if not Assigned(CurrentRealm) then
    raise Exception.Create('Generator prototype methods require an active realm');
  Result := TGocciaGeneratorPrototypeMethodHost(
    CurrentRealm.GetOwnedSlot(GGeneratorPrototypeMethodHostSlot));
  if Assigned(Result) then
    Exit;
  Result := TGocciaGeneratorPrototypeMethodHost.Create;
  CurrentRealm.SetOwnedSlot(GGeneratorPrototypeMethodHostSlot, Result);
end;

function AsyncGeneratorPrototypeMethodHost: TGocciaAsyncGeneratorPrototypeMethodHost;
begin
  if not Assigned(CurrentRealm) then
    raise Exception.Create('AsyncGenerator prototype methods require an active realm');
  Result := TGocciaAsyncGeneratorPrototypeMethodHost(
    CurrentRealm.GetOwnedSlot(GAsyncGeneratorPrototypeMethodHostSlot));
  if Assigned(Result) then
    Exit;
  Result := TGocciaAsyncGeneratorPrototypeMethodHost.Create;
  CurrentRealm.SetOwnedSlot(GAsyncGeneratorPrototypeMethodHostSlot, Result);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorNext(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorNext(AArgs,
    AThisValue);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorReturn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorReturn(AArgs,
    AThisValue);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorThrow(AArgs,
    AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorNext(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorNext(
    AArgs, AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorReturn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorReturn(
    AArgs, AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorThrow(
    AArgs, AThisValue);
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

procedure EnsureGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
var
  Host: TGocciaGeneratorPrototypeMethodHost;
begin
  if not Assigned(APrototype) then
    Exit;

  Host := nil;
  if not APrototype.HasOwnProperty(PROP_NEXT) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_NEXT,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorNext, PROP_NEXT, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_RETURN) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_RETURN,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorReturn, PROP_RETURN, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_THROW) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_THROW,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorThrow, PROP_THROW, 1),
        [pfConfigurable, pfWritable]));
  end;
end;

procedure EnsureAsyncGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
var
  Host: TGocciaAsyncGeneratorPrototypeMethodHost;
begin
  if not Assigned(APrototype) then
    Exit;

  Host := nil;
  if not APrototype.HasOwnProperty(PROP_NEXT) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_NEXT,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorNext, PROP_NEXT, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_RETURN) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_RETURN,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorReturn, PROP_RETURN, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_THROW) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_THROW,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorThrow, PROP_THROW, 1),
        [pfConfigurable, pfWritable]));
  end;
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

function TGocciaGeneratorObjectValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
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
      on E: Exception do
      begin
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
        on E: Exception do
        begin
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
    on E: Exception do
    begin
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

function TGocciaAsyncGeneratorObjectValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
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
  const AThisValue: TGocciaValue;
  const AIsAsyncGenerator: Boolean = False): TGocciaGeneratorContinuation;
var
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
      ArgumentsObjectEnabled and CreatesArgumentsObject;
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
    if Context.NonStrictMode and FIsSimpleParams then
    begin
      SetLength(ParameterNames, Length(FParameters));
      for ParameterIndex := 0 to High(FParameters) do
        ParameterNames[ParameterIndex] := FParameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        CallScope, Self));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
  CallScope := CreateCallScope;
  BindThis(CallScope, AThisValue);

  FillChar(Context, SizeOf(Context), 0);
  Context.Realm := CurrentRealm;
  Context.Scope := CallScope;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.LoadModuleSource := FClosure.LoadModuleSource;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := False;
  // EffectiveStrictTypes walks to the root scope so generator bodies
  // observe TGocciaEngine.SetStrictTypes updates made after the
  // generator's closure scope was created.
  Context.StrictTypes := FClosure.EffectiveStrictTypes;
  CompatibilityNonStrictMode := FClosure.EffectiveNonStrictMode;
  ArgumentsObjectEnabled := FClosure.EffectiveArgumentsObjectEnabled;
  Context.NonStrictMode := CompatibilityNonStrictMode and not FStrictCode;
  Context.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
  Context.DisposalTracker := nil;
  HasParamExpressions := HasParameterExpressions;
  // EvalRejectNames is only read while evaluating a parameter default, so
  // only build it when a parameter actually has a default or pattern
  // expression (parity with TGocciaFunctionValue.ExecuteBody).
  if HasParamExpressions then
    EvalRejectNames := BuildParameterEvalVarDeclarationRejectNames(
      ArgumentsObjectEnabled and CreatesArgumentsObject and
      not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS))
  else
    EvalRejectNames := nil;

  if ArgumentsObjectEnabled and CreatesArgumentsObject and
     not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS) and
     not CallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
    CallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
      CreateArgumentsObjectForCall, True);

  if HasParamExpressions then
    PredeclareParameterBindings(CallScope);

  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then
    begin
      Value := TGocciaArrayValue.Create;
      if I < AArguments.Length then
        for J := I to AArguments.Length - 1 do
          TGocciaArrayValue(Value).Elements.Add(AArguments.GetElement(J));
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);

      // Mirrors TGocciaFunctionValue.ExecuteBody's IsRest branch: record
      // the type hint on the binding so subsequent reassignments are
      // guarded under --strict-types; skip initial enforcement on the
      // rest array itself.
      if Context.StrictTypes and (FParameters[I].TypeAnnotation <> '') then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          CallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
      end;

      Break;
    end
    else if FParameters[I].IsPattern then
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FParameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(FParameters[I].DefaultValue);

      // Mirrors TGocciaFunctionValue.ExecuteBody's IsPattern branch:
      // enforce on the raw pre-destructured value (defaults and
      // optionals skip enforcement, parity with the named-param loop
      // below).
      if Context.StrictTypes
        and (FParameters[I].TypeAnnotation <> '')
        and not FParameters[I].IsOptional
        and not Assigned(FParameters[I].DefaultValue) then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          EnforceStrictType(Value, ParamTypeHint);
      end;

      AssignPattern(FParameters[I].Pattern, Value, Context, True, dtParameter);
    end
    else
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FParameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(FParameters[I].DefaultValue);
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
    end;
  end;

  // Mirrors TGocciaFunctionValue.ExecuteBody's named-param post-binding
  // loop: for non-rest, non-pattern, non-default, non-optional named
  // params, enforce on the bound value and record the type hint so
  // subsequent reassignments are guarded.  Skips the categories already
  // handled inline above.
  if Context.StrictTypes then
  begin
    for I := 0 to Length(FParameters) - 1 do
    begin
      if FParameters[I].TypeAnnotation = '' then
        Continue;
      if FParameters[I].IsRest then
        Continue;
      if FParameters[I].IsOptional then
        Continue;
      if Assigned(FParameters[I].DefaultValue) then
        Continue;
      if FParameters[I].IsPattern then
        Continue;
      ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
      if ParamTypeHint = sltUntyped then
        Continue;
      EnforceStrictType(CallScope.GetValue(FParameters[I].Name),
        ParamTypeHint);
      CallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
    end;
  end;

  BodyScope := CallScope;
  if HasParamExpressions then
    BodyScope := CallScope.CreateChild(skFunction, FName + ':body');

  Context.Scope := BodyScope;

  HoistVarDeclarations(FBodyStatements, BodyScope, Context);
  PredeclareFunctionBodyLexicalDeclarations(FBodyStatements, BodyScope);
  HoistFunctionDeclarations(FBodyStatements, Context);
  Result := TGocciaGeneratorContinuation.Create(FBodyStatements, BodyScope,
    Context, AIsAsyncGenerator);
end;

function TGocciaGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  GeneratorObject: TGocciaGeneratorObjectValue;
  PrototypeValue: TGocciaValue;
begin
  GeneratorObject := TGocciaGeneratorObjectValue.Create(
    CreateContinuation(AArguments, AThisValue));
  PrototypeValue := GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    GeneratorObject.Prototype := TGocciaObjectValue(PrototypeValue)
  else
    GeneratorObject.Prototype := GeneratorObjectIntrinsicParent(foikGenerator,
      CreationRealm);
  Result := GeneratorObject;
end;

function TGocciaGeneratorFunctionValue.IsConstructable: Boolean;
begin
  Result := False;
end;

function TGocciaAsyncGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  AsyncGeneratorObject: TGocciaAsyncGeneratorObjectValue;
  PrototypeValue: TGocciaValue;
begin
  AsyncGeneratorObject := TGocciaAsyncGeneratorObjectValue.Create(
    CreateContinuation(AArguments, AThisValue, True));
  PrototypeValue := GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    AsyncGeneratorObject.Prototype := TGocciaObjectValue(PrototypeValue)
  else
    AsyncGeneratorObject.Prototype := GeneratorObjectIntrinsicParent(
      foikAsyncGenerator, CreationRealm);
  Result := AsyncGeneratorObject;
end;

{ TGocciaGeneratorMethodValue }

function TGocciaGeneratorMethodValue.CreateContinuation(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue;
  const AIsAsyncGenerator: Boolean = False): TGocciaGeneratorContinuation;
var
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
      ArgumentsObjectEnabled and CreatesArgumentsObject;
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
    if Context.NonStrictMode and FIsSimpleParams then
    begin
      SetLength(ParameterNames, Length(FParameters));
      for ParameterIndex := 0 to High(FParameters) do
        ParameterNames[ParameterIndex] := FParameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        CallScope, Self));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
  CallScope := CreateCallScope;
  BindThis(CallScope, AThisValue);

  FillChar(Context, SizeOf(Context), 0);
  Context.Realm := CurrentRealm;
  Context.Scope := CallScope;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.LoadModuleSource := FClosure.LoadModuleSource;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := False;
  // EffectiveStrictTypes — see CreateContinuation above.
  Context.StrictTypes := FClosure.EffectiveStrictTypes;
  CompatibilityNonStrictMode := FClosure.EffectiveNonStrictMode;
  ArgumentsObjectEnabled := FClosure.EffectiveArgumentsObjectEnabled;
  Context.NonStrictMode := CompatibilityNonStrictMode and not FStrictCode;
  Context.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
  Context.DisposalTracker := nil;
  HasParamExpressions := HasParameterExpressions;
  // EvalRejectNames is only read while evaluating a parameter default, so
  // only build it when a parameter actually has a default or pattern
  // expression (parity with TGocciaFunctionValue.ExecuteBody).
  if HasParamExpressions then
    EvalRejectNames := BuildParameterEvalVarDeclarationRejectNames(
      ArgumentsObjectEnabled and CreatesArgumentsObject and
      not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS))
  else
    EvalRejectNames := nil;

  if ArgumentsObjectEnabled and CreatesArgumentsObject and
     not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS) and
     not CallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
    CallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
      CreateArgumentsObjectForCall, True);

  if HasParamExpressions then
    PredeclareParameterBindings(CallScope);

  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then
    begin
      Value := TGocciaArrayValue.Create;
      if I < AArguments.Length then
        for J := I to AArguments.Length - 1 do
          TGocciaArrayValue(Value).Elements.Add(AArguments.GetElement(J));
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);

      // Mirrors TGocciaFunctionValue.ExecuteBody's IsRest branch: see
      // TGocciaGeneratorFunctionValue.CreateContinuation above.
      if Context.StrictTypes and (FParameters[I].TypeAnnotation <> '') then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          CallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
      end;

      Break;
    end
    else if FParameters[I].IsPattern then
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FParameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(FParameters[I].DefaultValue);

      // Mirrors TGocciaFunctionValue.ExecuteBody's IsPattern branch.
      if Context.StrictTypes
        and (FParameters[I].TypeAnnotation <> '')
        and not FParameters[I].IsOptional
        and not Assigned(FParameters[I].DefaultValue) then
      begin
        ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
        if ParamTypeHint <> sltUntyped then
          EnforceStrictType(Value, ParamTypeHint);
      end;

      AssignPattern(FParameters[I].Pattern, Value, Context, True, dtParameter);
    end
    else
    begin
      if I < AArguments.Length then
        Value := AArguments.GetElement(I)
      else
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FParameters[I].DefaultValue) and
         (Value is TGocciaUndefinedLiteralValue) then
        Value := EvaluateParameterDefault(FParameters[I].DefaultValue);
      CallScope.DefineLexicalBinding(FParameters[I].Name, Value, dtParameter);
    end;
  end;

  // Mirrors TGocciaFunctionValue.ExecuteBody's named-param post-binding
  // loop -- see TGocciaGeneratorFunctionValue.CreateContinuation above.
  if Context.StrictTypes then
  begin
    for I := 0 to Length(FParameters) - 1 do
    begin
      if FParameters[I].TypeAnnotation = '' then
        Continue;
      if FParameters[I].IsRest then
        Continue;
      if FParameters[I].IsOptional then
        Continue;
      if Assigned(FParameters[I].DefaultValue) then
        Continue;
      if FParameters[I].IsPattern then
        Continue;
      ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
      if ParamTypeHint = sltUntyped then
        Continue;
      EnforceStrictType(CallScope.GetValue(FParameters[I].Name),
        ParamTypeHint);
      CallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
    end;
  end;

  BodyScope := CallScope;
  if HasParamExpressions then
    BodyScope := CallScope.CreateChild(skFunction, FName + ':body');

  Context.Scope := BodyScope;

  HoistVarDeclarations(FBodyStatements, BodyScope, Context);
  PredeclareFunctionBodyLexicalDeclarations(FBodyStatements, BodyScope);
  HoistFunctionDeclarations(FBodyStatements, Context);
  Result := TGocciaGeneratorContinuation.Create(FBodyStatements, BodyScope,
    Context, AIsAsyncGenerator);
end;

function TGocciaGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaGeneratorObjectValue.Create(CreateContinuation(AArguments, AThisValue));
end;

function TGocciaAsyncGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaAsyncGeneratorObjectValue.Create(
    CreateContinuation(AArguments, AThisValue, True));
end;

initialization
  GGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'Generator prototype method host');
  GAsyncGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'AsyncGenerator prototype method host');

end.
