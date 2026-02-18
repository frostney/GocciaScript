unit Goccia.Values.PromiseValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Goccia.SharedPrototype, Generics.Collections, SysUtils;

type
  TGocciaPromiseState = (gpsPending, gpsFulfilled, gpsRejected);

  TGocciaPromiseValue = class;

  TGocciaPromiseReaction = record
    OnFulfilled: TGocciaValue;
    OnRejected: TGocciaValue;
    ResultPromise: TGocciaPromiseValue;
  end;

  TGocciaPromiseFinallyWrapper = class(TGocciaObjectValue)
  private
    FOnFinally: TGocciaValue;
    FIsFulfill: Boolean;
  public
    constructor Create(AOnFinally: TGocciaValue; AIsFulfill: Boolean);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TGocciaPromiseValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FState: TGocciaPromiseState;
    FResult: TGocciaValue;
    FReactions: TList<TGocciaPromiseReaction>;
    FAlreadyResolved: Boolean;

    function PromiseThen(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseCatch(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseFinally(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure TriggerReactions;
    procedure InitializePrototype;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure Resolve(AValue: TGocciaValue);
    procedure Reject(AReason: TGocciaValue);
    procedure SubscribeTo(APromise: TGocciaPromiseValue);

    function DoResolve(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DoReject(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure GCMarkReferences; override;

    class procedure ExposePrototype(AConstructor: TGocciaObjectValue);

    property State: TGocciaPromiseState read FState;
    property PromiseResult: TGocciaValue read FResult;
  end;

implementation

uses
  Goccia.MicrotaskQueue, Goccia.Values.FunctionBase,
  Goccia.Values.Error, Goccia.Values.ErrorHelper,
  Goccia.GarbageCollector;

type
  TGocciaFinallyPassthrough = class(TGocciaObjectValue)
  private
    FOriginalValue: TGocciaValue;
    FIsFulfill: Boolean;
  public
    constructor Create(AOriginalValue: TGocciaValue; AIsFulfill: Boolean);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

{ TGocciaFinallyPassthrough }

constructor TGocciaFinallyPassthrough.Create(AOriginalValue: TGocciaValue; AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOriginalValue := AOriginalValue;
  FIsFulfill := AIsFulfill;
end;

function TGocciaFinallyPassthrough.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  if FIsFulfill then
    Result := FOriginalValue
  else
    raise TGocciaThrowValue.Create(FOriginalValue);
end;

procedure TGocciaFinallyPassthrough.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOriginalValue) then
    FOriginalValue.GCMarkReferences;
end;

{ TGocciaPromiseFinallyWrapper }

constructor TGocciaPromiseFinallyWrapper.Create(AOnFinally: TGocciaValue; AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOnFinally := AOnFinally;
  FIsFulfill := AIsFulfill;
end;

function TGocciaPromiseFinallyWrapper.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  OriginalValue, CallbackResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallbackPromise, IntermediatePromise: TGocciaPromiseValue;
  Passthrough: TGocciaFinallyPassthrough;
  PassthroughFn: TGocciaNativeFunctionValue;
  Reaction: TGocciaPromiseReaction;
begin
  OriginalValue := Args.GetElement(0);

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    CallbackResult := TGocciaFunctionBase(FOnFinally).Call(
      CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;

  if CallbackResult is TGocciaPromiseValue then
  begin
    CallbackPromise := TGocciaPromiseValue(CallbackResult);

    case CallbackPromise.State of
      gpsFulfilled:
      begin
        if FIsFulfill then
          Result := OriginalValue
        else
          raise TGocciaThrowValue.Create(OriginalValue);
      end;
      gpsRejected:
        raise TGocciaThrowValue.Create(CallbackPromise.PromiseResult);
      gpsPending:
      begin
        IntermediatePromise := TGocciaPromiseValue.Create;

        Passthrough := TGocciaFinallyPassthrough.Create(OriginalValue, FIsFulfill);
        PassthroughFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Passthrough.Invoke, 'finally-passthrough', 1);

        Reaction.OnFulfilled := PassthroughFn;
        Reaction.OnRejected := nil;
        Reaction.ResultPromise := IntermediatePromise;
        CallbackPromise.FReactions.Add(Reaction);

        Result := IntermediatePromise;
      end;
    end;
  end
  else
  begin
    if FIsFulfill then
      Result := OriginalValue
    else
      raise TGocciaThrowValue.Create(OriginalValue);
  end;
end;

procedure TGocciaPromiseFinallyWrapper.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOnFinally) then
    FOnFinally.GCMarkReferences;
end;

{ TGocciaPromiseValue }

constructor TGocciaPromiseValue.Create;
begin
  inherited Create(nil);
  FState := gpsPending;
  FResult := TGocciaUndefinedLiteralValue.UndefinedValue;
  FReactions := TList<TGocciaPromiseReaction>.Create;
  FAlreadyResolved := False;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaPromiseValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseThen, 'then', 2));
  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseCatch, 'catch', 1));
  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseFinally, 'finally', 1));
end;

class procedure TGocciaPromiseValue.ExposePrototype(AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaPromiseValue.Create;
  FShared.ExposeOnConstructor(AConstructor);
end;

destructor TGocciaPromiseValue.Destroy;
begin
  FReactions.Free;
  inherited;
end;

procedure TGocciaPromiseValue.GCMarkReferences;
var
  I: Integer;
  Reaction: TGocciaPromiseReaction;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FResult) then
    FResult.GCMarkReferences;

  for I := 0 to FReactions.Count - 1 do
  begin
    Reaction := FReactions[I];
    if Assigned(Reaction.OnFulfilled) then
      Reaction.OnFulfilled.GCMarkReferences;
    if Assigned(Reaction.OnRejected) then
      Reaction.OnRejected.GCMarkReferences;
    if Assigned(Reaction.ResultPromise) then
      Reaction.ResultPromise.GCMarkReferences;
  end;
end;

procedure TGocciaPromiseValue.Resolve(AValue: TGocciaValue);
var
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if FState <> gpsPending then Exit;

  if AValue = Self then
  begin
    FState := gpsRejected;
    FResult := Goccia.Values.ErrorHelper.CreateErrorObject('TypeError',
      'Chaining cycle detected for promise');
    TriggerReactions;
    Exit;
  end;

  if AValue is TGocciaPromiseValue then
  begin
    Queue := TGocciaMicrotaskQueue.Instance;
    if Assigned(Queue) then
    begin
      Task.Handler := nil;
      Task.Value := AValue;
      Task.ResultPromise := Self;
      Task.ReactionType := prtThenableResolve;
      Queue.Enqueue(Task);
    end;
    Exit;
  end;

  FState := gpsFulfilled;
  FResult := AValue;
  TriggerReactions;
end;

procedure TGocciaPromiseValue.Reject(AReason: TGocciaValue);
begin
  if FState <> gpsPending then Exit;

  FState := gpsRejected;
  FResult := AReason;
  TriggerReactions;
end;

procedure TGocciaPromiseValue.SubscribeTo(APromise: TGocciaPromiseValue);
var
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  case APromise.FState of
    gpsFulfilled, gpsRejected:
    begin
      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) then
      begin
        Task.Handler := nil;
        Task.Value := APromise.FResult;
        Task.ResultPromise := Self;
        if APromise.FState = gpsFulfilled then
          Task.ReactionType := prtFulfill
        else
          Task.ReactionType := prtReject;
        Queue.Enqueue(Task);
      end;
    end;
    gpsPending:
    begin
      Reaction.OnFulfilled := nil;
      Reaction.OnRejected := nil;
      Reaction.ResultPromise := Self;
      APromise.FReactions.Add(Reaction);
    end;
  end;
end;

procedure TGocciaPromiseValue.TriggerReactions;
var
  I: Integer;
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then Exit;

  for I := 0 to FReactions.Count - 1 do
  begin
    Reaction := FReactions[I];

    case FState of
      gpsFulfilled:
      begin
        Task.Handler := Reaction.OnFulfilled;
        Task.Value := FResult;
        Task.ResultPromise := Reaction.ResultPromise;
        Task.ReactionType := prtFulfill;
        Queue.Enqueue(Task);
      end;
      gpsRejected:
      begin
        Task.Handler := Reaction.OnRejected;
        Task.Value := FResult;
        Task.ResultPromise := Reaction.ResultPromise;
        Task.ReactionType := prtReject;
        Queue.Enqueue(Task);
      end;
    end;
  end;

  FReactions.Clear;
end;

function TGocciaPromiseValue.DoResolve(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if Args.Length > 0 then
    Resolve(Args.GetElement(0))
  else
    Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaPromiseValue.DoReject(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if Args.Length > 0 then
    Reject(Args.GetElement(0))
  else
    Reject(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

{ Prototype methods }

function TGocciaPromiseValue.PromiseThen(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
  OnFulfilled, OnRejected: TGocciaValue;
  ChildPromise: TGocciaPromiseValue;
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if not (ThisValue is TGocciaPromiseValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.prototype.then called on non-Promise');

  P := TGocciaPromiseValue(ThisValue);

  OnFulfilled := nil;
  OnRejected := nil;
  if (Args.Length > 0) and Args.GetElement(0).IsCallable then
    OnFulfilled := Args.GetElement(0);
  if (Args.Length > 1) and Args.GetElement(1).IsCallable then
    OnRejected := Args.GetElement(1);

  ChildPromise := TGocciaPromiseValue.Create;

  Queue := TGocciaMicrotaskQueue.Instance;

  case P.FState of
    gpsPending:
    begin
      Reaction.OnFulfilled := OnFulfilled;
      Reaction.OnRejected := OnRejected;
      Reaction.ResultPromise := ChildPromise;
      P.FReactions.Add(Reaction);
    end;
    gpsFulfilled:
    begin
      Task.Handler := OnFulfilled;
      Task.Value := P.FResult;
      Task.ResultPromise := ChildPromise;
      Task.ReactionType := prtFulfill;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
    gpsRejected:
    begin
      Task.Handler := OnRejected;
      Task.Value := P.FResult;
      Task.ResultPromise := ChildPromise;
      Task.ReactionType := prtReject;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
  end;

  Result := ChildPromise;
end;

function TGocciaPromiseValue.PromiseCatch(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  ThenArgs: TGocciaArgumentsCollection;
begin
  ThenArgs := TGocciaArgumentsCollection.Create([
    TGocciaUndefinedLiteralValue.UndefinedValue,
    Args.GetElement(0)
  ]);
  try
    Result := PromiseThen(ThenArgs, ThisValue);
  finally
    ThenArgs.Free;
  end;
end;

function TGocciaPromiseValue.PromiseFinally(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  OnFinally: TGocciaValue;
  ThenArgs: TGocciaArgumentsCollection;
  WrapFulfilled, WrapRejected: TGocciaPromiseFinallyWrapper;
  WrapFulfilledFn, WrapRejectedFn: TGocciaNativeFunctionValue;
begin
  if not (ThisValue is TGocciaPromiseValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.prototype.finally called on non-Promise');

  OnFinally := nil;
  if (Args.Length > 0) and Args.GetElement(0).IsCallable then
    OnFinally := Args.GetElement(0);

  if not Assigned(OnFinally) then
  begin
    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaUndefinedLiteralValue.UndefinedValue,
      TGocciaUndefinedLiteralValue.UndefinedValue
    ]);
    try
      Result := PromiseThen(ThenArgs, ThisValue);
    finally
      ThenArgs.Free;
    end;
    Exit;
  end;

  WrapFulfilled := TGocciaPromiseFinallyWrapper.Create(OnFinally, True);
  WrapRejected := TGocciaPromiseFinallyWrapper.Create(OnFinally, False);

  WrapFulfilledFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(WrapFulfilled.Invoke, 'finally-fulfill', 1);
  WrapRejectedFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(WrapRejected.Invoke, 'finally-reject', 1);

  ThenArgs := TGocciaArgumentsCollection.Create([WrapFulfilledFn, WrapRejectedFn]);
  try
    Result := PromiseThen(ThenArgs, ThisValue);
  finally
    ThenArgs.Free;
  end;
end;

function TGocciaPromiseValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := inherited GetProperty(AName);
end;

function TGocciaPromiseValue.ToStringTag: string;
begin
  Result := 'Promise';
end;

end.
