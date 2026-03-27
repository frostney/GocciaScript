unit Goccia.Values.PromiseValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

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
    constructor Create(const AOnFinally: TGocciaValue; const AIsFulfill: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaPromiseValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
  private
    FState: TGocciaPromiseState;
    FResult: TGocciaValue;
    FReactions: TList<TGocciaPromiseReaction>;
    FAlreadyResolved: Boolean;

    procedure TriggerReactions;
    procedure InitializePrototype;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure Resolve(const AValue: TGocciaValue);
    procedure Reject(const AReason: TGocciaValue);
    procedure SubscribeTo(const APromise: TGocciaPromiseValue);

    function DoResolve(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DoReject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    function InvokeThen(const AOnFulfilled, AOnRejected: TGocciaValue): TGocciaPromiseValue;
    function InvokeFinally(const AOnFinally: TGocciaValue): TGocciaPromiseValue;

    property State: TGocciaPromiseState read FState;
    property PromiseResult: TGocciaValue read FResult;
  published
    function PromiseThen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseCatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseFinally(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  GarbageCollector.Generic,

  Goccia.Constants.ErrorNames,
  Goccia.MicrotaskQueue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction;

type
  TGocciaFinallyPassthrough = class(TGocciaObjectValue)
  private
    FOriginalValue: TGocciaValue;
    FIsFulfill: Boolean;
  public
    constructor Create(const AOriginalValue: TGocciaValue; const AIsFulfill: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ TGocciaFinallyPassthrough }

constructor TGocciaFinallyPassthrough.Create(const AOriginalValue: TGocciaValue; const AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOriginalValue := AOriginalValue;
  FIsFulfill := AIsFulfill;
end;

function TGocciaFinallyPassthrough.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FIsFulfill then
    Result := FOriginalValue
  else
    raise TGocciaThrowValue.Create(FOriginalValue);
end;

procedure TGocciaFinallyPassthrough.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOriginalValue) then
    FOriginalValue.MarkReferences;
end;

{ TGocciaPromiseFinallyWrapper }

constructor TGocciaPromiseFinallyWrapper.Create(const AOnFinally: TGocciaValue; const AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOnFinally := AOnFinally;
  FIsFulfill := AIsFulfill;
end;

function TGocciaPromiseFinallyWrapper.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  OriginalValue, CallbackResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallbackPromise, IntermediatePromise: TGocciaPromiseValue;
  Passthrough: TGocciaFinallyPassthrough;
  PassthroughFn: TGocciaNativeFunctionValue;
  Reaction: TGocciaPromiseReaction;
begin
  OriginalValue := AArgs.GetElement(0);

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
        if not Assigned(CallbackPromise.FReactions) then
          CallbackPromise.FReactions := TList<TGocciaPromiseReaction>.Create;
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

procedure TGocciaPromiseFinallyWrapper.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOnFinally) then
    FOnFinally.MarkReferences;
end;

{ TGocciaPromiseValue }

constructor TGocciaPromiseValue.Create;
begin
  inherited Create(nil);
  FState := gpsPending;
  FResult := TGocciaUndefinedLiteralValue.UndefinedValue;
  FAlreadyResolved := False;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaPromiseValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(PromiseThen, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(PromiseCatch, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(PromiseFinally, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaPromiseValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaPromiseValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

destructor TGocciaPromiseValue.Destroy;
begin
  FReactions.Free;
  inherited;
end;

procedure TGocciaPromiseValue.MarkReferences;
var
  I: Integer;
  Reaction: TGocciaPromiseReaction;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FResult) then
    FResult.MarkReferences;

  if Assigned(FReactions) then
    for I := 0 to FReactions.Count - 1 do
    begin
      Reaction := FReactions[I];
      if Assigned(Reaction.OnFulfilled) then
        Reaction.OnFulfilled.MarkReferences;
      if Assigned(Reaction.OnRejected) then
        Reaction.OnRejected.MarkReferences;
      if Assigned(Reaction.ResultPromise) then
        Reaction.ResultPromise.MarkReferences;
    end;
end;

procedure TGocciaPromiseValue.Resolve(const AValue: TGocciaValue);
var
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if FState <> gpsPending then Exit;

  if AValue = Self then
  begin
    FState := gpsRejected;
    FResult := Goccia.Values.ErrorHelper.CreateErrorObject(TYPE_ERROR_NAME,
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

procedure TGocciaPromiseValue.Reject(const AReason: TGocciaValue);
begin
  if FState <> gpsPending then Exit;

  FState := gpsRejected;
  FResult := AReason;
  TriggerReactions;
end;

procedure TGocciaPromiseValue.SubscribeTo(const APromise: TGocciaPromiseValue);
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
      if not Assigned(APromise.FReactions) then
        APromise.FReactions := TList<TGocciaPromiseReaction>.Create;
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
  if not Assigned(FReactions) or (FReactions.Count = 0) then Exit;
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

function TGocciaPromiseValue.DoResolve(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if AArgs.Length > 0 then
    Resolve(AArgs.GetElement(0))
  else
    Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaPromiseValue.DoReject(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if AArgs.Length > 0 then
    Reject(AArgs.GetElement(0))
  else
    Reject(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

{ Public helper — core then logic without TGocciaArgumentsCollection overhead }

function TGocciaPromiseValue.InvokeThen(
  const AOnFulfilled, AOnRejected: TGocciaValue): TGocciaPromiseValue;
var
  ChildPromise: TGocciaPromiseValue;
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  ChildPromise := TGocciaPromiseValue.Create;
  Queue := TGocciaMicrotaskQueue.Instance;

  case FState of
    gpsPending:
    begin
      Reaction.OnFulfilled := AOnFulfilled;
      Reaction.OnRejected := AOnRejected;
      Reaction.ResultPromise := ChildPromise;
      if not Assigned(FReactions) then
        FReactions := TList<TGocciaPromiseReaction>.Create;
      FReactions.Add(Reaction);
    end;
    gpsFulfilled:
    begin
      Task.Handler := AOnFulfilled;
      Task.Value := FResult;
      Task.ResultPromise := ChildPromise;
      Task.ReactionType := prtFulfill;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
    gpsRejected:
    begin
      Task.Handler := AOnRejected;
      Task.Value := FResult;
      Task.ResultPromise := ChildPromise;
      Task.ReactionType := prtReject;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
  end;

  Result := ChildPromise;
end;

{ Public helper — core finally logic without TGocciaArgumentsCollection overhead }

function TGocciaPromiseValue.InvokeFinally(
  const AOnFinally: TGocciaValue): TGocciaPromiseValue;
var
  WrapFulfilled, WrapRejected: TGocciaPromiseFinallyWrapper;
  WrapFulfilledFn, WrapRejectedFn: TGocciaNativeFunctionValue;
begin
  if not Assigned(AOnFinally) or not AOnFinally.IsCallable then
    Exit(InvokeThen(nil, nil));

  WrapFulfilled := TGocciaPromiseFinallyWrapper.Create(AOnFinally, True);
  WrapRejected := TGocciaPromiseFinallyWrapper.Create(AOnFinally, False);
  WrapFulfilledFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    WrapFulfilled.Invoke, 'finally-fulfill', 1);
  WrapRejectedFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    WrapRejected.Invoke, 'finally-reject', 1);
  Result := InvokeThen(WrapFulfilledFn, WrapRejectedFn);
end;

{ Prototype methods }

function TGocciaPromiseValue.PromiseThen(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
  OnFulfilled, OnRejected: TGocciaValue;
begin
  if not (AThisValue is TGocciaPromiseValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.prototype.then called on non-Promise');

  P := TGocciaPromiseValue(AThisValue);

  OnFulfilled := nil;
  OnRejected := nil;
  if (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable then
    OnFulfilled := AArgs.GetElement(0);
  if (AArgs.Length > 1) and AArgs.GetElement(1).IsCallable then
    OnRejected := AArgs.GetElement(1);

  Result := P.InvokeThen(OnFulfilled, OnRejected);
end;

function TGocciaPromiseValue.PromiseCatch(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ThenArgs: TGocciaArgumentsCollection;
begin
  ThenArgs := TGocciaArgumentsCollection.Create([
    TGocciaUndefinedLiteralValue.UndefinedValue,
    AArgs.GetElement(0)
  ]);
  try
    Result := PromiseThen(ThenArgs, AThisValue);
  finally
    ThenArgs.Free;
  end;
end;

function TGocciaPromiseValue.PromiseFinally(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  OnFinally: TGocciaValue;
begin
  if not (AThisValue is TGocciaPromiseValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.prototype.finally called on non-Promise');

  OnFinally := nil;
  if (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable then
    OnFinally := AArgs.GetElement(0);

  Result := TGocciaPromiseValue(AThisValue).InvokeFinally(OnFinally);
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
