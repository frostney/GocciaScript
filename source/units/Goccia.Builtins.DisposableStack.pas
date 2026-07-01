unit Goccia.Builtins.DisposableStack;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaBuiltinDisposableStack = class(TGocciaBuiltin)
  private
    FDisposableStackPrototype: TGocciaValue;
    FAsyncDisposableStackPrototype: TGocciaValue;
    FDisposableStackConstructorValue: TGocciaValue;
    FAsyncDisposableStackConstructorValue: TGocciaValue;
  published
    function DisposableStackConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncDisposableStackConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Prototype methods (method host for TGocciaNativeFunctionValue)
    function StackUse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StackAdopt(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StackDefer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StackDispose(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StackMove(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StackGetDisposed(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Async variants
    function AsyncStackUse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackAdopt(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackDefer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackDispose(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackMove(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackGetDisposed(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ADefineGlobalBinding: Boolean = True);

    property DisposableStackConstructorValue: TGocciaValue read FDisposableStackConstructorValue;
    property AsyncDisposableStackConstructorValue: TGocciaValue read FAsyncDisposableStackConstructorValue;
  end;

procedure ClearDisposableStackSlotMap;

implementation

uses
  SysUtils,

  HashMap,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.DisposalTracker,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Scope.BindingMap,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue,
  Goccia.VM.Exception;

type
  { Internal slot record for DisposableStack/AsyncDisposableStack instances.
    Stored in a side-channel map keyed by object identity, invisible to JS. }
  TDisposableStackSlot = record
    Tracker: TGocciaDisposalTracker;
    Disposed: Boolean;
    IsAsync: Boolean;
  end;
  PDisposableStackSlot = ^TDisposableStackSlot;

  TAsyncDisposableStackDisposeJob = class(TGocciaObjectValue)
  private
    FSlot: PDisposableStackSlot;
    FIndex: Integer;
    FCurrentError: TGocciaValue;
    FHasError: Boolean;
    FResultPromise: TGocciaPromiseValue;
    procedure AddError(const AError: TGocciaValue);
    procedure Drain;
    procedure ScheduleAwait(const AValue: TGocciaValue);
  public
    constructor Create(const ASlot: PDisposableStackSlot;
      const APromise: TGocciaPromiseValue);
    function AwaitFulfilled(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AwaitRejected(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
    procedure Start;
  end;

threadvar
  GSlotMap: THashMap<TGocciaObjectValue, PDisposableStackSlot>;

function EnsureSlotMap: THashMap<TGocciaObjectValue, PDisposableStackSlot>;
begin
  if not Assigned(GSlotMap) then
    GSlotMap := THashMap<TGocciaObjectValue, PDisposableStackSlot>.Create;
  Result := GSlotMap;
end;

function GetSlot(const AThisValue: TGocciaValue): PDisposableStackSlot;
var
  Obj: TGocciaObjectValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorDisposableStackIncompatibleReceiver, SSuggestDisposableStackThisType);
  Obj := TGocciaObjectValue(AThisValue);
  if not Assigned(GSlotMap) or not GSlotMap.TryGetValue(Obj, Result) then
    ThrowTypeError(SErrorDisposableStackIncompatibleReceiver, SSuggestDisposableStackThisType);
end;

function GetSlotForKind(const AThisValue: TGocciaValue;
  const AExpectedAsync: Boolean): PDisposableStackSlot;
begin
  Result := GetSlot(AThisValue);
  if Result^.IsAsync <> AExpectedAsync then
    ThrowTypeError(SErrorDisposableStackIncompatibleReceiver,
      SSuggestDisposableStackThisType);
end;

constructor TAsyncDisposableStackDisposeJob.Create(
  const ASlot: PDisposableStackSlot; const APromise: TGocciaPromiseValue);
begin
  inherited Create(nil);
  FSlot := ASlot;
  FIndex := ASlot^.Tracker.Count - 1;
  FResultPromise := APromise;
end;

procedure TAsyncDisposableStackDisposeJob.AddError(const AError: TGocciaValue);
begin
  if FHasError then
    FCurrentError := CreateSuppressedErrorObject(AError, FCurrentError)
  else
    FCurrentError := AError;
  FHasError := True;
end;

procedure TAsyncDisposableStackDisposeJob.ScheduleAwait(
  const AValue: TGocciaValue);
var
  AwaitPromise: TGocciaPromiseValue;
  FulfillHandler, RejectHandler: TGocciaNativeFunctionValue;
begin
  AwaitPromise := TGocciaPromiseValue.Create;
  FulfillHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    AwaitFulfilled, '', 1);
  RejectHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    AwaitRejected, '', 1);
  FulfillHandler.CapturedRoot := Self;
  RejectHandler.CapturedRoot := Self;
  try
    if AValue is TGocciaPromiseValue then
      AwaitPromise.SubscribeTo(TGocciaPromiseValue(AValue))
    else
      AwaitPromise.Resolve(AValue);
    AwaitPromise.InvokeThen(FulfillHandler, RejectHandler);
  except
    on E: EGocciaBytecodeThrow do
    begin
      AddError(E.ThrownValue);
      Drain;
    end;
    on E: TGocciaThrowValue do
    begin
      AddError(E.Value);
      Drain;
    end;
  end;
end;

procedure TAsyncDisposableStackDisposeJob.Drain;
var
  Resource: TGocciaDisposableResource;
  CallArgs: TGocciaArgumentsCollection;
  DisposeResult: TGocciaValue;
begin
  while FIndex >= 0 do
  begin
    Resource := FSlot^.Tracker.GetResource(FIndex);
    Dec(FIndex);

    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        if Resource.HasDisposeArgument then
        begin
          CallArgs := TGocciaArgumentsCollection.Create([Resource.DisposeArgument]);
          try
            DisposeResult := InvokeCallable(Resource.DisposeMethod, CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue);
          finally
            CallArgs.Free;
          end;
        end
        else
        begin
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            DisposeResult := InvokeCallable(Resource.DisposeMethod, CallArgs,
              Resource.ResourceValue);
          finally
            CallArgs.Free;
          end;
        end;
        if Resource.Hint = dhAsyncDispose then
        begin
          ScheduleAwait(DisposeResult);
          Exit;
        end;
      except
        on E: EGocciaBytecodeThrow do
          AddError(E.ThrownValue);
        on E: TGocciaThrowValue do
          AddError(E.Value);
      end;
    end
    else if Resource.Hint = dhAsyncDispose then
    begin
      ScheduleAwait(TGocciaUndefinedLiteralValue.UndefinedValue);
      Exit;
    end;
  end;

  if FHasError then
    FResultPromise.Reject(FCurrentError)
  else
    FResultPromise.Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TAsyncDisposableStackDisposeJob.AwaitFulfilled(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Drain;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TAsyncDisposableStackDisposeJob.AwaitRejected(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    AddError(AArgs.GetElement(0))
  else
    AddError(TGocciaUndefinedLiteralValue.UndefinedValue);
  Drain;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TAsyncDisposableStackDisposeJob.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCurrentError) then
    FCurrentError.MarkReferences;
  if Assigned(FResultPromise) then
    FResultPromise.MarkReferences;
end;

procedure TAsyncDisposableStackDisposeJob.Start;
begin
  Drain;
end;

procedure RequireNotDisposed(const ASlot: PDisposableStackSlot);
begin
  if ASlot^.Disposed then
    ThrowReferenceError(SErrorDisposableStackAlreadyDisposed, SSuggestDisposableStackAlreadyDisposed);
end;

function GetAsyncStackUseDisposeMethod(const AValue: TGocciaValue;
  out AHint: TGocciaDisposalHint): TGocciaValue;
var
  Method: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(nil);

  Method := TGocciaObjectValue(AValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownAsyncDispose);
  if Assigned(Method) and not (Method is TGocciaUndefinedLiteralValue) and
     not (Method is TGocciaNullLiteralValue) then
  begin
    if not Method.IsCallable then
      ThrowTypeError(Format(SErrorDisposePropertyNotFunction,
        ['asyncDispose']), SSuggestDisposable);
    AHint := dhAsyncDispose;
    Exit(Method);
  end;

  Method := TGocciaObjectValue(AValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownDispose);
  if Assigned(Method) and not (Method is TGocciaUndefinedLiteralValue) and
     not (Method is TGocciaNullLiteralValue) then
  begin
    if not Method.IsCallable then
      ThrowTypeError(Format(SErrorDisposePropertyNotFunction, ['dispose']),
        SSuggestDisposable);
    AHint := dhSyncDispose;
    Exit(Method);
  end;

  Result := nil;
end;

function CreateDisposableStackInstance(const AIsAsync: Boolean;
  const APrototype: TGocciaObjectValue): TGocciaObjectValue; forward;

// TC39 Explicit Resource Management §3.4.3.2 DisposableStack.prototype.use(value)
function TGocciaBuiltinDisposableStack.StackUse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
begin
  Slot := GetSlotForKind(AThisValue, False);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if (Value is TGocciaUndefinedLiteralValue) or (Value is TGocciaNullLiteralValue) then
  begin
    Result := Value;
    Exit;
  end;

  DisposeMethod := GetDisposeMethod(Value, dhSyncDispose);
  if not Assigned(DisposeMethod) then
    ThrowTypeError(SErrorValueNotDisposable, SSuggestDisposable);

  Slot^.Tracker.AddResource(Value, DisposeMethod, dhSyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.5.3.2 AsyncDisposableStack.prototype.use(value)
function TGocciaBuiltinDisposableStack.AsyncStackUse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
  Hint: TGocciaDisposalHint;
begin
  Slot := GetSlotForKind(AThisValue, True);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if (Value is TGocciaUndefinedLiteralValue) or (Value is TGocciaNullLiteralValue) then
  begin
    Slot^.Tracker.AddResource(Value, nil, dhAsyncDispose);
    Result := Value;
    Exit;
  end;

  DisposeMethod := GetAsyncStackUseDisposeMethod(Value, Hint);
  if not Assigned(DisposeMethod) then
    ThrowTypeError(SErrorValueNotAsyncDisposable, SSuggestDisposable);

  Slot^.Tracker.AddResource(Value, DisposeMethod, Hint);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.4.3.3 DisposableStack.prototype.adopt(value, onDispose)
function TGocciaBuiltinDisposableStack.StackAdopt(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  Value, OnDispose: TGocciaValue;
begin
  Slot := GetSlotForKind(AThisValue, False);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    OnDispose := AArgs.GetElement(1)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError(SErrorOnDisposeMustBeFunction, SSuggestNotFunctionType);

  Slot^.Tracker.AddResourceWithArgument(Value, OnDispose, Value, dhSyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.4.3.4 DisposableStack.prototype.defer(onDispose)
function TGocciaBuiltinDisposableStack.StackDefer(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  OnDispose: TGocciaValue;
begin
  Slot := GetSlotForKind(AThisValue, False);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    OnDispose := AArgs.GetElement(0)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError(SErrorOnDisposeMustBeFunction, SSuggestNotFunctionType);

  Slot^.Tracker.AddResource(TGocciaUndefinedLiteralValue.UndefinedValue, OnDispose, dhSyncDispose);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function DisposeStackForKind(const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  I: Integer;
  Resource: TGocciaDisposableResource;
  CallArgs: TGocciaArgumentsCollection;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  Slot := GetSlotForKind(AThisValue, False);

  // Idempotent: if already disposed, return undefined
  if Slot^.Disposed then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Slot^.Disposed := True;

  CurrentError := nil;
  HasError := False;

  // Dispose in reverse order
  for I := Slot^.Tracker.Count - 1 downto 0 do
  begin
    Resource := Slot^.Tracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        if Resource.HasDisposeArgument then
        begin
          CallArgs := TGocciaArgumentsCollection.Create([Resource.DisposeArgument]);
          try
            InvokeCallable(Resource.DisposeMethod, CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue);
          finally
            CallArgs.Free;
          end;
        end
        else
        begin
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            InvokeCallable(Resource.DisposeMethod, CallArgs,
              Resource.ResourceValue);
          finally
            CallArgs.Free;
          end;
        end;
      except
        on E: EGocciaBytecodeThrow do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.ThrownValue, CurrentError)
          else
            CurrentError := E.ThrownValue;
          HasError := True;
        end;
        on E: TGocciaThrowValue do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.Value, CurrentError)
          else
            CurrentError := E.Value;
          HasError := True;
        end;
      end;
    end;
  end;

  if HasError then
    raise TGocciaThrowValue.Create(CurrentError);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// TC39 Explicit Resource Management §3.4.3.5 DisposableStack.prototype.dispose() / [Symbol.dispose]()
function TGocciaBuiltinDisposableStack.StackDispose(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := DisposeStackForKind(AThisValue);
end;

// TC39 Explicit Resource Management §3.4.3.7 DisposableStack.prototype.move()
function TGocciaBuiltinDisposableStack.StackMove(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot, NewSlot: PDisposableStackSlot;
  NewInstance: TGocciaObjectValue;
begin
  Slot := GetSlotForKind(AThisValue, False);
  RequireNotDisposed(Slot);

  NewInstance := CreateDisposableStackInstance(False,
    TGocciaObjectValue(FDisposableStackPrototype));
  Slot := GetSlot(AThisValue);
  NewSlot := GetSlot(NewInstance);
  NewSlot^.Tracker.MoveResourcesFrom(Slot^.Tracker);
  Slot^.Disposed := True;

  Result := NewInstance;
end;

// TC39 Explicit Resource Management §3.5.3.3 AsyncDisposableStack.prototype.adopt(value, onDispose)
function TGocciaBuiltinDisposableStack.AsyncStackAdopt(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  Value, OnDispose: TGocciaValue;
begin
  Slot := GetSlotForKind(AThisValue, True);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    OnDispose := AArgs.GetElement(1)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError(SErrorOnDisposeMustBeFunction, SSuggestNotFunctionType);

  Slot^.Tracker.AddResourceWithArgument(Value, OnDispose, Value, dhAsyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.5.3.4 AsyncDisposableStack.prototype.defer(onDispose)
function TGocciaBuiltinDisposableStack.AsyncStackDefer(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  OnDispose: TGocciaValue;
begin
  Slot := GetSlotForKind(AThisValue, True);
  RequireNotDisposed(Slot);

  if AArgs.Length > 0 then
    OnDispose := AArgs.GetElement(0)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError(SErrorOnDisposeMustBeFunction, SSuggestNotFunctionType);

  Slot^.Tracker.AddResource(TGocciaUndefinedLiteralValue.UndefinedValue,
    OnDispose, dhAsyncDispose);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// TC39 Explicit Resource Management §3.5.3.5 AsyncDisposableStack.prototype.disposeAsync()
function TGocciaBuiltinDisposableStack.AsyncStackDispose(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Job: TAsyncDisposableStackDisposeJob;
  Promise: TGocciaPromiseValue;
  Slot: PDisposableStackSlot;
begin
  Promise := TGocciaPromiseValue.Create;
  try
    Slot := GetSlotForKind(AThisValue, True);
    if Slot^.Disposed then
    begin
      Promise.Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
      Exit(Promise);
    end
    else
      Slot^.Disposed := True;

    Job := TAsyncDisposableStackDisposeJob.Create(Slot, Promise);
    Job.Start;
  except
    on E: EGocciaBytecodeThrow do
      Promise.Reject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Promise.Reject(E.Value);
  end;
  Result := Promise;
end;

// TC39 Explicit Resource Management §3.5.3.7 AsyncDisposableStack.prototype.move()
function TGocciaBuiltinDisposableStack.AsyncStackMove(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot, NewSlot: PDisposableStackSlot;
  NewInstance: TGocciaObjectValue;
begin
  Slot := GetSlotForKind(AThisValue, True);
  RequireNotDisposed(Slot);

  NewInstance := CreateDisposableStackInstance(True,
    TGocciaObjectValue(FAsyncDisposableStackPrototype));
  Slot := GetSlot(AThisValue);
  NewSlot := GetSlot(NewInstance);
  NewSlot^.IsAsync := Slot^.IsAsync;
  NewSlot^.Tracker.MoveResourcesFrom(Slot^.Tracker);
  Slot^.Disposed := True;

  Result := NewInstance;
end;

// TC39 Explicit Resource Management §3.4.3.6 get DisposableStack.prototype.disposed
function TGocciaBuiltinDisposableStack.StackGetDisposed(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
begin
  Slot := GetSlotForKind(AThisValue, False);
  if Slot^.Disposed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// TC39 Explicit Resource Management §3.5.3.6 get AsyncDisposableStack.prototype.disposed
function TGocciaBuiltinDisposableStack.AsyncStackGetDisposed(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
begin
  Slot := GetSlotForKind(AThisValue, True);
  if Slot^.Disposed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function CreateDisposableStackInstance(const AIsAsync: Boolean;
  const APrototype: TGocciaObjectValue): TGocciaObjectValue;
var
  Instance: TGocciaObjectValue;
  Slot: PDisposableStackSlot;
begin
  Instance := TGocciaObjectValue.Create(APrototype);

  // Allocate internal slot (side-channel, invisible to JS)
  New(Slot);
  Slot^.Tracker := TGocciaDisposalTracker.Create;
  Slot^.Disposed := False;
  Slot^.IsAsync := AIsAsync;
  EnsureSlotMap.AddOrSetValue(Instance, Slot);

  Result := Instance;
end;

{ TGocciaBuiltinDisposableStack }

constructor TGocciaBuiltinDisposableStack.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const ADefineGlobalBinding: Boolean = True);
var
  DisposableStackFunc: TGocciaNativeFunctionValue;
  AsyncDisposableStackFunc: TGocciaNativeFunctionValue;
  UseFunc, AdoptFunc, DeferFunc, DisposeFunc, MoveFunc: TGocciaNativeFunctionValue;
  AsyncUseFunc, AsyncAdoptFunc, AsyncDeferFunc, DisposeAsyncFunc: TGocciaNativeFunctionValue;
  AsyncMoveFunc, DisposedGetterFunc, AsyncDisposedGetterFunc: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FDisposableStackPrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  FAsyncDisposableStackPrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);

  DisposableStackFunc := TGocciaNativeFunctionValue.Create(
    DisposableStackConstructor, CONSTRUCTOR_DISPOSABLE_STACK, 0);
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(DisposableStackFunc, [pfWritable, pfConfigurable]));
  DisposableStackFunc.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(FDisposableStackPrototype, []));
  FDisposableStackConstructorValue := DisposableStackFunc;
  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(CONSTRUCTOR_DISPOSABLE_STACK, DisposableStackFunc, dtConst, True);

  AsyncDisposableStackFunc := TGocciaNativeFunctionValue.Create(
    AsyncDisposableStackConstructor, CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, 0);
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AsyncDisposableStackFunc, [pfWritable, pfConfigurable]));
  AsyncDisposableStackFunc.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(FAsyncDisposableStackPrototype, []));
  FAsyncDisposableStackConstructorValue := AsyncDisposableStackFunc;
  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, AsyncDisposableStackFunc, dtConst, True);

  UseFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackUse, PROP_USE, 1);
  AdoptFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackAdopt, PROP_ADOPT, 2);
  DeferFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackDefer, PROP_DEFER, 1);
  DisposeFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackDispose, 'dispose', 0);
  MoveFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackMove, PROP_MOVE, 0);
  DisposedGetterFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(StackGetDisposed, 'get ' + PROP_DISPOSED, 0);
  AsyncUseFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackUse, PROP_USE, 1);
  AsyncAdoptFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackAdopt, PROP_ADOPT, 2);
  AsyncDeferFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackDefer, PROP_DEFER, 1);
  DisposeAsyncFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackDispose, PROP_DISPOSE_ASYNC, 0);
  AsyncMoveFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackMove, PROP_MOVE, 0);
  AsyncDisposedGetterFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncStackGetDisposed, 'get ' + PROP_DISPOSED, 0);

  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_USE,
    TGocciaPropertyDescriptorData.Create(UseFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_ADOPT,
    TGocciaPropertyDescriptorData.Create(AdoptFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_DEFER,
    TGocciaPropertyDescriptorData.Create(DeferFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty('dispose',
    TGocciaPropertyDescriptorData.Create(DisposeFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_MOVE,
    TGocciaPropertyDescriptorData.Create(MoveFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineProperty(PROP_DISPOSED,
    TGocciaPropertyDescriptorAccessor.Create(DisposedGetterFunc, nil, [pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineSymbolProperty(TGocciaSymbolValue.WellKnownDispose,
    TGocciaPropertyDescriptorData.Create(DisposeFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FDisposableStackPrototype).DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_DISPOSABLE_STACK), [pfConfigurable]));

  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_USE,
    TGocciaPropertyDescriptorData.Create(AsyncUseFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_ADOPT,
    TGocciaPropertyDescriptorData.Create(AsyncAdoptFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_DEFER,
    TGocciaPropertyDescriptorData.Create(AsyncDeferFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_DISPOSE_ASYNC,
    TGocciaPropertyDescriptorData.Create(DisposeAsyncFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_MOVE,
    TGocciaPropertyDescriptorData.Create(AsyncMoveFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_DISPOSED,
    TGocciaPropertyDescriptorAccessor.Create(AsyncDisposedGetterFunc, nil, [pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineSymbolProperty(TGocciaSymbolValue.WellKnownAsyncDispose,
    TGocciaPropertyDescriptorData.Create(DisposeAsyncFunc, [pfWritable, pfConfigurable]));
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_ASYNC_DISPOSABLE_STACK), [pfConfigurable]));
end;

function TGocciaBuiltinDisposableStack.DisposableStackConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateDisposableStackInstance(False,
    TGocciaObjectValue(FDisposableStackPrototype));
end;

function TGocciaBuiltinDisposableStack.AsyncDisposableStackConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateDisposableStackInstance(True,
    TGocciaObjectValue(FAsyncDisposableStackPrototype));
end;

procedure ClearDisposableStackSlotMap;
var
  Pair: THashMap<TGocciaObjectValue, PDisposableStackSlot>.TKeyValuePair;
begin
  if Assigned(GSlotMap) then
  begin
    for Pair in GSlotMap do
    begin
      Pair.Value^.Tracker.Free;
      Dispose(Pair.Value);
    end;
    GSlotMap.Free;
    GSlotMap := nil;
  end;
end;

initialization
  // FPC does not auto-finalize managed threadvars at thread exit. The registry
  // drain releases this thread's slot map on worker exit (ShutdownThreadRuntime)
  // and on the main thread (Goccia.ThreadCleanupRegistry's finalization).
  RegisterThreadvarCleanup(@ClearDisposableStackSlotMap);

end.
