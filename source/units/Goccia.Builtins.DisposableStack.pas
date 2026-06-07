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
      const AThrowError: TGocciaThrowErrorCallback);
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
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
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

procedure RequireNotDisposed(const ASlot: PDisposableStackSlot);
begin
  if ASlot^.Disposed then
    ThrowReferenceError(SErrorDisposableStackAlreadyDisposed, SSuggestDisposableStackAlreadyDisposed);
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
begin
  Slot := GetSlotForKind(AThisValue, True);
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

  DisposeMethod := GetDisposeMethod(Value, dhAsyncDispose);
  if not Assigned(DisposeMethod) then
    ThrowTypeError(SErrorValueNotAsyncDisposable, SSuggestDisposable);

  Slot^.Tracker.AddResource(Value, DisposeMethod, dhAsyncDispose);
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

function DisposeStackForKind(const AThisValue: TGocciaValue;
  const AExpectedAsync: Boolean): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  Slot := GetSlotForKind(AThisValue, AExpectedAsync);

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
          TGocciaFunctionBase(Resource.DisposeMethod).CallOneArg(
            Resource.DisposeArgument,
            TGocciaUndefinedLiteralValue.UndefinedValue)
        else
          TGocciaFunctionBase(Resource.DisposeMethod).CallNoArgs(Resource.ResourceValue);
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
  Result := DisposeStackForKind(AThisValue, False);
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
begin
  Result := DisposeStackForKind(AThisValue, True);
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
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
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
  AScope.DefineLexicalBinding(CONSTRUCTOR_DISPOSABLE_STACK, DisposableStackFunc, dtConst, True);

  AsyncDisposableStackFunc := TGocciaNativeFunctionValue.Create(
    AsyncDisposableStackConstructor, CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, 0);
  TGocciaObjectValue(FAsyncDisposableStackPrototype).DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AsyncDisposableStackFunc, [pfWritable, pfConfigurable]));
  AsyncDisposableStackFunc.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(FAsyncDisposableStackPrototype, []));
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

finalization
  ClearDisposableStackSlotMap;

end.
