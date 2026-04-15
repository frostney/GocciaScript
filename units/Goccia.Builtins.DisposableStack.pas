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
    function StackGetDisposed(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Async variants
    function AsyncStackUse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncStackDispose(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

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
  end;
  PDisposableStackSlot = ^TDisposableStackSlot;

var
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

procedure RequireNotDisposed(const ASlot: PDisposableStackSlot);
begin
  if ASlot^.Disposed then
    ThrowReferenceError(SErrorDisposableStackAlreadyDisposed, SSuggestDisposableStackAlreadyDisposed);
end;

// TC39 Explicit Resource Management §3.4.3.2 DisposableStack.prototype.use(value)
function TGocciaBuiltinDisposableStack.StackUse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
begin
  Slot := GetSlot(AThisValue);
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
  Slot := GetSlot(AThisValue);
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
  Slot := GetSlot(AThisValue);
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

  Slot^.Tracker.AddResource(Value, OnDispose, dhSyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.4.3.4 DisposableStack.prototype.defer(onDispose)
function TGocciaBuiltinDisposableStack.StackDefer(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  OnDispose: TGocciaValue;
begin
  Slot := GetSlot(AThisValue);
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

// TC39 Explicit Resource Management §3.4.3.5 DisposableStack.prototype.dispose() / [Symbol.dispose]()
function TGocciaBuiltinDisposableStack.StackDispose(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  Slot := GetSlot(AThisValue);

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

// TC39 Explicit Resource Management §3.5.3.5 AsyncDisposableStack.prototype.disposeAsync()
function TGocciaBuiltinDisposableStack.AsyncStackDispose(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Delegate to sync dispose — full async awaiting requires Promise integration
  Result := StackDispose(AArgs, AThisValue);
end;

// TC39 Explicit Resource Management §3.4.3.6 get DisposableStack.prototype.disposed
function TGocciaBuiltinDisposableStack.StackGetDisposed(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Slot: PDisposableStackSlot;
begin
  Slot := GetSlot(AThisValue);
  if Slot^.Disposed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function CreateDisposableStackInstance(
  const AHost: TGocciaBuiltinDisposableStack;
  const AToStringTag: string;
  const AIsAsync: Boolean): TGocciaObjectValue;
var
  Instance: TGocciaObjectValue;
  Slot: PDisposableStackSlot;
  UseFunc, AdoptFunc, DeferFunc, DisposeFunc: TGocciaNativeFunctionValue;
  DisposedGetterFunc: TGocciaNativeFunctionValue;
begin
  Instance := TGocciaObjectValue.Create;

  // Allocate internal slot (side-channel, invisible to JS)
  New(Slot);
  Slot^.Tracker := TGocciaDisposalTracker.Create;
  Slot^.Disposed := False;
  EnsureSlotMap.AddOrSetValue(Instance, Slot);

  // Use async-aware methods for AsyncDisposableStack
  if AIsAsync then
    UseFunc := TGocciaNativeFunctionValue.Create(AHost.AsyncStackUse, PROP_USE, 1)
  else
    UseFunc := TGocciaNativeFunctionValue.Create(AHost.StackUse, PROP_USE, 1);
  AdoptFunc := TGocciaNativeFunctionValue.Create(AHost.StackAdopt, PROP_ADOPT, 2);
  DeferFunc := TGocciaNativeFunctionValue.Create(AHost.StackDefer, PROP_DEFER, 1);

  if AIsAsync then
    DisposeFunc := TGocciaNativeFunctionValue.Create(AHost.AsyncStackDispose, PROP_DISPOSE_ASYNC, 0)
  else
    DisposeFunc := TGocciaNativeFunctionValue.Create(AHost.StackDispose, 'dispose', 0);
  DisposedGetterFunc := TGocciaNativeFunctionValue.Create(AHost.StackGetDisposed, PROP_DISPOSED, 0);

  Instance.AssignProperty(PROP_USE, UseFunc);
  Instance.AssignProperty(PROP_ADOPT, AdoptFunc);
  Instance.AssignProperty(PROP_DEFER, DeferFunc);

  if AIsAsync then
    Instance.AssignProperty(PROP_DISPOSE_ASYNC, DisposeFunc)
  else
    Instance.AssignProperty('dispose', DisposeFunc);

  Instance.DefineProperty(PROP_DISPOSED,
    TGocciaPropertyDescriptorAccessor.Create(DisposedGetterFunc, nil,
      [pfConfigurable, pfEnumerable]));

  // Wire up the correct well-known symbol
  if AIsAsync then
    Instance.AssignSymbolProperty(TGocciaSymbolValue.WellKnownAsyncDispose, DisposeFunc)
  else
    Instance.AssignSymbolProperty(TGocciaSymbolValue.WellKnownDispose, DisposeFunc);

  Instance.AssignSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaStringLiteralValue.Create(AToStringTag));

  Result := Instance;
end;

{ TGocciaBuiltinDisposableStack }

constructor TGocciaBuiltinDisposableStack.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  DisposableStackFunc: TGocciaNativeFunctionValue;
  AsyncDisposableStackFunc: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  DisposableStackFunc := TGocciaNativeFunctionValue.Create(
    DisposableStackConstructor, CONSTRUCTOR_DISPOSABLE_STACK, 0);
  AScope.DefineLexicalBinding(CONSTRUCTOR_DISPOSABLE_STACK, DisposableStackFunc, dtConst);

  AsyncDisposableStackFunc := TGocciaNativeFunctionValue.Create(
    AsyncDisposableStackConstructor, CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, 0);
  AScope.DefineLexicalBinding(CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, AsyncDisposableStackFunc, dtConst);
end;

function TGocciaBuiltinDisposableStack.DisposableStackConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateDisposableStackInstance(Self, CONSTRUCTOR_DISPOSABLE_STACK, False);
end;

function TGocciaBuiltinDisposableStack.AsyncDisposableStackConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateDisposableStackInstance(Self, CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, True);
end;

initialization

finalization
  if Assigned(GSlotMap) then
  begin
    GSlotMap.Free;
    GSlotMap := nil;
  end;

end.
