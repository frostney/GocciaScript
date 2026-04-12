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
  Goccia.Scope.BindingMap,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

var
  GTrackerMap: THashMap<TGocciaObjectValue, TGocciaDisposalTracker>;

function EnsureTrackerMap: THashMap<TGocciaObjectValue, TGocciaDisposalTracker>;
begin
  if not Assigned(GTrackerMap) then
    GTrackerMap := THashMap<TGocciaObjectValue, TGocciaDisposalTracker>.Create;
  Result := GTrackerMap;
end;

function GetStackTracker(const AThisValue: TGocciaValue): TGocciaDisposalTracker;
var
  Obj: TGocciaObjectValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('DisposableStack method called on incompatible receiver');
  Obj := TGocciaObjectValue(AThisValue);
  if not Assigned(GTrackerMap) or not GTrackerMap.TryGetValue(Obj, Result) then
    ThrowTypeError('DisposableStack method called on incompatible receiver');
end;

function IsStackDisposed(const AObj: TGocciaObjectValue): Boolean;
var
  DisposedVal: TGocciaValue;
begin
  DisposedVal := AObj.GetProperty('__disposed__');
  Result := Assigned(DisposedVal) and (DisposedVal is TGocciaBooleanLiteralValue) and
    TGocciaBooleanLiteralValue(DisposedVal).Value;
end;

procedure RequireNotDisposed(const AObj: TGocciaObjectValue);
begin
  if IsStackDisposed(AObj) then
    ThrowReferenceError('DisposableStack has already been disposed');
end;

// TC39 Explicit Resource Management §3.4.3.2 DisposableStack.prototype.use(value)
function TGocciaBuiltinDisposableStack.StackUse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Tracker: TGocciaDisposalTracker;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
begin
  RequireNotDisposed(TGocciaObjectValue(AThisValue));
  Tracker := GetStackTracker(AThisValue);

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
    ThrowTypeError('The value is not disposable (missing [Symbol.dispose])');

  Tracker.AddResource(Value, DisposeMethod, dhSyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.4.3.3 DisposableStack.prototype.adopt(value, onDispose)
function TGocciaBuiltinDisposableStack.StackAdopt(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Tracker: TGocciaDisposalTracker;
  Value, OnDispose: TGocciaValue;
begin
  RequireNotDisposed(TGocciaObjectValue(AThisValue));
  Tracker := GetStackTracker(AThisValue);

  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    OnDispose := AArgs.GetElement(1)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError('The onDispose argument must be a function');

  Tracker.AddResource(Value, OnDispose, dhSyncDispose);
  Result := Value;
end;

// TC39 Explicit Resource Management §3.4.3.4 DisposableStack.prototype.defer(onDispose)
function TGocciaBuiltinDisposableStack.StackDefer(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Tracker: TGocciaDisposalTracker;
  OnDispose: TGocciaValue;
begin
  RequireNotDisposed(TGocciaObjectValue(AThisValue));
  Tracker := GetStackTracker(AThisValue);

  if AArgs.Length > 0 then
    OnDispose := AArgs.GetElement(0)
  else
    OnDispose := TGocciaUndefinedLiteralValue.UndefinedValue;

  if not OnDispose.IsCallable then
    ThrowTypeError('The onDispose argument must be a function');

  Tracker.AddResource(TGocciaUndefinedLiteralValue.UndefinedValue, OnDispose, dhSyncDispose);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// TC39 Explicit Resource Management §3.4.3.5 DisposableStack.prototype.dispose() / [Symbol.dispose]()
function TGocciaBuiltinDisposableStack.StackDispose(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Tracker: TGocciaDisposalTracker;
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  Obj := TGocciaObjectValue(AThisValue);

  if IsStackDisposed(Obj) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Obj.AssignProperty('__disposed__', TGocciaBooleanLiteralValue.TrueValue);
  Tracker := GetStackTracker(AThisValue);

  CurrentError := nil;
  HasError := False;

  for I := Tracker.Count - 1 downto 0 do
  begin
    Resource := Tracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        TGocciaFunctionBase(Resource.DisposeMethod).CallNoArgs(Resource.ResourceValue);
      except
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

// TC39 Explicit Resource Management §3.4.3.6 get DisposableStack.prototype.disposed
function TGocciaBuiltinDisposableStack.StackGetDisposed(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('DisposableStack getter called on incompatible receiver');
  if IsStackDisposed(TGocciaObjectValue(AThisValue)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function CreateDisposableStackInstance(
  const AHost: TGocciaBuiltinDisposableStack;
  const AToStringTag: string): TGocciaObjectValue;
var
  Instance: TGocciaObjectValue;
  Tracker: TGocciaDisposalTracker;
  UseFunc, AdoptFunc, DeferFunc, DisposeFunc: TGocciaNativeFunctionValue;
  DisposedGetterFunc: TGocciaNativeFunctionValue;
begin
  Instance := TGocciaObjectValue.Create;
  Instance.AssignProperty('__disposed__', TGocciaBooleanLiteralValue.FalseValue);

  Tracker := TGocciaDisposalTracker.Create;
  EnsureTrackerMap.AddOrSetValue(Instance, Tracker);

  UseFunc := TGocciaNativeFunctionValue.Create(AHost.StackUse, PROP_USE, 1);
  AdoptFunc := TGocciaNativeFunctionValue.Create(AHost.StackAdopt, PROP_ADOPT, 2);
  DeferFunc := TGocciaNativeFunctionValue.Create(AHost.StackDefer, PROP_DEFER, 1);
  DisposeFunc := TGocciaNativeFunctionValue.Create(AHost.StackDispose, 'dispose', 0);
  DisposedGetterFunc := TGocciaNativeFunctionValue.Create(AHost.StackGetDisposed, PROP_DISPOSED, 0);

  Instance.AssignProperty(PROP_USE, UseFunc);
  Instance.AssignProperty(PROP_ADOPT, AdoptFunc);
  Instance.AssignProperty(PROP_DEFER, DeferFunc);
  Instance.AssignProperty('dispose', DisposeFunc);

  Instance.DefineProperty(PROP_DISPOSED,
    TGocciaPropertyDescriptorAccessor.Create(DisposedGetterFunc, nil,
      [pfConfigurable, pfEnumerable]));

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
  Result := CreateDisposableStackInstance(Self, CONSTRUCTOR_DISPOSABLE_STACK);
end;

function TGocciaBuiltinDisposableStack.AsyncDisposableStackConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateDisposableStackInstance(Self, CONSTRUCTOR_ASYNC_DISPOSABLE_STACK);
end;

initialization

finalization
  if Assigned(GTrackerMap) then
  begin
    GTrackerMap.Free;
    GTrackerMap := nil;
  end;

end.
