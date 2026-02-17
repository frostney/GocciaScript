unit Goccia.Builtins.GlobalPromise;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error.ThrowErrorCallback,
  Goccia.Values.Primitives, Goccia.Values.PromiseValue,
  Goccia.Values.NativeFunction, Goccia.Values.ObjectValue,
  Goccia.Values.ArrayValue, Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaGlobalPromise = class(TGocciaBuiltin)
  private
    FPromiseConstructor: TGocciaNativeFunctionValue;

    function PromiseConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseResolve(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseReject(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseAll(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseAllSettled(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseRace(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PromiseAny(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ExtractPromiseArray(Args: TGocciaArgumentsCollection): TGocciaArrayValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.FunctionBase, Goccia.Values.Error, Goccia.Values.ErrorHelper,
  Goccia.GarbageCollector, Goccia.MicrotaskQueue;

type
  TPromiseAllState = class(TGocciaObjectValue)
  private
    FResults: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(AResultPromise: TGocciaPromiseValue; ACount: Integer);
    procedure GCMarkReferences; override;
    property Results: TGocciaArrayValue read FResults;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaPromiseValue read FResultPromise;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseAllHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(AState: TPromiseAllState; AIndex: Integer);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseAllRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
  public
    constructor Create(AState: TPromiseAllState);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseAllSettledFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(AState: TPromiseAllState; AIndex: Integer);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseAllSettledRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(AState: TPromiseAllState; AIndex: Integer);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseRaceHandler = class(TGocciaObjectValue)
  private
    FResultPromise: TGocciaPromiseValue;
    FIsResolve: Boolean;
  public
    constructor Create(AResultPromise: TGocciaPromiseValue; AIsResolve: Boolean);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseAnyState = class(TGocciaObjectValue)
  private
    FErrors: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(AResultPromise: TGocciaPromiseValue; ACount: Integer);
    procedure GCMarkReferences; override;
    property Errors: TGocciaArrayValue read FErrors;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaPromiseValue read FResultPromise;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseAnyFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
  public
    constructor Create(AState: TPromiseAnyState);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

  TPromiseAnyRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
    FIndex: Integer;
  public
    constructor Create(AState: TPromiseAnyState; AIndex: Integer);
    function Invoke(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure GCMarkReferences; override;
  end;

{ Helper: wrap value as Promise if not already one }
function WrapAsPromise(AValue: TGocciaValue): TGocciaPromiseValue;
var
  P: TGocciaPromiseValue;
begin
  if AValue is TGocciaPromiseValue then
    Result := TGocciaPromiseValue(AValue)
  else
  begin
    P := TGocciaPromiseValue.Create;
    P.Resolve(AValue);
    Result := P;
  end;
end;

{ TPromiseAllState }

constructor TPromiseAllState.Create(AResultPromise: TGocciaPromiseValue; ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FRemaining := ACount;
  FSettled := False;
  FResults := TGocciaArrayValue.Create;
  for I := 0 to ACount - 1 do
    FResults.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TPromiseAllState.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResults) then FResults.GCMarkReferences;
  if Assigned(FResultPromise) then FResultPromise.GCMarkReferences;
end;

{ TPromiseAllHandler }

constructor TPromiseAllHandler.Create(AState: TPromiseAllState; AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Results.Elements[FIndex] := Args.GetElement(0);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TPromiseAllRejectHandler }

constructor TPromiseAllRejectHandler.Create(AState: TPromiseAllState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAllRejectHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Reject(Args.GetElement(0));
end;

procedure TPromiseAllRejectHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TPromiseAllSettledFulfillHandler }

constructor TPromiseAllSettledFulfillHandler.Create(AState: TPromiseAllState; AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledFulfillHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('fulfilled'));
  Entry.AssignProperty('value', Args.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledFulfillHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TPromiseAllSettledRejectHandler }

constructor TPromiseAllSettledRejectHandler.Create(AState: TPromiseAllState; AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledRejectHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('rejected'));
  Entry.AssignProperty('reason', Args.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledRejectHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TPromiseRaceHandler }

constructor TPromiseRaceHandler.Create(AResultPromise: TGocciaPromiseValue; AIsResolve: Boolean);
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FIsResolve := AIsResolve;
end;

function TPromiseRaceHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FIsResolve then
    FResultPromise.Resolve(Args.GetElement(0))
  else
    FResultPromise.Reject(Args.GetElement(0));
end;

procedure TPromiseRaceHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResultPromise) then FResultPromise.GCMarkReferences;
end;

{ TPromiseAnyState }

constructor TPromiseAnyState.Create(AResultPromise: TGocciaPromiseValue; ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FRemaining := ACount;
  FSettled := False;
  FErrors := TGocciaArrayValue.Create;
  for I := 0 to ACount - 1 do
    FErrors.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TPromiseAnyState.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FErrors) then FErrors.GCMarkReferences;
  if Assigned(FResultPromise) then FResultPromise.GCMarkReferences;
end;

{ TPromiseAnyFulfillHandler }

constructor TPromiseAnyFulfillHandler.Create(AState: TPromiseAnyState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAnyFulfillHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Resolve(Args.GetElement(0));
end;

procedure TPromiseAnyFulfillHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TPromiseAnyRejectHandler }

constructor TPromiseAnyRejectHandler.Create(AState: TPromiseAnyState; AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAnyRejectHandler.Invoke(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Errors.Elements[FIndex] := Args.GetElement(0);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject('AggregateError',
      'All promises were rejected');
    ErrorObj.AssignProperty('errors', FState.Errors);
    FState.ResultPromise.Reject(ErrorObj);
  end;
end;

procedure TPromiseAnyRejectHandler.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.GCMarkReferences;
end;

{ TGocciaGlobalPromise }

constructor TGocciaGlobalPromise.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FPromiseConstructor := TGocciaNativeFunctionValue.Create(PromiseConstructorFn, 'Promise', 1);

  TGocciaPromiseValue.ExposePrototype(FPromiseConstructor);

  FPromiseConstructor.AssignProperty('resolve',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseResolve, 'resolve', 1));
  FPromiseConstructor.AssignProperty('reject',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseReject, 'reject', 1));
  FPromiseConstructor.AssignProperty('all',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAll, 'all', 1));
  FPromiseConstructor.AssignProperty('allSettled',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAllSettled, 'allSettled', 1));
  FPromiseConstructor.AssignProperty('race',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseRace, 'race', 1));
  FPromiseConstructor.AssignProperty('any',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAny, 'any', 1));

  AScope.DefineLexicalBinding(AName, FPromiseConstructor, dtLet);
end;

function TGocciaGlobalPromise.PromiseConstructorFn(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Executor: TGocciaValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ExecutorArgs, RejectArgs: TGocciaArgumentsCollection;
begin
  if Args.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise resolver undefined is not a function');

  Executor := Args.GetElement(0);
  if not Executor.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise resolver is not a function');

  Promise := TGocciaPromiseValue.Create;
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);

  ExecutorArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  try
    try
      TGocciaFunctionBase(Executor).Call(ExecutorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    except
      on E: TGocciaThrowValue do
      begin
        RejectArgs := TGocciaArgumentsCollection.Create([E.Value]);
        try
          Promise.DoReject(RejectArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          RejectArgs.Free;
        end;
      end;
    end;
  finally
    ExecutorArgs.Free;
  end;

  Result := Promise;
end;

function TGocciaGlobalPromise.PromiseResolve(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
  P: TGocciaPromiseValue;
begin
  Value := Args.GetElement(0);

  if Value is TGocciaPromiseValue then
  begin
    Result := Value;
    Exit;
  end;

  P := TGocciaPromiseValue.Create;
  P.Resolve(Value);
  Result := P;
end;

function TGocciaGlobalPromise.PromiseReject(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
begin
  P := TGocciaPromiseValue.Create;
  P.Reject(Args.GetElement(0));
  Result := P;
end;

function TGocciaGlobalPromise.ExtractPromiseArray(Args: TGocciaArgumentsCollection): TGocciaArrayValue;
var
  Iterable: TGocciaValue;
begin
  Iterable := Args.GetElement(0);
  if Iterable is TGocciaArrayValue then
    Result := TGocciaArrayValue(Iterable)
  else
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise combinator requires an array argument');
    Result := nil;
  end;
end;

function TGocciaGlobalPromise.PromiseAll(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAllState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAllHandler;
  RejectHandler: TPromiseAllRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  InputArray := ExtractPromiseArray(Args);
  ResultPromise := TGocciaPromiseValue.Create;

  if InputArray.Elements.Count = 0 then
  begin
    ResultPromise.Resolve(TGocciaArrayValue.Create);
    Result := ResultPromise;
    Exit;
  end;

  State := TPromiseAllState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAllHandler.Create(State, I);
    RejectHandler := TPromiseAllRejectHandler.Create(State);

    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'all-resolve', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'all-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  Result := ResultPromise;
end;

function TGocciaGlobalPromise.PromiseAllSettled(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAllState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAllSettledFulfillHandler;
  RejectHandler: TPromiseAllSettledRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  InputArray := ExtractPromiseArray(Args);
  ResultPromise := TGocciaPromiseValue.Create;

  if InputArray.Elements.Count = 0 then
  begin
    ResultPromise.Resolve(TGocciaArrayValue.Create);
    Result := ResultPromise;
    Exit;
  end;

  State := TPromiseAllState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAllSettledFulfillHandler.Create(State, I);
    RejectHandler := TPromiseAllSettledRejectHandler.Create(State, I);

    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'allSettled-fulfill', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'allSettled-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  Result := ResultPromise;
end;

function TGocciaGlobalPromise.PromiseRace(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  ResolveHandler, RejectHandler: TPromiseRaceHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  InputArray := ExtractPromiseArray(Args);
  ResultPromise := TGocciaPromiseValue.Create;

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    ResolveHandler := TPromiseRaceHandler.Create(ResultPromise, True);
    RejectHandler := TPromiseRaceHandler.Create(ResultPromise, False);

    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ResolveHandler.Invoke, 'race-resolve', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'race-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  Result := ResultPromise;
end;

function TGocciaGlobalPromise.PromiseAny(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAnyState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAnyFulfillHandler;
  RejectHandler: TPromiseAnyRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
  ErrorObj: TGocciaObjectValue;
begin
  InputArray := ExtractPromiseArray(Args);
  ResultPromise := TGocciaPromiseValue.Create;

  if InputArray.Elements.Count = 0 then
  begin
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject('AggregateError',
      'All promises were rejected');
    ErrorObj.AssignProperty('errors', TGocciaArrayValue.Create);
    ResultPromise.Reject(ErrorObj);
    Result := ResultPromise;
    Exit;
  end;

  State := TPromiseAnyState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAnyFulfillHandler.Create(State);
    RejectHandler := TPromiseAnyRejectHandler.Create(State, I);

    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'any-fulfill', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'any-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  Result := ResultPromise;
end;

end.
