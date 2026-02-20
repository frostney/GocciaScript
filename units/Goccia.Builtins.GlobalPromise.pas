unit Goccia.Builtins.GlobalPromise;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaGlobalPromise = class(TGocciaBuiltin)
  private
    FPromiseConstructor: TGocciaNativeFunctionValue;

    function PromiseConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseResolve(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseReject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAllSettled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseRace(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAny(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ExtractPromiseArray(const AArgs: TGocciaArgumentsCollection): TGocciaArrayValue;
    function PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue;

type
  TPromiseAllState = class(TGocciaObjectValue)
  private
    FResults: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
    procedure MarkReferences; override;
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
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
  public
    constructor Create(const AState: TPromiseAllState);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllSettledFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllSettledRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseRaceHandler = class(TGocciaObjectValue)
  private
    FResultPromise: TGocciaPromiseValue;
    FIsResolve: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const AIsResolve: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAnyState = class(TGocciaObjectValue)
  private
    FErrors: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
    procedure MarkReferences; override;
    property Errors: TGocciaArrayValue read FErrors;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaPromiseValue read FResultPromise;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseAnyFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
  public
    constructor Create(const AState: TPromiseAnyState);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAnyRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAnyState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ Helper: wrap value as Promise if not already one }
function WrapAsPromise(const AValue: TGocciaValue): TGocciaPromiseValue;
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

constructor TPromiseAllState.Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
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

procedure TPromiseAllState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResults) then FResults.MarkReferences;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAllHandler }

constructor TPromiseAllHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Results.Elements[FIndex] := AArgs.GetElement(0);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllRejectHandler }

constructor TPromiseAllRejectHandler.Create(const AState: TPromiseAllState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAllRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Reject(AArgs.GetElement(0));
end;

procedure TPromiseAllRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllSettledFulfillHandler }

constructor TPromiseAllSettledFulfillHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledFulfillHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('fulfilled'));
  Entry.AssignProperty('value', AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledFulfillHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllSettledRejectHandler }

constructor TPromiseAllSettledRejectHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('rejected'));
  Entry.AssignProperty('reason', AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseRaceHandler }

constructor TPromiseRaceHandler.Create(const AResultPromise: TGocciaPromiseValue; const AIsResolve: Boolean);
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FIsResolve := AIsResolve;
end;

function TPromiseRaceHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FIsResolve then
    FResultPromise.Resolve(AArgs.GetElement(0))
  else
    FResultPromise.Reject(AArgs.GetElement(0));
end;

procedure TPromiseRaceHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAnyState }

constructor TPromiseAnyState.Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
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

procedure TPromiseAnyState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FErrors) then FErrors.MarkReferences;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAnyFulfillHandler }

constructor TPromiseAnyFulfillHandler.Create(const AState: TPromiseAnyState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAnyFulfillHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Resolve(AArgs.GetElement(0));
end;

procedure TPromiseAnyFulfillHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAnyRejectHandler }

constructor TPromiseAnyRejectHandler.Create(const AState: TPromiseAnyState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAnyRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Errors.Elements[FIndex] := AArgs.GetElement(0);
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

procedure TPromiseAnyRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
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
  FPromiseConstructor.AssignProperty('withResolvers',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseWithResolvers, 'withResolvers', 0));
  FPromiseConstructor.AssignProperty('try',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseTry, 'try', 1));

  AScope.DefineLexicalBinding(AName, FPromiseConstructor, dtLet);
end;

function TGocciaGlobalPromise.PromiseConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Executor: TGocciaValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ExecutorArgs, RejectArgs: TGocciaArgumentsCollection;
begin
  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise resolver undefined is not a function');

  Executor := AArgs.GetElement(0);
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

function TGocciaGlobalPromise.PromiseResolve(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
  P: TGocciaPromiseValue;
begin
  Value := AArgs.GetElement(0);

  if Value is TGocciaPromiseValue then
  begin
    Result := Value;
    Exit;
  end;

  P := TGocciaPromiseValue.Create;
  P.Resolve(Value);
  Result := P;
end;

function TGocciaGlobalPromise.PromiseReject(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
begin
  P := TGocciaPromiseValue.Create;
  P.Reject(AArgs.GetElement(0));
  Result := P;
end;

function TGocciaGlobalPromise.ExtractPromiseArray(const AArgs: TGocciaArgumentsCollection): TGocciaArrayValue;
var
  Iterable: TGocciaValue;
  Str: string;
  I: Integer;
begin
  Iterable := AArgs.GetElement(0);
  if Iterable is TGocciaArrayValue then
    Result := TGocciaArrayValue(Iterable)
  else if Iterable is TGocciaStringLiteralValue then
  begin
    Str := TGocciaStringLiteralValue(Iterable).Value;
    Result := TGocciaArrayValue.Create;
    TGocciaGarbageCollector.Instance.AddTempRoot(Result);
    try
      for I := 1 to Length(Str) do
        Result.Elements.Add(TGocciaStringLiteralValue.Create(Str[I]));
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Result);
    end;
  end
  else if Iterable is TGocciaSetValue then
    Result := TGocciaSetValue(Iterable).ToArray
  else if Iterable is TGocciaMapValue then
    Result := TGocciaMapValue(Iterable).ToArray
  else
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      Iterable.ToStringLiteral.Value + ' is not iterable');
    Result := nil;
  end;
end;

function TGocciaGlobalPromise.PromiseAll(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
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
  ResultPromise := TGocciaPromiseValue.Create;
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

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

function TGocciaGlobalPromise.PromiseAllSettled(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
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
  ResultPromise := TGocciaPromiseValue.Create;
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

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

function TGocciaGlobalPromise.PromiseRace(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  ResolveHandler, RejectHandler: TPromiseRaceHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  ResultPromise := TGocciaPromiseValue.Create;
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

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

function TGocciaGlobalPromise.PromiseAny(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
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
  ResultPromise := TGocciaPromiseValue.Create;
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

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

function TGocciaGlobalPromise.PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
begin
  Promise := TGocciaPromiseValue.Create;
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);

  ResultObj := TGocciaObjectValue.Create;
  ResultObj.AssignProperty('promise', Promise);
  ResultObj.AssignProperty('resolve', ResolveFn);
  ResultObj.AssignProperty('reject', RejectFn);

  Result := ResultObj;
end;

function TGocciaGlobalPromise.PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Callback: TGocciaValue;
  CallbackResult: TGocciaValue;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  Promise := TGocciaPromiseValue.Create;

  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.try requires a callback function');

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.try requires a callback function');

  try
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      CallbackResult := TGocciaFunctionBase(Callback).Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      EmptyArgs.Free;
    end;
    Promise.Resolve(CallbackResult);
  except
    on E: TGocciaThrowValue do
      Promise.Reject(E.Value);
    on E: Exception do
      Promise.Reject(CreateErrorObject('Error', E.Message));
  end;

  Result := Promise;
end;

end.
