unit Goccia.Values.AsyncFunctionValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.Primitives;

type
  TGocciaAsyncFunctionValue = class(TGocciaFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue; override;
  end;

  TGocciaAsyncArrowFunctionValue = class(TGocciaArrowFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue; override;
  end;

  TGocciaAsyncMethodValue = class(TGocciaMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Values.Error,
  Goccia.Values.PromiseValue;

{ TGocciaAsyncFunctionValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PushActiveScope(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.PopActiveScope;
    end;
  finally
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

function TGocciaAsyncFunctionValue.CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);
  Result := TGocciaAsyncFunctionValue.Create(FParameters, ClonedStatements, ANewScope, FName);
end;

{ TGocciaAsyncArrowFunctionValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncArrowFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PushActiveScope(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.PopActiveScope;
    end;
  finally
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

function TGocciaAsyncArrowFunctionValue.CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);
  Result := TGocciaAsyncArrowFunctionValue.Create(FParameters, ClonedStatements, ANewScope, FName);
  TGocciaFunctionValue(Result).IsExpressionBody := FIsExpressionBody;
end;

{ TGocciaAsyncMethodValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncMethodValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PushActiveScope(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.PopActiveScope;
    end;
  finally
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

end.
