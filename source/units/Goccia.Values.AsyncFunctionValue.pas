unit Goccia.Values.AsyncFunctionValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.Primitives;

type
  TGocciaAsyncFunctionValue = class(TGocciaFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncArrowFunctionValue = class(TGocciaArrowFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
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
  GC: TGarbageCollector;
  CollectionCountBefore: Integer;
begin
  Promise := TGocciaPromiseValue.Create;

  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;
    CollectionCountBefore := -1;

    if Assigned(GC) then
    begin
      CollectionCountBefore := GC.TotalCollections;
      GC.PushActiveRoot(Self);
      GC.PushActiveRoot(CallScope);
    end;
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(GC) then
      begin
        GC.PopActiveRoot;
        GC.PopActiveRoot;
      end;
      if ((not Assigned(GC)) or (GC.TotalCollections = CollectionCountBefore)) and
         (not CallScope.Escaped) then
        CallScope.Free;
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

{ TGocciaAsyncArrowFunctionValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncArrowFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
  GC: TGarbageCollector;
  CollectionCountBefore: Integer;
begin
  Promise := TGocciaPromiseValue.Create;

  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;
    CollectionCountBefore := -1;

    if Assigned(GC) then
    begin
      CollectionCountBefore := GC.TotalCollections;
      GC.PushActiveRoot(Self);
      GC.PushActiveRoot(CallScope);
    end;
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(GC) then
      begin
        GC.PopActiveRoot;
        GC.PopActiveRoot;
      end;
      if ((not Assigned(GC)) or (GC.TotalCollections = CollectionCountBefore)) and
         (not CallScope.Escaped) then
        CallScope.Free;
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

{ TGocciaAsyncMethodValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncMethodValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
  GC: TGarbageCollector;
  CollectionCountBefore: Integer;
begin
  Promise := TGocciaPromiseValue.Create;

  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;
    CollectionCountBefore := -1;

    if Assigned(GC) then
    begin
      CollectionCountBefore := GC.TotalCollections;
      GC.PushActiveRoot(Self);
      GC.PushActiveRoot(CallScope);
    end;
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      if Assigned(GC) then
      begin
        GC.PopActiveRoot;
        GC.PopActiveRoot;
      end;
      if ((not Assigned(GC)) or (GC.TotalCollections = CollectionCountBefore)) and
         (not CallScope.Escaped) then
        CallScope.Free;
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

end.
