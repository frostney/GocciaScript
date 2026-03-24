unit Goccia.MicrotaskQueue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Souffle.Value,

  Goccia.Runtime.Collections,
  Goccia.Values.Primitives;

type
  TPromiseReactionType = (prtFulfill, prtReject, prtThenableResolve);

  TGocciaMicrotask = record
    Handler: TGocciaValue;
    ResultPromise: TGocciaValue;
    Value: TGocciaValue;
    ReactionType: TPromiseReactionType;
  end;

  TSouffleMicrotask = record
    Handler: TSouffleValue;
    Value: TSouffleValue;
    ResultPromise: TSoufflePromise;
    ReactionType: TPromiseReactionType;
  end;

  TSouffleMicrotaskInvoker = procedure(const AHandler, AValue: TSouffleValue;
    out AResult: TSouffleValue; out AHadError: Boolean) of object;

  TGocciaMicrotaskQueue = class
  private class var
    FInstance: TGocciaMicrotaskQueue;
  private
    FQueue: TList<TGocciaMicrotask>;
    FSouffleQueue: TList<TSouffleMicrotask>;
    FTaskOrder: TList<Byte>; { 0 = Goccia, 1 = Souffle }
    FSouffleInvoker: TSouffleMicrotaskInvoker;
  public
    class function Instance: TGocciaMicrotaskQueue;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(const AMicrotask: TGocciaMicrotask);
    procedure EnqueueSouffle(const AMicrotask: TSouffleMicrotask);
    procedure DrainQueue;
    procedure ClearQueue;
    function HasPending: Boolean;

    property SouffleInvoker: TSouffleMicrotaskInvoker read FSouffleInvoker write FSouffleInvoker;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Arguments.Collection,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.PromiseValue;

class function TGocciaMicrotaskQueue.Instance: TGocciaMicrotaskQueue;
begin
  Result := FInstance;
end;

class procedure TGocciaMicrotaskQueue.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaMicrotaskQueue.Create;
end;

class procedure TGocciaMicrotaskQueue.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaMicrotaskQueue.Create;
begin
  FQueue := TList<TGocciaMicrotask>.Create;
  FSouffleQueue := TList<TSouffleMicrotask>.Create;
  FTaskOrder := TList<Byte>.Create;
  FSouffleInvoker := nil;
end;

destructor TGocciaMicrotaskQueue.Destroy;
begin
  FTaskOrder.Free;
  FSouffleQueue.Free;
  FQueue.Free;
  inherited;
end;

procedure TGocciaMicrotaskQueue.Enqueue(const AMicrotask: TGocciaMicrotask);
begin
  FQueue.Add(AMicrotask);
  FTaskOrder.Add(0);
end;

procedure TGocciaMicrotaskQueue.EnqueueSouffle(const AMicrotask: TSouffleMicrotask);
begin
  FSouffleQueue.Add(AMicrotask);
  FTaskOrder.Add(1);
end;

procedure TGocciaMicrotaskQueue.DrainQueue;
var
  I: Integer;
  Task: TGocciaMicrotask;
  Promise: TGocciaPromiseValue;
  HandlerResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  STask: TSouffleMicrotask;
  SResult: TSouffleValue;
  SHadError: Boolean;
  SI: Integer;
var
  GIdx, SIdx, OIdx: Integer;
begin
  GIdx := 0;
  SIdx := 0;
  OIdx := 0;
  while OIdx < FTaskOrder.Count do
  begin
    if FTaskOrder[OIdx] = 0 then
    begin
      { Goccia microtask }
      Task := FQueue[GIdx];
      Inc(GIdx);
      Promise := TGocciaPromiseValue(Task.ResultPromise);
      if Assigned(TGarbageCollector.Instance) then
      begin
        if Assigned(Task.Handler) then
          TGarbageCollector.Instance.AddTempRoot(Task.Handler);
        if Assigned(Task.Value) then
          TGarbageCollector.Instance.AddTempRoot(Task.Value);
        if Assigned(Promise) then
          TGarbageCollector.Instance.AddTempRoot(Promise);
      end;
      try
        if Assigned(Task.Handler) and Task.Handler.IsCallable then
        begin
          CallArgs := TGocciaArgumentsCollection.Create([Task.Value]);
          try
            try
              HandlerResult := TGocciaFunctionBase(Task.Handler).Call(
                CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
              if Assigned(Promise) then
                Promise.Resolve(HandlerResult);
            except
              on E: TGocciaThrowValue do
                if Assigned(Promise) then
                  Promise.Reject(E.Value);
            end;
          finally
            CallArgs.Free;
          end;
        end
        else if Assigned(Promise) then
        begin
          case Task.ReactionType of
            prtFulfill: Promise.Resolve(Task.Value);
            prtReject: Promise.Reject(Task.Value);
            prtThenableResolve:
              if Task.Value is TGocciaPromiseValue then
                Promise.SubscribeTo(TGocciaPromiseValue(Task.Value));
          end;
        end;
      finally
        if Assigned(TGarbageCollector.Instance) then
        begin
          if Assigned(Task.Handler) then
            TGarbageCollector.Instance.RemoveTempRoot(Task.Handler);
          if Assigned(Task.Value) then
            TGarbageCollector.Instance.RemoveTempRoot(Task.Value);
          if Assigned(Promise) then
            TGarbageCollector.Instance.RemoveTempRoot(Promise);
        end;
      end;
    end
    else
    begin
      { Souffle microtask }
      if Assigned(FSouffleInvoker) then
      begin
        STask := FSouffleQueue[SIdx];
        Inc(SIdx);
        if SouffleIsReference(STask.Handler) and Assigned(STask.Handler.AsReference) then
        begin
          FSouffleInvoker(STask.Handler, STask.Value, SResult, SHadError);
          if Assigned(STask.ResultPromise) then
          begin
            if SHadError then
              STask.ResultPromise.Reject(SResult)
            else
              STask.ResultPromise.Resolve(SResult);
          end;
        end
        else if Assigned(STask.ResultPromise) then
        begin
          case STask.ReactionType of
            prtFulfill: STask.ResultPromise.Resolve(STask.Value);
            prtReject: STask.ResultPromise.Reject(STask.Value);
            prtThenableResolve:
              if SouffleIsReference(STask.Value) and Assigned(STask.Value.AsReference) and
                 (STask.Value.AsReference is TSoufflePromise) then
                STask.ResultPromise.SubscribeTo(TSoufflePromise(STask.Value.AsReference));
          end;
        end;
      end
      else
        Inc(SIdx);
    end;
    Inc(OIdx);
  end;

  if GIdx > 0 then FQueue.Clear;
  if SIdx > 0 then FSouffleQueue.Clear;
  if OIdx > 0 then FTaskOrder.Clear;

end;

procedure TGocciaMicrotaskQueue.ClearQueue;
var
  I: Integer;
  Task: TGocciaMicrotask;
begin
  if Assigned(TGarbageCollector.Instance) then
    for I := 0 to FQueue.Count - 1 do
    begin
      Task := FQueue[I];
      if Assigned(Task.Handler) then
        TGarbageCollector.Instance.RemoveTempRoot(Task.Handler);
    end;
  FQueue.Clear;
  FSouffleQueue.Clear;
  FTaskOrder.Clear;
end;

function TGocciaMicrotaskQueue.HasPending: Boolean;
begin
  Result := FTaskOrder.Count > 0;
end;

end.
